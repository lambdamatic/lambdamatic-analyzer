/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.lambdamatic.analyzer.LambdaExpressionAnalyzer;
import org.lambdamatic.analyzer.ast.node.ArrayVariable;
import org.lambdamatic.analyzer.ast.node.Assignment;
import org.lambdamatic.analyzer.ast.node.BooleanLiteral;
import org.lambdamatic.analyzer.ast.node.CapturedArgument;
import org.lambdamatic.analyzer.ast.node.CapturedArgumentRef;
import org.lambdamatic.analyzer.ast.node.ClassLiteral;
import org.lambdamatic.analyzer.ast.node.CompoundExpression;
import org.lambdamatic.analyzer.ast.node.CompoundExpression.CompoundExpressionOperator;
import org.lambdamatic.analyzer.ast.node.ControlFlowStatement;
import org.lambdamatic.analyzer.ast.node.Expression;
import org.lambdamatic.analyzer.ast.node.Expression.ExpressionType;
import org.lambdamatic.analyzer.ast.node.ExpressionFactory;
import org.lambdamatic.analyzer.ast.node.ExpressionStatement;
import org.lambdamatic.analyzer.ast.node.FieldAccess;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.LocalVariable;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.MethodReference;
import org.lambdamatic.analyzer.ast.node.Node;
import org.lambdamatic.analyzer.ast.node.NullLiteral;
import org.lambdamatic.analyzer.ast.node.NumberLiteral;
import org.lambdamatic.analyzer.ast.node.ObjectInstanciation;
import org.lambdamatic.analyzer.ast.node.Operation;
import org.lambdamatic.analyzer.ast.node.Operation.Operator;
import org.lambdamatic.analyzer.ast.node.ReturnStatement;
import org.lambdamatic.analyzer.ast.node.Statement;
import org.lambdamatic.analyzer.exception.AnalyzeException;
import org.lambdamatic.analyzer.utils.ClassUtils;
import org.lambdamatic.analyzer.utils.Pair;
import org.lambdamatic.analyzer.utils.ReflectionUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.InvokeDynamicInsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.LocalVariableNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.VarInsnNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An internal utility class that uses ASM to read the bytecode of a (desugared lambda expression)
 * method and converts it into an {@link Node}.
 *
 * @author Xavier Coulon
 *
 */
public class LambdaExpressionReader {

  /**
   * A {@link SignatureVisitor} that retrieves the class type.
   * 
   * @author Xavier Coulon
   *
   */
  private static final class ClassTypeRetriever extends SignatureVisitor {

    /** the type (once resolved). */
    private Class<?> type = null;

    /**
     * Constructor.
     */
    ClassTypeRetriever() {
      super(Opcodes.ASM5);
    }

    public Class<?> getType() {
      return this.type;
    }

    @Override
    public void visitClassType(final String className) {
      try {
        this.type = Class.forName(className.replace("/", "."));
      } catch (final ClassNotFoundException e) {
        throw new AnalyzeException("Failed to retrieve class with name " + className, e);
      }
    }

    @Override
    public void visitBaseType(final char descriptor) {
      switch (Type.getType(Character.toString(descriptor)).getSort()) {
        case Type.VOID:
          this.type = void.class;
          break;
        case Type.BYTE:
          this.type = byte.class;
          break;
        case Type.BOOLEAN:
          this.type = boolean.class;
          break;
        case Type.SHORT:
          this.type = short.class;
          break;
        case Type.INT:
          this.type = int.class;
          break;
        case Type.DOUBLE:
          this.type = double.class;
          break;
        case Type.FLOAT:
          this.type = float.class;
          break;
        case Type.LONG:
          this.type = long.class;
          break;
        default:
          throw new AnalyzeException(
              "Failed to retrieve primitive class with descriptor '" + descriptor + "'");
      }
    }

  }

  /** The usual Logger. */
  static final Logger LOGGER = LoggerFactory.getLogger(LambdaExpressionReader.class);

  /**
   * Reads the given {@link List} of (bytecode) {@link AbstractInsnNode} located at the known
   * {@link SerializedLambdaInfo} and computes a simplified list of {@link Statement} based tree
   * representing the initial lambda expression.
   * 
   * @param lambdaInfo the info about the Lambda expression synthetic implementation
   * @return the {@link List} of {@link Statement} found while reading the bytecode along with the
   *         arguments passed to the Lambda Expression
   * @throws IOException if a problem occurred while reading the underlying {@link Class}
   */
  public Pair<List<Statement>, List<LocalVariable>> readBytecodeStatements(
      final SerializedLambdaInfo lambdaInfo) throws IOException {
    if (!lambdaInfo.getImplMethod().isSynthetic()) {
      return readMethodReference(lambdaInfo);
    }
    return readLambdaExpression(lambdaInfo);
  }

  /**
   * Analyzes the synthetic method that was generated by the compiler after the user-defined lambda
   * expression.
   * 
   * @param lambdaInfo the info about the Lambda expression synthetic implementation
   * @return the {@link List} of {@link Statement} found while reading the bytecode along with the
   *         arguments passed to the Lambda Expression
   * @throws IOException if a problem occurred while reading the underlying {@link Class}
   */
  private Pair<List<Statement>, List<LocalVariable>> readLambdaExpression(
      final SerializedLambdaInfo lambdaInfo) throws IOException {
    final LambdaExpressionClassVisitor desugaredExpressionVisitor =
        new LambdaExpressionClassVisitor(lambdaInfo);
    try (final InputStream serializedLambdaStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(lambdaInfo.getImplClass().getName().replace('.', '/') + ".class")) {
      final ClassReader classReader = new ClassReader(serializedLambdaStream);
      classReader.accept(desugaredExpressionVisitor, 0);
      final InsnList instructions = desugaredExpressionVisitor.getInstructions();
      final LocalVariables localVariables =
          new LocalVariables(desugaredExpressionVisitor.getLocalVariables());
      final Map<String, AbstractInsnNode> labels = desugaredExpressionVisitor.getLabels();
      final InsnCursor insnCursor = new InsnCursor(instructions, labels);
      // we must set the cursor on the first instruction before calling the readStatementSubTree()
      // method
      insnCursor.next();
      final List<Statement> statements = readStatements(insnCursor, new LinkedList<>(),
          lambdaInfo.getCapturedArguments(), localVariables);
      // now, let's identify the lambda expression arguments (_excluding_ the captured arguments)
      final List<LocalVariable> lambdaExpressionArguments = localVariables.toLocalVariables();
      return new Pair<>(statements, lambdaExpressionArguments);
    }
  }


  /**
   * For method reference, the lambda expression simply calls the Java method without having to pass
   * by a synthetic method.
   * 
   * kinds of method reference:
   * <ul>
   * <li>Reference to a static method. Eg: <code>ContainingClass::staticMethodName</code></li>
   * <li>Reference to an instance method of a particular object. Eg:
   * <code>containingObject::instanceMethodName</code></li>
   * <li>Reference to an instance method of an arbitrary object of a particular type. Eg:
   * <code>ContainingType::methodName</code></li>
   * <li>Reference to an instance method of an arbitrary object of a particular type. Eg:
   * <code>ContainingType::methodName</code></li>
   * </ul>
   * 
   * @param lambdaInfo the info about the Lambda expression synthetic implementation
   * @return the {@link List} of {@link Statement} found while reading the bytecode along with the
   *         arguments passed to the Lambda Expression
   */
  private static Pair<List<Statement>, List<LocalVariable>> readMethodReference(
      final SerializedLambdaInfo lambdaInfo) {
    LOGGER.trace("Lambda expression is not implemented in a synthetic method: {}.{}",
        lambdaInfo.getImplClass().getName(), lambdaInfo.getImplMethod().getName());
    final Type returnType = Type.getReturnType(lambdaInfo.getInstantiatedMethodType());
    final boolean isConstructorMethod = lambdaInfo.getImplMethod() instanceof Constructor;
    final boolean isReturnStatement = isConstructorMethod
        || (returnType != null && !returnType.getClassName().equals(void.class.getName()));
    final MethodReference methodReference =
        new MethodReference(lambdaInfo.getImplClass(), lambdaInfo.getImplMethod(), lambdaInfo.getCapturedArguments());
    final Statement lambdaExpressionStatement = isReturnStatement
        ? new ReturnStatement(methodReference) : new ExpressionStatement(methodReference);
    return new Pair<>(Arrays.asList(lambdaExpressionStatement), Collections.emptyList());
  }

  /**
   * Reads the bytecode from the given {@link InsnCursor}'s <strong>current position</strong>, until
   * there is no further instruction to proceed. It is the responsability of the caller to set the
   * cursor position.
   * 
   * @param insnCursor the instruction cursor used to read the bytecode
   * @param operandStack the expression stack to put on or pop from
   * @param localVariables the local variables
   * @return a {@link List} of {@link Statement} containing the {@link Statement}
   */
  private List<Statement> readStatements(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final List<CapturedArgument> capturedArguments,
      final LocalVariables localVariables) {
    final List<Statement> statements = new ArrayList<>();
    while (insnCursor.hasCurrent()) {
      final AbstractInsnNode currentInstruction = insnCursor.getCurrent();
      switch (currentInstruction.getType()) {
        case AbstractInsnNode.VAR_INSN:
          readVariableInstructionNode(operandStack, capturedArguments, localVariables,
              (VarInsnNode) currentInstruction);
          break;
        case AbstractInsnNode.LDC_INSN:
          final LdcInsnNode ldcInsnNode = (LdcInsnNode) currentInstruction;
          readLdcInstructionNode(operandStack, ldcInsnNode);
          break;
        case AbstractInsnNode.FIELD_INSN:
          final FieldInsnNode fieldInsnNode = (FieldInsnNode) currentInstruction;
          readFieldInstructionNode(operandStack, statements, fieldInsnNode);
          break;
        case AbstractInsnNode.METHOD_INSN:
          final MethodInsnNode methodInsnNode = (MethodInsnNode) currentInstruction;
          final Statement methodInvocationStatement =
              readMethodInstructionNode(insnCursor, operandStack, methodInsnNode);
          if (methodInvocationStatement != null) {
            statements.add(methodInvocationStatement);
          }
          break;
        case AbstractInsnNode.INVOKE_DYNAMIC_INSN:
          final InvokeDynamicInsnNode invokeDynamicInsnNode =
              (InvokeDynamicInsnNode) currentInstruction;
          final Statement methodInkeDynamicStatement = readInvokeDynamicInstructionNode(
              operandStack, capturedArguments, invokeDynamicInsnNode);
          if (methodInkeDynamicStatement != null) {
            statements.add(methodInkeDynamicStatement);
          }
          break;
        case AbstractInsnNode.JUMP_INSN:
          statements.addAll(
              readJumpInstruction(insnCursor, operandStack, capturedArguments, localVariables));
          return statements;
        case AbstractInsnNode.INT_INSN:
          readIntInstruction((IntInsnNode) currentInstruction, operandStack, localVariables);
          break;
        case AbstractInsnNode.INSN:
          final List<Statement> instructionStatement =
              readInstruction(insnCursor, operandStack, capturedArguments, localVariables);
          statements.addAll(instructionStatement);
          break;
        case AbstractInsnNode.TYPE_INSN:
          readTypeInstruction((TypeInsnNode) currentInstruction, operandStack);
          break;
        default:
          throw new AnalyzeException(
              "This is embarrassing... We've reached an unexpected instruction operator: "
                  + currentInstruction.getType());
      }
      insnCursor.next();
    }
    return statements;
  }

  /**
   * Reads the given {@link MethodInsnNode}.
   * 
   * @param insnCursor the instruction cursor used to read the bytecode
   * @param operandStack the pile of {@link Expression}
   * @param methodInsnNode the {@link MethodInsnNode} to read
   * @return an {@link ExpressionStatement} if the inked method returned <code>null</code>,
   *         <code>null</code> otherwise (the invoked method is put on top of the given
   *         {@code operandStack}).
   */
  private static Statement readMethodInstructionNode(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final MethodInsnNode methodInsnNode) {
    final Type[] argumentTypes = Type.getArgumentTypes(methodInsnNode.desc);
    final List<Expression> args = new ArrayList<>();
    final List<Class<?>> parameterTypes = new ArrayList<>();
    Stream.of(argumentTypes).forEach(argumentType -> {
      final Expression arg = operandStack.pollLast();
      final String argumentClassName = argumentType.getClassName();
      args.add(castOperand(arg, argumentClassName));
      parameterTypes.add(ClassUtils.getClass(argumentClassName));
    });
    // arguments appear in reverse order in the bytecode
    Collections.reverse(args);
    switch (methodInsnNode.getOpcode()) {
      case Opcodes.INVOKEINTERFACE:
      case Opcodes.INVOKEVIRTUAL:
      case Opcodes.INVOKESPECIAL:
        return readInvokeMethod(insnCursor, operandStack, methodInsnNode, args, parameterTypes);
      case Opcodes.INVOKESTATIC:
        return readInvokeStaticMethod(insnCursor, operandStack, methodInsnNode, args,
            parameterTypes);
      default:
        throw new AnalyzeException(
            "Unexpected method invocation type: " + methodInsnNode.getOpcode());
    }
  }

  /**
   * Reads the given {@link MethodInsnNode}.
   * 
   * @param insnCursor the instruction cursor.
   * @param operandStack the pile of {@link Expression}
   * @param methodInsnNode the {@link MethodInsnNode} to read
   * @param args the arguments passed to the method
   * @param parameterTypes the types of parameters of the called method
   * @return an {@link ExpressionStatement} if the inked method returned <code>null</code>,
   *         <code>null</code> otherwise (the invoked method is put on top of the given
   *         {@code operandStack}).
   */
  private static Statement readInvokeStaticMethod(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final MethodInsnNode methodInsnNode,
      final List<Expression> args, final List<Class<?>> parameterTypes) {
    final Type type = Type.getObjectType(methodInsnNode.owner);
    try {
      final Class<?> sourceClass = Class.forName(type.getClassName());
      final Method javaMethod =
          ReflectionUtils.getMethod(sourceClass, methodInsnNode.name, parameterTypes);
      final Class<?> returnType = findReturnType(insnCursor, javaMethod);
      final MethodInvocation invokedStaticMethod =
          new MethodInvocation(new ClassLiteral(sourceClass), javaMethod, returnType, args);
      if (returnType.equals(void.class) || returnType.equals(Void.class)) {
        return new ExpressionStatement(invokedStaticMethod);
      }
      operandStack.add(invokedStaticMethod);
      return null;
    } catch (ClassNotFoundException e) {
      throw new AnalyzeException("Failed to retrieve class for " + methodInsnNode.owner, e);
    }
  }

  /**
   * Reads the {@code invoke special} {@link MethodInsnNode}.
   * 
   * @param insnCursor the instruction cursor used to read the bytecode.
   * @param operandStack the pile of {@link Expression}
   * @param methodInsnNode the {@link MethodInsnNode} to read
   * @param args the arguments passed to the method
   * @param parameterTypes the types of parameters of the called method
   * @return an {@link ExpressionStatement} if the inked method returned <code>null</code>,
   *         <code>null</code> otherwise (the invoked method is put on top of the given
   *         {@code operandStack}).
   */
  private static Statement readInvokeMethod(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final MethodInsnNode methodInsnNode,
      final List<Expression> args, final List<Class<?>> parameterTypes) {
    // object instantiation
    if ("<init>".equals(methodInsnNode.name)) {
      final ObjectInstanciation objectVariable = (ObjectInstanciation) operandStack.pollLast();
      objectVariable.setInitArguments(args);
    } else {
      final Expression sourceExpression = operandStack.pollLast();
      final Method javaMethod = ReflectionUtils.getMethod(sourceExpression.getJavaType(),
          methodInsnNode.name, parameterTypes);
      final Class<?> returnType = findReturnType(insnCursor, javaMethod);
      final MethodInvocation invokedMethod =
          new MethodInvocation(sourceExpression, javaMethod, returnType, args);
      // if the method returns 'void', we can convert the method invocation into a statement
      if (returnType.equals(void.class) || returnType.equals(Void.class)) {
        return new ExpressionStatement(invokedMethod);
      }
      // otherwise, the method invocation 'result' returns to the operand stack
      operandStack.add(invokedMethod);
    }
    return null;
  }

  /**
   * Reads the {@link InvokeDynamicInsnNode}.
   * 
   * @param operandStack the pile of {@link Expression}
   * @param capturedArguments the captured arguments passed to the method
   * @param invokeDynamicInsnNode the {@link InvokeDynamicInsnNode} to read
   */
  private static Statement readInvokeDynamicInstructionNode(
      final LinkedList<Expression> operandStack, final List<CapturedArgument> capturedArguments,
      final InvokeDynamicInsnNode invokeDynamicInsnNode) {
    final Handle handle = (Handle) invokeDynamicInsnNode.bsmArgs[1];
    final int argNumber = Type.getArgumentTypes(invokeDynamicInsnNode.desc).length;
    final List<CapturedArgumentRef> lambdaArgs = new ArrayList<>();
    for (int i = 0; i < argNumber; i++) {
      final Expression expr = operandStack.pollLast();
      if (expr.getType() != ExpressionType.CAPTURED_ARGUMENT_REF) {
        throw new AnalyzeException("Unexpected argument type when following InvokeDynamic call: "
            + expr.getType());
      }
      lambdaArgs.add((CapturedArgumentRef) expr); // , expr.getValue()
    }
    Collections.reverse(lambdaArgs);
    final EmbeddedSerializedLambdaInfo lambdaInfo =
        // should be invokedDynamic but can't find it in MethodHandleInfo ?
        new EmbeddedSerializedLambdaInfo(handle.getOwner(), handle.getName(), handle.getDesc(), 0,
            lambdaArgs, capturedArguments);
    final LambdaExpression lambdaExpression =
        LambdaExpressionAnalyzer.getInstance().analyzeExpression(lambdaInfo);
    operandStack.add(lambdaExpression);
    return null;
  }

  /**
   * Reads the given {@link LdcInsnNode}.
   * 
   * @param operandStack the pile of {@link Expression}
   * @param ldcInsnNode the instruction to read.
   */
  private static void readLdcInstructionNode(final LinkedList<Expression> operandStack,
      final LdcInsnNode ldcInsnNode) {
    // let's move this instruction on top of the stack until it
    // is used as an argument during a method call
    final Expression constant = ExpressionFactory.getExpression(ldcInsnNode.cst);
    LOGGER.trace("Adding constant {}", constant);
    operandStack.add(constant);
  }

  /**
   * Reads the {@link FieldInsnNode}.
   * 
   * @param operandStack the pile of {@link Expression}
   * @param statementPile the pile of {@link Statement}
   * @param fieldInsnNode the {@link FieldInsnNode} to read
   */
  private static void readFieldInstructionNode(final LinkedList<Expression> operandStack,
      final List<Statement> statementPile, final FieldInsnNode fieldInsnNode) {
    final Class<?> fieldType = getJavaType(Type.getType(fieldInsnNode.desc));
    switch (fieldInsnNode.getOpcode()) {
      case Opcodes.GETSTATIC:
        final Class<?> ownerType = getJavaType(Type.getObjectType(fieldInsnNode.owner));
        final FieldAccess staticFieldAccess =
            new FieldAccess(new ClassLiteral(ownerType), fieldInsnNode.name, fieldType);
        operandStack.add(staticFieldAccess);
        break;
      case Opcodes.GETFIELD:
        final Expression fieldAccessParent = operandStack.pollLast();
        final FieldAccess fieldAccess =
            new FieldAccess(fieldAccessParent, fieldInsnNode.name, fieldType);
        operandStack.add(fieldAccess);
        break;
      case Opcodes.PUTFIELD:
        final Expression fieldAssignationValue = operandStack.pollLast();
        final Expression parentSource = operandStack.pollLast();
        final FieldAccess source = new FieldAccess(parentSource, fieldInsnNode.name, fieldType);
        final Assignment assignmentExpression = new Assignment(source, fieldAssignationValue);
        statementPile.add(new ExpressionStatement(assignmentExpression));
        break;
      default:
        throw new AnalyzeException(
            "Unexpected field instruction type: " + fieldInsnNode.getOpcode());

    }
  }

  /**
   * Reads the variable instruction.
   * 
   * @param operandStack the expression stack to put on or pop from
   * @param capturedArguments the captured argument references, if any
   * @param localVariables the local variables
   * @param varInstruction the 'variable instruction' to process
   */
  private static void readVariableInstructionNode(final LinkedList<Expression> operandStack,
      final List<CapturedArgument> capturedArguments, final LocalVariables localVariables,
      final VarInsnNode varInstruction) {
    switch (varInstruction.getOpcode()) {
      // load a reference onto the stack from a local variable
      case Opcodes.ALOAD:
      case Opcodes.ILOAD:
        // load an int value from a local variable
        // Note: The 'var' operand is the index of a local variable
        // all captured arguments come before the local variable in the method signature,
        // which means that the local variables table is empty on the first slots which are
        // "allocated"
        // for the captured arguments.
        if (varInstruction.var < capturedArguments.size()) {
          // if the variable index matches a captured argument
          // note: not using actual captured argument but rather, use a _reference_ to it.
          final Object capturedArgumentValue = capturedArguments.get(varInstruction.var).getValue();
          final Class<?> capturedArgumentValueType =
              capturedArgumentValue != null ? capturedArgumentValue.getClass() : Object.class;
          final CapturedArgumentRef capturedArgumentRef =
              new CapturedArgumentRef(varInstruction.var, capturedArgumentValueType);
          operandStack.add(capturedArgumentRef);
        } else {
          // the variable index matches a local variable
          final LocalVariableNode var = localVariables.load(varInstruction.var);
          operandStack.add(new LocalVariable(var.index, var.name, readSignature(var.desc)));
        }
        break;
      case Opcodes.ASTORE:
        // store a reference into a local variable
        localVariables.store(varInstruction.var);
        break;
      default:
        throw new AnalyzeException(
            "Unexpected Variable instruction code: " + varInstruction.getOpcode());
    }
  }

  /**
   * Checks if an operation of type OpsCode.CHECKCAST is following this operation, in which case it
   * provides info about the actual returned type of the method
   * 
   * @param insnCursor the {@link InsnCursor}
   * @param javaMethod the current Java {@link Method}
   * @return the return type of the given method.
   */
  private static Class<?> findReturnType(final InsnCursor insnCursor, final Method javaMethod) {
    if (insnCursor.hasNext()) {
      final AbstractInsnNode nextOp = insnCursor.getNext();
      if (nextOp.getOpcode() == Opcodes.CHECKCAST) {
        final TypeInsnNode checkCastInsnNode = (TypeInsnNode) nextOp;
        try {
          return Class.forName(Type.getObjectType(checkCastInsnNode.desc).getClassName());
        } catch (ClassNotFoundException e) {
          throw new AnalyzeException("Failed to retrieve class for " + checkCastInsnNode.desc, e);
        }
      }
      // move cursor position backwards
      insnCursor.getPrevious();
    }
    return javaMethod.getReturnType();
  }

  /**
   * @param typeName the fully qualified name of the type (primitive or Objecttype )
   * @return the returned Java type.
   * @throws AnalyzeException if the Class could not be found
   */
  private static Class<?> getArrayType(final Type type) {
    try {
      // TODO: can we avoid the array instantiation ?
      final Type elementType = Type.getType(type.getDescriptor());
      return Array.newInstance(Class.forName(elementType.getClassName()), 0).getClass();
    } catch (ClassNotFoundException e) {
      throw new AnalyzeException("Failed to retrieve type named " + type.getClassName(), e);
    }
  }

  /**
   * @param typeName the fully qualified name of the type (primitive or Object type)
   * @return the Java type corresponding to the given type.
   * @throws AnalyzeException if the Class could not be found
   */
  private static Class<?> getJavaType(final Type type) {
    try {
      if (type.getSort() == Type.ARRAY) {
        // TODO: can we avoid the array instantiation ?
        final Type elementType = type.getElementType();
        return Array.newInstance(Class.forName(elementType.getClassName()), 0).getClass();
      }
      switch (type.getClassName()) {
        case "boolean":
          return boolean.class;
        case "byte":
          return byte.class;
        case "short":
          return short.class;
        case "int":
          return int.class;
        case "long":
          return long.class;
        case "float":
          return float.class;
        case "double":
          return double.class;
        case "char":
          return char.class;
        case "void":
          return void.class;
        default:
          return Class.forName(type.getClassName());
      }
    } catch (ClassNotFoundException e) {
      throw new AnalyzeException("Failed to retrieve type named " + type.getClassName(), e);
    }
  }

  /**
   * Reads the given type signature and returns a proper {@link Class}.
   * 
   * @param desc the type signature to read
   * @return the associated {@link Class}
   * 
   */
  static Class<?> readSignature(final String desc) {
    final SignatureReader signatureReader = new SignatureReader(desc);
    final ClassTypeRetriever classTypeRetriever = new ClassTypeRetriever();
    signatureReader.accept(classTypeRetriever);
    return classTypeRetriever.getType();
  }

  /**
   * Reads the current {@link InsnNode} instruction and returns a {@link Statement} or {@code null}
   * if the instruction is not a full statement (in that case, the instruction is stored in the
   * given Expression {@link LinkedList}).
   * 
   * @param insnNode the instruction to read
   * @param operandStack the expression stack to put on or pop from
   * @param localVariables the local variables
   * @return a {@link List} of {@link Statement} or empty list if no {@link Statement} was created
   *         after reading the current instruction
   * @see <a href="https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings">Java bytcode
   *      instruction listings on Wikipedia</a>
   */
  private List<Statement> readInstruction(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final List<CapturedArgument> capturedArguments,
      final LocalVariables localVariables) {
    final List<Statement> statements = new ArrayList<>();
    final AbstractInsnNode insnNode = insnCursor.getCurrent();
    switch (insnNode.getOpcode()) {
      // return a reference from a method
      case Opcodes.ARETURN:
        // return an integer from a method
      case Opcodes.IRETURN:
        statements.add(new ReturnStatement(operandStack.pollLast()));
        break;
      // return void from method
      case Opcodes.RETURN:
        // wrap all pending expressions into ExpressionStatements
        while (!operandStack.isEmpty()) {
          final Expression pendingExpression = operandStack.pollLast();
          statements.add(new ExpressionStatement(pendingExpression));
        }
        break;
      // push a null reference onto the stack
      case Opcodes.ACONST_NULL:
        operandStack.add(new NullLiteral());
        break;
      // load the int value 0 onto the stack
      case Opcodes.ICONST_0:
        // applies for byte, short, int and boolean
        operandStack.add(new NumberLiteral(0));
        break;
      // load the int value 1 onto the stack
      case Opcodes.ICONST_1:
        // applies for byte, short, int and boolean
        operandStack.add(new NumberLiteral(1));
        break;
      // load the int value 2 onto the stack
      case Opcodes.ICONST_2:
        operandStack.add(new NumberLiteral(2));
        break;
      // load the int value 3 onto the stack
      case Opcodes.ICONST_3:
        operandStack.add(new NumberLiteral(3));
        break;
      // load the int value 4 onto the stack
      case Opcodes.ICONST_4:
        operandStack.add(new NumberLiteral(4));
        break;
      // load the int value 5 onto the stack
      case Opcodes.ICONST_5:
        operandStack.add(new NumberLiteral(5));
        break;
      // push the long 0 onto the stack
      case Opcodes.LCONST_0:
        operandStack.add(new NumberLiteral(0L));
        break;
      // push the long 1 onto the stack
      case Opcodes.LCONST_1:
        operandStack.add(new NumberLiteral(1L));
        break;
      // push the 0.0f onto the stack
      case Opcodes.FCONST_0:
        operandStack.add(new NumberLiteral(0f));
        break;
      // push the 1.0f onto the stack
      case Opcodes.FCONST_1:
        operandStack.add(new NumberLiteral(1f));
        break;
      // push the 2.0f onto the stack
      case Opcodes.FCONST_2:
        operandStack.add(new NumberLiteral(2f));
        break;
      // push the constant 0.0 onto the stack
      case Opcodes.DCONST_0:
        operandStack.add(new NumberLiteral(0d));
        break;
      // push the constant 1.0 onto the stack
      case Opcodes.DCONST_1:
        operandStack.add(new NumberLiteral(1d));
        break;
      // compare two longs values
      case Opcodes.LCMP:
        // compare two doubles
      case Opcodes.DCMPL:
        // compare two doubles
      case Opcodes.DCMPG:
        // compare two floats
      case Opcodes.FCMPL:
        // compare two floats
      case Opcodes.FCMPG:
        statements.addAll(readJumpInstruction(insnCursor.next(), operandStack, capturedArguments,
            localVariables));
        break;
      // add 2 ints
      case Opcodes.IADD:
        operandStack.add(readOperation(Operator.ADD, operandStack));
        break;
      // int subtract
      case Opcodes.ISUB:
        operandStack.add(readOperation(Operator.SUBTRACT, operandStack));
        break;
      // multiply 2 integers
      case Opcodes.IMUL:
        operandStack.add(readOperation(Operator.MULTIPLY, operandStack));
        break;
      // divide 2 integers
      case Opcodes.IDIV:
        operandStack.add(readOperation(Operator.DIVIDE, operandStack));
        break;
      // negate int
      case Opcodes.INEG:
        operandStack.add(inverseInteger(operandStack));
        break;
      // discard the top value on the stack
      case Opcodes.POP:
      case Opcodes.POP2:
        // statements.add(new ExpressionStatement(operandStack.pollLast()));
        LOGGER.trace("Ignoring POP/POP2 instruction");
        break;
      // duplicate the value on top of the stack
      case Opcodes.DUP:
        operandStack.add(operandStack.peekLast());
        break;
      // insert a copy of the top value into the stack two values from the top.
      case Opcodes.DUP_X1:
        operandStack.add(operandStack.size() - 2, operandStack.peekLast());
        break;
      // store into a reference in an array
      case Opcodes.AASTORE:
        readArrayStoreInstruction(operandStack);
        break;
      // converts Float to Double -> ignored.
      case Opcodes.F2D:
        break;
      default:
        throw new AnalyzeException(
            "Bytecode instruction with OpCode '" + insnNode.getOpcode() + "' is not supported.");
    }
    return statements;
  }

  /**
   * Reads the {@link Operation} using the given <code>operator</code> and the 2 top-most elements
   * in the given <code>operandStack</code>.
   * 
   * @param operator the Operation {@link Operator}
   * @param operandStack the stack of {@link Expression} from which to take the operands
   * @return the result {@link Operation}
   * 
   */
  private static Operation readOperation(final Operator operator,
      final LinkedList<Expression> operandStack) {
    final Expression rightOperand = operandStack.pollLast();
    final Expression leftOperand = operandStack.pollLast();
    return new Operation(operator, leftOperand, rightOperand);
  }

  /**
   * Takes the first {@link Expression} from the given {@link LinkedList}, assuming it is a
   * {@link NumberLiteral}, and returns a new {@link NumberLiteral} with its negated value.
   * 
   * @param operandStack the stack of {@link Expression} from which to take the operand
   * @return the result
   * @throws AnalyzeException if the operand is not {@link NumberLiteral}
   * 
   */
  private static NumberLiteral inverseInteger(final LinkedList<Expression> operandStack) {
    final Expression operand = operandStack.pollLast();
    try {
      final Number value = ((NumberLiteral) operand).getValue();
      return new NumberLiteral(-value.intValue());
    } catch (ClassCastException e) {
      throw new AnalyzeException(
          "Cannot perform the inversion of operand of type " + operand.getType(), e);
    }
  }

  /**
   * Extracts the comparison {@link CompoundExpressionOperator} from the given {@link JumpInsnNode}.
   * 
   * @param currentInstruction the comparison instruction
   * @return the corresponding {@link CompoundExpressionOperator}
   */
  private static CompoundExpressionOperator extractComparisonOperator(
      final AbstractInsnNode currentInstruction) {
    switch (currentInstruction.getOpcode()) {
      case Opcodes.IF_ACMPNE:
      case Opcodes.IF_ICMPNE:
      case Opcodes.IFNE:
        return CompoundExpressionOperator.NOT_EQUALS;
      case Opcodes.IF_ACMPEQ:
      case Opcodes.IF_ICMPEQ:
      case Opcodes.IFEQ:
        return CompoundExpressionOperator.EQUALS;
      case Opcodes.IF_ICMPLE:
      case Opcodes.IFLE:
        return CompoundExpressionOperator.LESS_EQUALS;
      case Opcodes.IF_ICMPLT:
      case Opcodes.IFLT:
        return CompoundExpressionOperator.LESS;
      case Opcodes.IF_ICMPGE:
      case Opcodes.IFGE:
        return CompoundExpressionOperator.GREATER_EQUALS;
      case Opcodes.IF_ICMPGT:
      case Opcodes.IFGT:
        return CompoundExpressionOperator.GREATER;
      default:
        throw new AnalyzeException(
            "Failed to retrieve the operator for the current comparison instruction (opcode: "
                + currentInstruction.getOpcode() + ")");
    }
  }

  /**
   * Reads the given {@link IntInsnNode} instruction and adds the associated {@link Expression} to
   * the given {@link LinkedList}.
   * 
   * @param intInsnNode the instruction to read
   * @param operandStack the expression stack to put on or pop from
   * @param localVariables the local variables
   */
  private static void readIntInstruction(final IntInsnNode intInsnNode,
      final LinkedList<Expression> operandStack, final LocalVariables localVariables) {
    switch (intInsnNode.getOpcode()) {
      case Opcodes.BIPUSH:
      case Opcodes.SIPUSH:
        final Expression literal = new NumberLiteral(intInsnNode.operand);
        LOGGER.trace("Adding literal {}", literal);
        operandStack.add(literal);
        break;
      default:
        LOGGER.warn("IntInsnNode with OpCode {} was ignored.", intInsnNode.getOpcode());
    }
  }

  /**
   * The {@link AbstractInsnNode#getOpcode()} value should be one of {@code IFEQ}, {@code IFNE},
   * {@code IFLT}, {@code IFGE}, {@code IFGT}, {@code IFLE}, {@code IFLT}, {@code IFGE},
   * {@code IFGT},{@code IF_ICMPEQ}, {@code IF_ICMPNE}, {@code IF_ICMPLT}, {@code IF_ICMPGE},
   * {@code IF_ICMPGT}, {@code IF_ICMPLE}, {@code IF_ACMPEQ}, {@code IF_ACMPNE}, {@code GOTO},
   * {@code JSR}, {@code IFNULL} or {@code IFNONNULL}.
   * 
   * 
   * @param instructionCursor the cursor for the current instruction to read
   * @param operandStack the stack of Expressions
   * @param capturedArguments the captured arguments
   * @param localVariables the local variables
   * @return the list of statements read from the jump instruction
   */
  private List<Statement> readJumpInstruction(final InsnCursor instructionCursor,
      final LinkedList<Expression> operandStack, final List<CapturedArgument> capturedArguments,
      final LocalVariables localVariables) {
    final JumpInsnNode jumpInsnNode = (JumpInsnNode) instructionCursor.getCurrent();
    final LabelNode jumpLabel = jumpInsnNode.label;
    // FIXME: add support for LCMP:
    // Takes two two-word long integers off the stack and compares them. If
    // the two integers are the same, the int 0 is pushed onto the stack. If
    // value2 is greater than value1, the int 1 is pushed onto the stack. If
    // value1 is greater than value2, the int -1 is pushed onto the stack.
    switch (jumpInsnNode.getOpcode()) {
      case Opcodes.IFEQ:
      case Opcodes.IFNE:
      case Opcodes.IFLE:
      case Opcodes.IFLT:
      case Opcodes.IFGE:
      case Opcodes.IFGT:
      case Opcodes.IF_ICMPEQ:
      case Opcodes.IF_ICMPNE:
      case Opcodes.IF_ICMPLE:
      case Opcodes.IF_ICMPLT:
      case Opcodes.IF_ICMPGE:
      case Opcodes.IF_ICMPGT:
      case Opcodes.IF_ACMPEQ:
      case Opcodes.IF_ACMPNE:
        return Arrays.asList(buildControlFlowStatement(instructionCursor, operandStack,
            capturedArguments, localVariables));
      case Opcodes.GOTO:
        final InsnCursor jumpInstructionCursor = instructionCursor.duplicate();
        jumpInstructionCursor.move(jumpLabel.getLabel());
        return readStatements(jumpInstructionCursor, operandStack, capturedArguments,
            localVariables);
      default:
        throw new AnalyzeException("Unexpected JumpInsnNode OpCode: " + jumpInsnNode.getOpcode());
    }
  }

  /**
   * Builds a {@link ControlFlowStatement} from the given elements
   * 
   * @param insnCursor the InsnCursor to read the execution branches
   * @param operandStack the stack of expressions waiting to be used
   * @param capturedArguments the captured argument references, if any
   * @param localVariables the local variables, if any
   * @return an {@link ControlFlowStatement}.
   */
  private Statement buildControlFlowStatement(final InsnCursor insnCursor,
      final LinkedList<Expression> operandStack, final List<CapturedArgument> capturedArguments,
      final LocalVariables localVariables) {
    final JumpInsnNode jumpInsnNode = (JumpInsnNode) insnCursor.getCurrent();
    final Expression comparisonExpression = getControlFlowExpression(jumpInsnNode, operandStack);
    final LabelNode jumpLabel = jumpInsnNode.label;
    final InsnCursor thenInstructionCursor = insnCursor;
    final InsnCursor elseInstructionCursor = insnCursor.duplicate().next();
    thenInstructionCursor.move(jumpLabel.getLabel());
    // reading statements using a clean Expression stacks for both branches
    final List<Statement> thenStatements = readStatements(thenInstructionCursor,
        new LinkedList<Expression>(), capturedArguments, localVariables);
    final List<Statement> elseStatements = readStatements(elseInstructionCursor,
        new LinkedList<Expression>(), capturedArguments, localVariables);
    return new ControlFlowStatement(comparisonExpression, thenStatements, elseStatements);
  }

  /**
   * Returns the {@link Expression} from the current {@link Expression} {@link LinkedList},
   * depending on the current context (i.e., the current {@link JumpInsnNode} and its previous
   * {@link AbstractInsnNode}.
   * 
   * @param jumpInsnNode the instruction
   * @param operandStack the stack of expressions
   * @return the comparison expression (can be an {@link CompoundExpression} or some other form of
   *         type of {@link Expression} that return a Boolean value)
   */
  private static Expression getControlFlowExpression(final JumpInsnNode jumpInsnNode,
      final LinkedList<Expression> operandStack) {
    final CompoundExpressionOperator comparisonOperator = extractComparisonOperator(jumpInsnNode);
    final Expression rightSideOperand = operandStack.pollLast();
    final Expression leftSideOperand = operandStack.isEmpty()
        ? getDefaultComparisonOperand(rightSideOperand) : operandStack.pollLast();
    if (leftSideOperand.equals(new BooleanLiteral(false))) {
      switch (comparisonOperator) {
        case EQUALS:
          // if we have: 'expr == false', just return '!expr'
          return rightSideOperand.inverse();
        case NOT_EQUALS:
          // if we have: 'expr != false', just return 'expr'
          return rightSideOperand;
        default:
          throw new AnalyzeException("There's no expression to compare with " + rightSideOperand
              + " " + comparisonOperator + " [expected something here]");
      }
    }
    // ensure the operand types match by forcing the right side to be the same type as the left side
    final Class<?> leftSideOperandType = getOperandType(leftSideOperand);
    final Expression castedRightOperand =
        castOperand(rightSideOperand, leftSideOperandType.getName());
    return new CompoundExpression(comparisonOperator,
        Arrays.asList(leftSideOperand, castedRightOperand));
  }

  /**
   * Attempts to cast the given operand (if it is a Literal) to the given {@code targetType}.
   * 
   * @param operand the operand to process
   * @param targetType the target type
   * @return the casted operand or the given operand if no cast could be performed
   */
  private static Expression castOperand(final Expression operand, final String targetTypeName) {
    if (operand.getType() == ExpressionType.NUMBER_LITERAL) {
      return ExpressionFactory.getLiteral((NumberLiteral) operand, targetTypeName);
    }
    return operand;
  }

  /**
   * @param operand the operand to analyze
   * @return the type of the operand, in particular, the {@code returnType} if the given
   *         {@code operand} is a {@link MethodInvocation}.
   * @see MethodInvocation#getReturnType()
   */
  private static Class<?> getOperandType(final Expression operand) {
    if (operand.getType() == ExpressionType.METHOD_INVOCATION) {
      return ((MethodInvocation) operand).getReturnType();
    }
    return operand.getJavaType();
  }

  /**
   * Attempts to return the default {@link Expression} to compare against the given one, because the
   * generated bytecode may not have one in some cases (eg: {@code if(a == null)}).
   * 
   * @param expression the {@link Expression} to compare
   * @return a default {@link Expression} to compare against the given one
   * @throws AnalyzeException when no default {@link Expression} can be provided.
   */
  private static Expression getDefaultComparisonOperand(final Expression expression) {
    if (expression != null && expression.getType() == ExpressionType.METHOD_INVOCATION) {
      final MethodInvocation methodInvocation = (MethodInvocation) expression;
      // if the expression is something like 'a.equals(b)', there's no need to add an extra ' ==
      // true' in the
      // equation.
      final Class<?> methodReturnType = methodInvocation.getReturnType();
      if (org.lambdamatic.analyzer.utils.ClassUtils.isBoolean(methodReturnType)) {
        return new BooleanLiteral(false);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isByte(methodReturnType)) {
        return new NumberLiteral((byte) 0);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isShort(methodReturnType)) {
        return new NumberLiteral((short) 0);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isInteger(methodReturnType)) {
        return new NumberLiteral(0);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isLong(methodReturnType)) {
        return new NumberLiteral(0L);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isFloat(methodReturnType)) {
        return new NumberLiteral(0f);
      } else if (org.lambdamatic.analyzer.utils.ClassUtils.isDouble(methodReturnType)) {
        return new NumberLiteral(0d);
      } else if (methodReturnType.equals(Character.class) || methodReturnType.equals(char.class)) {
        return new NullLiteral();
      } else if (methodReturnType.equals(String.class)) {
        return new NullLiteral();
      }
    } else if (expression != null && expression.getType() == ExpressionType.FIELD_ACCESS
        && (expression.getJavaType() == Boolean.class
            || expression.getJavaType() == boolean.class)) {
      return new BooleanLiteral(false);
    }
    throw new AnalyzeException("Can't give a default comparison operand for '" + expression + "'");
  }

  /**
   * Reads the given {@link TypeInsnNode} instruction.
   * 
   * @param typeInsnNode the instruction to read
   * @param operandStack the expression stack to put on or pop from.
   * @param localVariables the local variables
   */
  private static void readTypeInstruction(final TypeInsnNode typeInsnNode,
      final LinkedList<Expression> operandStack) {
    final Type instanceType = Type.getObjectType(typeInsnNode.desc);
    switch (typeInsnNode.getOpcode()) {
      case Opcodes.NEW:
        final ObjectInstanciation objectVariable =
            new ObjectInstanciation(getJavaType(instanceType));
        operandStack.add(objectVariable);
        break;
      case Opcodes.ANEWARRAY:
        final NumberLiteral arrayLength = (NumberLiteral) operandStack.pollLast();
        final ArrayVariable arrayVariable =
            new ArrayVariable(getArrayType(instanceType), arrayLength.getValue().intValue());
        operandStack.add(arrayVariable);
        break;
      default:
        LOGGER.warn("TypeInsnNode with OpCode {} was ignored.", typeInsnNode.getOpcode());
    }
  }

  /**
   * Reads the current ASTORE instruction, using elements from the given {@code operandStack}.
   * 
   * @param operandStack the stack of {@link Expression}
   */
  private static void readArrayStoreInstruction(final LinkedList<Expression> operandStack) {
    final Expression element = operandStack.pollLast();
    final NumberLiteral elementIndex = (NumberLiteral) operandStack.pollLast();
    final ArrayVariable targetArray = (ArrayVariable) operandStack.pollLast();
    targetArray.setElement(elementIndex.getValue().intValue(), element);
  }

  /**
   * Handler for the Loca Variables in the bytecode. The {@link LocalVariables} handles the elements
   * in a list and provides methods matching the <code>ALOAD</code> and <code>ASTORE</code>
   * instructions.
   */
  static class LocalVariables {

    private final List<LocalVariableNode> localVariableNodes;

    private final int initialSize;

    private LocalVariableNode currentVariable = null;

    public LocalVariables(final List<LocalVariableNode> localVariableNodes) {
      this.localVariableNodes = localVariableNodes;
      this.initialSize = localVariableNodes.size();
    }

    public List<LocalVariable> toLocalVariables() {
      return this.localVariableNodes.stream().limit(this.initialSize)
          .map(var -> (var != null && !"this".equals(var.name))
              ? new LocalVariable(var.index, var.name, readSignature(var.desc)) : null)
          .collect(Collectors.toList());
    }

    public LocalVariableNode load(final int index) {
      this.currentVariable = this.localVariableNodes.get(index);
      return this.currentVariable;
    }

    public LocalVariableNode store(final int index) {
      if (this.localVariableNodes.size() > index) {
        this.localVariableNodes.set(index, this.currentVariable);
      } else {
        this.localVariableNodes.add(index, this.currentVariable);
      }
      return this.currentVariable;
    }

  }

}
