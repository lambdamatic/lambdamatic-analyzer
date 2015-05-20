/**
 * 
 */
package org.lambdamatic.analyzer.ast;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.lambdamatic.analyzer.LambdaExpressionAnalyzer;
import org.lambdamatic.analyzer.ast.node.ASTNode;
import org.lambdamatic.analyzer.ast.node.ArrayVariable;
import org.lambdamatic.analyzer.ast.node.BooleanLiteral;
import org.lambdamatic.analyzer.ast.node.CapturedArgument;
import org.lambdamatic.analyzer.ast.node.CapturedArgumentRef;
import org.lambdamatic.analyzer.ast.node.ClassLiteral;
import org.lambdamatic.analyzer.ast.node.Expression;
import org.lambdamatic.analyzer.ast.node.Expression.ExpressionType;
import org.lambdamatic.analyzer.ast.node.ExpressionFactory;
import org.lambdamatic.analyzer.ast.node.ExpressionStatement;
import org.lambdamatic.analyzer.ast.node.FieldAccess;
import org.lambdamatic.analyzer.ast.node.IfStatement;
import org.lambdamatic.analyzer.ast.node.InfixExpression;
import org.lambdamatic.analyzer.ast.node.InfixExpression.InfixOperator;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.LocalVariable;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.NullLiteral;
import org.lambdamatic.analyzer.ast.node.NumberLiteral;
import org.lambdamatic.analyzer.ast.node.ObjectVariable;
import org.lambdamatic.analyzer.ast.node.ReturnStatement;
import org.lambdamatic.analyzer.ast.node.Statement;
import org.lambdamatic.analyzer.exception.AnalyzeException;
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
 * An internal utility class that uses ASM to read the bytecode of a (desugared lambda expression) method and converts
 * it into an {@link ASTNode}.
 *
 * @author Xavier Coulon <xcoulon@redhat.com>
 *
 */
public class LambdaExpressionReader {

	/**
	 * A {@link SignatureVisitor} that retrieves the class type.
	 * 
	 * @author Xavier Coulon <xcoulon@redhat.com>
	 *
	 */
	private static final class ClassTypeRetriever extends SignatureVisitor {

		/** the className (once visited). */
		private String className = null;

		/**
		 * Constructor
		 */
		private ClassTypeRetriever() {
			super(Opcodes.ASM5);
		}

		@Override
		public void visitClassType(String name) {
			this.className = name;
			super.visitClassType(name);
		}

		/**
		 * @return the {@link Class} matching the {@code className} in the visited signature.
		 */
		private Class<?> getClassType() {
			try {
				return Class.forName(className.replace("/", "."));
			} catch (final ClassNotFoundException e) {
				throw new AnalyzeException("Failed to retrieve class with name " + className, e);
			}
		}

	}

	/** The usual Logger. */
	static final Logger LOGGER = LoggerFactory.getLogger(LambdaExpressionReader.class);

	/**
	 * Reads the given {@link List} of (bytecode) {@link AbstractInsnNode} located at the known {@link SerializedLambdaInfo} and computes a simplified {@link Statement}
	 * based tree representing the initial lambda expression.
	 * 
	 * @return the {@link List} of {@link Statement} found while reading the bytecode
	 * @throws IOException
	 */
	public Pair<List<Statement>, List<LocalVariable>> readBytecodeStatement(final SerializedLambdaInfo lambdaInfo) throws IOException {
		final LambdaExpressionClassVisitor desugaredExpressionVisitor = new LambdaExpressionClassVisitor(lambdaInfo);
		final InputStream serializedLambdaStream = Thread.currentThread().getContextClassLoader()
				.getResourceAsStream(lambdaInfo.getImplClassName().replace('.', '/') + ".class");
		final ClassReader classReader = new ClassReader(serializedLambdaStream);
		classReader.accept(desugaredExpressionVisitor, 0);
		final InsnList instructions = desugaredExpressionVisitor.getInstructions();
		final List<LocalVariableNode> localVariables = desugaredExpressionVisitor.getLocalVariables();
		final Map<String, AbstractInsnNode> labels = desugaredExpressionVisitor.getLabels();
		final InsnCursor insnCursor = new InsnCursor(instructions, labels);
		// we must set the cursor on the first instruction before calling the readStatementSubTree() method
		insnCursor.next();
		final List<Statement> statements = readStatements(insnCursor, new Stack<>(), lambdaInfo.getCapturedArguments(), localVariables);
		// now, let's identify the lambda expression arguments (_excluding_ the captured arguments)
		final int argsCount = Type.getArgumentTypes(lambdaInfo.getImplMethodDesc()).length;
		final List<LocalVariable> lambdaExpressionArguments = localVariables.stream().filter(v -> v!= null && !v.name.equals("this")).limit(argsCount).map(var -> new LocalVariable(var.index, var.name, readSignature(var.desc))).collect(Collectors.toList());
		return new ImmutablePair<List<Statement>, List<LocalVariable>>(statements, lambdaExpressionArguments);
	}

	/**
	 * Reads the bytecode from the given {@link InsnCursor}'s <strong>current position</strong>, until there is no
	 * further instruction to proceed. It is the responsability of the caller to set the cursor position.
	 * 
	 * @param insnCursor
	 *            the instruction cursor used to read the bytecode.
	 * @param expressionStack
	 *            the expression stack to put on or pop from.
	 * @param localVariables
	 *            the local variables
	 * @return
	 */
	private List<Statement> readStatements(final InsnCursor insnCursor, final Stack<Expression> expressionStack,
			final List<CapturedArgument> capturedArguments, final List<LocalVariableNode> localVariables) {
		final List<Statement> statements = new ArrayList<Statement>();
		while (insnCursor.hasCurrent()) {
			final AbstractInsnNode currentInstruction = insnCursor.getCurrent();
			switch (currentInstruction.getType()) {
			case AbstractInsnNode.VAR_INSN:
				final VarInsnNode varInstruction = (VarInsnNode) currentInstruction;
				// The 'var' operand is the index of a local variable
				if (varInstruction.var < capturedArguments.size()) {
					// not using actual captured argument but rather, use a _reference_ to it.
					final Object capturedArgumentValue = capturedArguments.get(varInstruction.var).getValue();
					final Class<?> capturedArgumentValueType = capturedArgumentValue != null
							? capturedArgumentValue.getClass() : Object.class;
					final CapturedArgumentRef capturedArgumentRef = new CapturedArgumentRef(varInstruction.var,
							capturedArgumentValueType);
					expressionStack.add(capturedArgumentRef);
				} else {
					final LocalVariableNode var = localVariables.get(varInstruction.var);
					expressionStack.add(new LocalVariable(var.index, var.name, readSignature(var.desc)));
				}
				break;
			case AbstractInsnNode.LDC_INSN:
				// let's move this instruction on top of the stack until it
				// is used as an argument during a method call
				final LdcInsnNode ldcInsnNode = (LdcInsnNode) currentInstruction;
				final Expression constant = ExpressionFactory.getExpression(ldcInsnNode.cst);
				LOGGER.trace("Stacking constant {}", constant);
				expressionStack.add(constant);
				break;
			case AbstractInsnNode.FIELD_INSN:
				final FieldInsnNode fieldInsnNode = (FieldInsnNode) currentInstruction;
				switch (fieldInsnNode.getOpcode()) {
				case Opcodes.GETSTATIC:
					final Type ownerType = Type.getType(fieldInsnNode.desc);
					final FieldAccess staticFieldAccess = new FieldAccess(new ClassLiteral(getType(ownerType)),
							fieldInsnNode.name);
					expressionStack.add(staticFieldAccess);
					break;

				case Opcodes.GETFIELD:
					final FieldAccess fieldAccess = new FieldAccess(expressionStack.pop(), fieldInsnNode.name);
					expressionStack.add(fieldAccess);
					break;
				}
				break;
			case AbstractInsnNode.METHOD_INSN:
				final MethodInsnNode methodInsnNode = (MethodInsnNode) currentInstruction;
				final Type[] argumentTypes = Type.getArgumentTypes(methodInsnNode.desc);
				final List<Expression> args = new ArrayList<>();
				final List<Class<?>> parameterTypes = new ArrayList<>();
				Stream.of(argumentTypes).forEach(argumentType -> {
					final Expression arg = expressionStack.pop();
					final String argumentClassName = argumentType.getClassName();
					args.add(castOperand(arg, argumentClassName));
					try {
						parameterTypes.add(ClassUtils.getClass(argumentClassName));
					} catch (Exception e) {
						throw new AnalyzeException("Failed to find class '" + argumentClassName + "'", e);
					}
				} );
				// arguments appear in reverse order in the bytecode
				Collections.reverse(args);
				switch (methodInsnNode.getOpcode()) {
				case Opcodes.INVOKEINTERFACE:
				case Opcodes.INVOKEVIRTUAL:
				case Opcodes.INVOKESPECIAL:
					// object instantiation
					if (methodInsnNode.name.equals("<init>")) {
						final ObjectVariable objectVariable = (ObjectVariable) expressionStack.pop();
						objectVariable.setInitArguments(args);
					} else {
						final Expression sourceExpression = expressionStack.pop();
						final Method javaMethod = ReflectionUtils.findJavaMethod(sourceExpression.getJavaType(),
								methodInsnNode.name, parameterTypes);
						final MethodInvocation invokedMethod = new MethodInvocation(sourceExpression, javaMethod, args);
						expressionStack.add(invokedMethod);
					}
					break;
				case Opcodes.INVOKESTATIC:
					final Type type = Type.getObjectType(methodInsnNode.owner);
					try {
						final Class<?> sourceClass = Class.forName(type.getClassName());
						final Method javaMethod = ReflectionUtils.findJavaMethod(sourceClass, methodInsnNode.name,
								parameterTypes);
						final MethodInvocation invokedStaticMethod = new MethodInvocation(new ClassLiteral(sourceClass),
								javaMethod, args);
						expressionStack.add(invokedStaticMethod);
					} catch (ClassNotFoundException e) {
						throw new AnalyzeException("Failed to retrieve class for " + methodInsnNode.owner, e);
					}
					break;
				default:
					throw new AnalyzeException("Unexpected method invocation type: " + methodInsnNode.getOpcode());
				}
				break;
			case AbstractInsnNode.INVOKE_DYNAMIC_INSN:
				final InvokeDynamicInsnNode invokeDynamicInsnNode = (InvokeDynamicInsnNode) currentInstruction;
				final Handle handle = (Handle) invokeDynamicInsnNode.bsmArgs[1];
				final int argNumber = Type.getArgumentTypes(invokeDynamicInsnNode.desc).length;
				final List<CapturedArgumentRef> lambdaArgs = new ArrayList<CapturedArgumentRef>();
				for (int i = 0; i < argNumber; i++) {
					final Expression expr = expressionStack.pop();
					if (expr.getExpressionType() != ExpressionType.CAPTURED_ARGUMENT_REF) {
						throw new AnalyzeException("Unexpected argument type when following InvokeDynamic call: "
								+ expr.getExpressionType());
					}
					lambdaArgs.add((CapturedArgumentRef) expr); // , expr.getValue()
				}
				Collections.reverse(lambdaArgs);
				final EmbeddedSerializedLambdaInfo lambdaInfo = new EmbeddedSerializedLambdaInfo(handle.getOwner(),
						handle.getName(), handle.getDesc(), lambdaArgs, capturedArguments);
				final LambdaExpression lambdaExpression = LambdaExpressionAnalyzer.getInstance()
						.analyzeExpression(lambdaInfo);
				expressionStack.add(lambdaExpression);
				// throw new AnalyzeException("Nested InvokeDynamic instruction is not supported yet. Should analyze " +
				// handle.getOwner() + "." + handle.getName() + " (desc=" + handle.getDesc() + ")");
				break;
			case AbstractInsnNode.JUMP_INSN:
				statements.addAll(readJumpInstruction(insnCursor, expressionStack, capturedArguments, localVariables));
				break;
			case AbstractInsnNode.INT_INSN:
				readIntInstruction((IntInsnNode) currentInstruction, expressionStack, localVariables);
				break;
			case AbstractInsnNode.INSN:
				final List<Statement> instructionStmts = readInstruction(insnCursor, expressionStack, capturedArguments,
						localVariables);
				statements.addAll(instructionStmts);
				break;
			case AbstractInsnNode.TYPE_INSN:
				readTypeInstruction((TypeInsnNode) currentInstruction, expressionStack, localVariables);
				break;
			default:
				throw new AnalyzeException("This is embarrassing... We've reached an unexpected instruction operator: "
						+ currentInstruction.getType());
			}
			insnCursor.next();
		}
		return statements;
	}

	/**
	 * @param typeName
	 *            the fully qualified name of the type (primitive or Objecttype )
	 * @return the returned Java type.
	 * @throws NegativeArraySizeException
	 * @throws AnalyzeException
	 *             if the Class could not be found
	 */
	private Class<?> getArrayType(final Type type) throws NegativeArraySizeException {
		try {
			// TODO: can we avoid the array instantiation ?
			final Type elementType = Type.getType(type.getDescriptor());
			return Array.newInstance(Class.forName(elementType.getClassName()), 0).getClass();
		} catch (ClassNotFoundException e) {
			throw new AnalyzeException("Failed to retrieve type named " + type.getClassName(), e);
		}
	}

	/**
	 * @param typeName
	 *            the fully qualified name of the type (primitive or Objecttype )
	 * @return the returned Java type.
	 * @throws AnalyzeException
	 *             if the Class could not be found
	 */
	private Class<?> getType(final Type type) {
		try {
			if (type.getSort() == Type.ARRAY) {
				// TODO: can we avoid the array instantiation ?
				final Type elementType = type.getElementType();
				return Array.newInstance(Class.forName(elementType.getClassName()), 0).getClass();
			} else {
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
			}
		} catch (ClassNotFoundException e) {
			throw new AnalyzeException("Failed to retrieve type named " + type.getClassName(), e);
		}
	}

	/**
	 * Reads the given type signature and returns a proper {@link Class}
	 * 
	 * @param desc
	 *            the type signature to read
	 * @return the associated {@link Class}
	 * 
	 */
	private Class<?> readSignature(final String desc) {
		SignatureReader signatureReader = new SignatureReader(desc);
		final ClassTypeRetriever classTypeRetriever = new ClassTypeRetriever();
		signatureReader.accept(classTypeRetriever);
		return classTypeRetriever.getClassType();
	}

	/**
	 * Reads the current {@link InsnNode} instruction and returns a {@link Statement} or {@code null} if the instruction
	 * is not a full statement (in that case, the instruction is stored in the given Expression {@link Stack}).
	 * 
	 * @param insnNode
	 *            the instruction to read
	 * @param expressionStack
	 *            the expression stack to put on or pop from.
	 * @param localVariables
	 *            the local variables
	 * @return a {@link Statement} or {@code null}
	 */
	private List<Statement> readInstruction(final InsnCursor insnCursor, final Stack<Expression> expressionStack,
			final List<CapturedArgument> capturedArguments, final List<LocalVariableNode> localVariables) {
		final List<Statement> statements = new ArrayList<Statement>();
		final AbstractInsnNode insnNode = insnCursor.getCurrent();
		switch (insnNode.getOpcode()) {
		case Opcodes.ARETURN:
		case Opcodes.IRETURN:
			statements.add(new ReturnStatement(expressionStack.pop()));
			break;
		case Opcodes.RETURN:
			// any previous expression in the stack should be wrapped into statements, but the 
			// empty 'return' statement can be ignored because it does not carry any semantic
			expressionStack.stream().forEach(e -> statements.add(new ExpressionStatement(e)));
			//statements.add(new ReturnStatement(null));
			break;
		case Opcodes.ACONST_NULL:
			expressionStack.add(new NullLiteral());
			break;
		case Opcodes.ICONST_0:
			// applies for byte, short, int and boolean
			expressionStack.add(new NumberLiteral(0));
			break;
		case Opcodes.ICONST_1:
			// applies for byte, short, int and boolean
			expressionStack.add(new NumberLiteral(1));
			break;
		case Opcodes.ICONST_2:
			expressionStack.add(new NumberLiteral(2));
			break;
		case Opcodes.ICONST_3:
			expressionStack.add(new NumberLiteral(3));
			break;
		case Opcodes.ICONST_4:
			expressionStack.add(new NumberLiteral(4));
			break;
		case Opcodes.ICONST_5:
			expressionStack.add(new NumberLiteral(5));
			break;
		case Opcodes.LCONST_0:
			expressionStack.add(new NumberLiteral(0l));
			break;
		case Opcodes.LCONST_1:
			expressionStack.add(new NumberLiteral(1l));
			break;
		case Opcodes.FCONST_0:
			expressionStack.add(new NumberLiteral(0f));
			break;
		case Opcodes.FCONST_1:
			expressionStack.add(new NumberLiteral(1f));
			break;
		case Opcodes.FCONST_2:
			expressionStack.add(new NumberLiteral(2f));
			break;
		case Opcodes.DCONST_0:
			expressionStack.add(new NumberLiteral(0d));
			break;
		case Opcodes.DCONST_1:
			expressionStack.add(new NumberLiteral(1d));
			break;
		case Opcodes.LCMP:
		case Opcodes.DCMPL:
		case Opcodes.DCMPG:
		case Opcodes.FCMPL:
		case Opcodes.FCMPG:
			statements.addAll(readJumpInstruction(insnCursor.next(), expressionStack, capturedArguments, localVariables));
			break;
		case Opcodes.IADD:
			expressionStack.add(addIntegers(expressionStack));
			break;
		case Opcodes.ISUB:
			expressionStack.add(substractIntegers(expressionStack));
			break;
		case Opcodes.INEG:
			expressionStack.add(inverseInteger(expressionStack));
			break;
		case Opcodes.POP:
			statements.add(new ExpressionStatement(expressionStack.pop()));
			break;
		case Opcodes.DUP:
			final Expression lastExpression = expressionStack.peek();
			expressionStack.push(lastExpression);
			break;
		case Opcodes.AASTORE:
			readArrayStoreInstruction(insnNode, expressionStack);
			break;
		default:
			throw new AnalyzeException("Bytecode instruction with OpCode '" + insnNode.getOpcode() + "' was ignored.");
		}
		// no statement to return for now, instruction was put on top of
		// ExpressionStack for further usage
		return statements;
	}

	/**
	 * Takes the 2 first {@link Expression} from the given {@link Stack}, assuming they are both {@link NumberLiteral},
	 * performs a subtraction and returns the result.
	 * 
	 * @param expressionStack
	 *            the stack of {@link Expression} from which to take the operands
	 * @return the result
	 * @throws AnalyzeException
	 *             if the 2 operands are not {@link NumberLiteral}
	 * 
	 */
	private NumberLiteral addIntegers(final Stack<Expression> expressionStack) {
		final Expression operand1 = expressionStack.pop();
		final Expression operand2 = expressionStack.pop();
		try {
			final Number value1 = ((NumberLiteral) operand1).getValue();
			final Number value2 = ((NumberLiteral) operand2).getValue();
			return new NumberLiteral(value2.intValue() + value1.intValue());
		} catch (ClassCastException e) {
			throw new AnalyzeException("Cannot perform the addition between operands of type "
					+ operand1.getExpressionType() + " and " + operand2.getExpressionType());
		}
	}

	/**
	 * Takes the 2 first {@link Expression} from the given {@link Stack}, assuming they are both {@link NumberLiteral},
	 * performs a subtraction and returns the result.
	 * 
	 * @param expressionStack
	 *            the stack of {@link Expression} from which to take the operands
	 * @return the result
	 * @throws AnalyzeException
	 *             if the 2 operands are not {@link NumberLiteral}
	 * 
	 */
	private NumberLiteral substractIntegers(final Stack<Expression> expressionStack) {
		final Expression operand1 = expressionStack.pop();
		final Expression operand2 = expressionStack.pop();
		try {
			final Number value1 = ((NumberLiteral) operand1).getValue();
			final Number value2 = ((NumberLiteral) operand2).getValue();
			return new NumberLiteral(value2.intValue() - value1.intValue());
		} catch (ClassCastException e) {
			throw new AnalyzeException("Cannot perform the subtraction between operands of type "
					+ operand1.getExpressionType() + " and " + operand2.getExpressionType());
		}
	}

	/**
	 * Takes the first {@link Expression} from the given {@link Stack}, assuming it is a {@link NumberLiteral}, and
	 * returns a new {@link NumberLiteral} with its negated value.
	 * 
	 * @param expressionStack
	 *            the stack of {@link Expression} from which to take the operand
	 * @return the result
	 * @throws AnalyzeException
	 *             if the operand is not {@link NumberLiteral}
	 * 
	 */
	private NumberLiteral inverseInteger(final Stack<Expression> expressionStack) {
		final Expression operand = expressionStack.pop();
		try {
			final Number value = ((NumberLiteral) operand).getValue();
			return new NumberLiteral(-value.intValue());
		} catch (ClassCastException e) {
			throw new AnalyzeException(
					"Cannot perform the inversion of operand of type " + operand.getExpressionType());
		}
	}

	/**
	 * Extracts the comparison {@link InfixOperator} from the given {@link JumpInsnNode}.
	 * 
	 * @param currentInstruction
	 *            the comparison instruction
	 * @return the corresponding {@link InfixOperator}
	 */
	private InfixOperator extractComparisonOperator(final AbstractInsnNode currentInstruction) {
		switch (currentInstruction.getOpcode()) {
		case Opcodes.IF_ACMPNE:
		case Opcodes.IF_ICMPNE:
		case Opcodes.IFNE:
			return InfixOperator.NOT_EQUALS;
		case Opcodes.IF_ACMPEQ:
		case Opcodes.IF_ICMPEQ:
		case Opcodes.IFEQ:
			return InfixOperator.EQUALS;
		case Opcodes.IF_ICMPLE:
		case Opcodes.IFLE:
			return InfixOperator.LESS_EQUALS;
		case Opcodes.IF_ICMPLT:
		case Opcodes.IFLT:
			return InfixOperator.LESS;
		case Opcodes.IF_ICMPGE:
		case Opcodes.IFGE:
			return InfixOperator.GREATER_EQUALS;
		case Opcodes.IF_ICMPGT:
		case Opcodes.IFGT:
			return InfixOperator.GREATER;
		default:
			throw new AnalyzeException(
					"Failed to retrieve the operator for the current comparison instruction (opcode: "
							+ currentInstruction.getOpcode() + ")");
		}
	}

	/**
	 * Reads the given {@link IntInsnNode} instruction and adds the associated {@link Expression} to the given
	 * {@link Stack}.
	 * 
	 * @param intInsnNode
	 *            the instruction to read
	 * @param expressionStack
	 *            the expression stack to put on or pop from.
	 * @param localVariables
	 *            the local variables
	 */
	private void readIntInstruction(final IntInsnNode intInsnNode, final Stack<Expression> expressionStack,
			final List<LocalVariableNode> localVariables) {
		switch (intInsnNode.getOpcode()) {
		case Opcodes.BIPUSH:
			// expressionStack.add(LiteralFactory.getLiteral(intInsnNode.operand, expressionStack.peek()));
			final Expression literal = new NumberLiteral(intInsnNode.operand);
			LOGGER.trace("Stacking literal {}", literal);
			expressionStack.add(literal);
			break;
		default:
			LOGGER.warn("IntInsnNode with OpCode {} was ignored.", intInsnNode.getOpcode());
		}
	}

	/**
	 * Converter Constructor: attempts to convert the given value {@link Expression} into a {@link BooleanLiteral} or a
	 * {@link MethodInvocation}, or throws an {@link IllegalArgumentException} if the conversion was not possible.
	 * 
	 * @param value
	 *            the initial value
	 * @return a {@link BooleanLiteral} or a {@link MethodInvocation}
	 * @throws IllegalArgumentException
	 *             if the conversion is not possible
	 */
	static Expression convert(final Expression value) throws IllegalArgumentException {
		switch (value.getExpressionType()) {
		case METHOD_INVOCATION:
		case BOOLEAN_LITERAL:
			return value;
		case NUMBER_LITERAL:
			final Number numberLiteralValue = ((NumberLiteral) value).getValue();
			if (numberLiteralValue.intValue() == 0) {
				return new BooleanLiteral(false);
			} else if (numberLiteralValue.intValue() == 1) {
				return new BooleanLiteral(true);
			}
		default:
			throw new IllegalArgumentException(
					"Could not convert '" + value + "' (" + value.getExpressionType() + ") into a Boolean Literal");
		}
	}

	/**
	 * The {@link AbstractInsnNode#getOpcode()} value should be one of {@code IFEQ}, {@code IFNE}, {@code IFLT},
	 * {@code IFGE}, {@code IFGT}, {@code IFLE}, {@code IFLT}, {@code IFGE}, {@code IFGT},{@code IF_ICMPEQ},
	 * {@code IF_ICMPNE}, {@code IF_ICMPLT}, {@code IF_ICMPGE}, {@code IF_ICMPGT}, {@code IF_ICMPLE}, {@code IF_ACMPEQ},
	 * {@code IF_ACMPNE}, {@code GOTO}, {@code JSR}, {@code IFNULL} or {@code IFNONNULL}
	 * 
	 * 
	 * @param instructionCursor
	 * @param expressionStack
	 * @param localVariables
	 * @return
	 */
	private List<Statement> readJumpInstruction(final InsnCursor instructionCursor, final Stack<Expression> expressionStack,
			final List<CapturedArgument> capturedArguments, final List<LocalVariableNode> localVariables) {
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
			return Arrays.asList(buildComparisonStatement(instructionCursor, expressionStack, capturedArguments, localVariables));
		case Opcodes.GOTO:
			final InsnCursor jumpInstructionCursor = instructionCursor.duplicate();
			jumpInstructionCursor.move(jumpLabel.getLabel());
			return readStatements(jumpInstructionCursor, expressionStack, capturedArguments, localVariables);
		default:
			throw new AnalyzeException("Unexpected JumpInsnNode OpCode: " + jumpInsnNode.getOpcode());
		}
	}

	/**
	 * Builds a comparison {@link IfStatement} from the given elements
	 * 
	 * @param jumpInsnNode
	 *            the node to follow if the 'if' expression is {@code false} (during code execution - at runtime)
	 * @param expressionStack
	 *            the stack of expressions waiting to be used
	 * @param capturedArguments
	 *            the captured argument references, if any
	 * @param localVariables
	 *            the local variables, if any
	 * @return an {@link IfStatement}.
	 */
	private Statement buildComparisonStatement(final InsnCursor insnCursor, final Stack<Expression> expressionStack,
			final List<CapturedArgument> capturedArguments, final List<LocalVariableNode> localVariables) {
		final JumpInsnNode jumpInsnNode = (JumpInsnNode) insnCursor.getCurrent();
		final LabelNode jumpLabel = jumpInsnNode.label;
		final InsnCursor jumpInstructionCursor = insnCursor.duplicate().move(jumpLabel.getLabel());
		final Expression comparisonExpression = getComparisonExpression(jumpInsnNode, expressionStack);
		final List<Statement> thenStatement = (List<Statement>) readStatements(jumpInstructionCursor, expressionStack,
				capturedArguments, localVariables);
		final List<Statement> elseStatement = (List<Statement>) readStatements(insnCursor.next(), expressionStack,
				capturedArguments, localVariables);
		return new IfStatement(comparisonExpression, thenStatement, elseStatement);
	}

	/**
	 * Returns the {@link InfixExpression} from the current {@link Expression} {@link Stack}, depending on the current
	 * context (i.e., the current {@link JumpInsnNode} and its previous {@link AbstractInsnNode}.
	 * 
	 * @param jumpInsnNode
	 *            the instruction
	 * @param expressionStack
	 *            the stack of expressions
	 * @return the comparison expression (can be an {@link InfixExpression} or some other form of {@link Expression})
	 */
	private Expression getComparisonExpression(final JumpInsnNode jumpInsnNode,
			final Stack<Expression> expressionStack) {
		final InfixOperator comparisonOperator = extractComparisonOperator(jumpInsnNode);
		final Expression rightSideOperand = expressionStack.pop();
		final Expression leftSideOperand = (expressionStack.empty() ? getDefaultComparisonOperand(rightSideOperand)
				: expressionStack.pop());
		if (leftSideOperand.equals(new BooleanLiteral(Boolean.FALSE))) {
			switch (comparisonOperator) {
				// if we have: 'expr == false', just return '!expr'
			case EQUALS:
				return rightSideOperand.inverse();
				// if we have: 'expr != false', just return 'expr'
			case NOT_EQUALS:
				return rightSideOperand;
			default:
				throw new AnalyzeException("There's no expression to compare with " + rightSideOperand + " "
						+ comparisonOperator + " [expected something here]");
			}
		}
		// ensure the operand types match by forcing the right side to be the same type as the left side
		final Class<?> leftSideOperandType = getOperandType(leftSideOperand);
		final Expression castedRightOperand = castOperand(rightSideOperand, leftSideOperandType.getName());
		final InfixExpression comparisonExpression = new InfixExpression(comparisonOperator,
				Arrays.asList(leftSideOperand, castedRightOperand));
		return comparisonExpression;
	}

	/**
	 * Attempts to cast the given operand (if it is a Literal) to the given {@code targetType}
	 * 
	 * @param operand
	 *            the operand to process
	 * @param targetType
	 *            the target type
	 * @return the casted operand or the given operand if no cast could be performed
	 */
	private Expression castOperand(final Expression operand, final String targetTypeName) {
		switch (operand.getExpressionType()) {
		case NUMBER_LITERAL:
			return ExpressionFactory.getLiteral((NumberLiteral) operand, targetTypeName);
		default:
			return operand;
		}
	}

	/**
	 * @param operand
	 *            the operand to analyze
	 * @return the type of the operand, in particular, the {@code returnType} if the given {@code operand} is a
	 *         {@link MethodInvocation}.
	 * @see MethodInvocation#getReturnType()
	 */
	private Class<?> getOperandType(final Expression operand) {
		switch (operand.getExpressionType()) {
		case METHOD_INVOCATION:
			return ((MethodInvocation) operand).getReturnType();
		default:
			return operand.getJavaType();
		}
	}

	/**
	 * Attempts to return the default {@link Expression} to compare against the given one, because the generated
	 * bytecode may not have one in some cases (eg: {@code if(a == null)}).
	 * 
	 * @param expression
	 *            the {@link Expression} to compare
	 * @return a default {@link Expression} to compare against the given one
	 * @throws AnalyzeException
	 *             when no default {@link Expression} can be provided.
	 */
	private Expression getDefaultComparisonOperand(final Expression expression) {
		if (expression != null && expression.getExpressionType() == ExpressionType.METHOD_INVOCATION) {
			final MethodInvocation methodInvocation = (MethodInvocation) expression;
			// if the expression is something like 'a.equals(b)', there's no need to add an extra ' == true' in the
			// equation.
			if (methodInvocation.getReturnType().equals(Boolean.class)
					|| methodInvocation.getReturnType().equals(boolean.class)) {
				return new BooleanLiteral(Boolean.FALSE);
			} else if (methodInvocation.getReturnType().equals(Byte.class)
					|| methodInvocation.getReturnType().equals(byte.class)) {
				return new NumberLiteral((byte) 0);
			} else if (methodInvocation.getReturnType().equals(Short.class)
					|| methodInvocation.getReturnType().equals(short.class)) {
				return new NumberLiteral((short) 0);
			} else if (methodInvocation.getReturnType().equals(Integer.class)
					|| methodInvocation.getReturnType().equals(int.class)) {
				return new NumberLiteral(0);
			} else if (methodInvocation.getReturnType().equals(Long.class)
					|| methodInvocation.getReturnType().equals(long.class)) {
				return new NumberLiteral(0l);
			} else if (methodInvocation.getReturnType().equals(Float.class)
					|| methodInvocation.getReturnType().equals(float.class)) {
				return new NumberLiteral(0f);
			} else if (methodInvocation.getReturnType().equals(Double.class)
					|| methodInvocation.getReturnType().equals(float.class)) {
				return new NumberLiteral(0d);
			} else if (methodInvocation.getReturnType().equals(Character.class)
					|| methodInvocation.getReturnType().equals(char.class)) {
				return new NullLiteral();
			} else if (methodInvocation.getReturnType().equals(String.class)) {
				return new NullLiteral();
			}
		}
		throw new AnalyzeException("Sorry, I can't give a default comparison operand for '" + expression + "'");
	}

	/**
	 * Reads the given {@link TypeInsnNode} instruction.
	 * 
	 * @param typeInsnNode
	 *            the instruction to read
	 * @param expressionStack
	 *            the expression stack to put on or pop from.
	 * @param localVariables
	 *            the local variables
	 */
	private void readTypeInstruction(final TypeInsnNode typeInsnNode, final Stack<Expression> expressionStack,
			final List<LocalVariableNode> localVariables) {
		switch (typeInsnNode.getOpcode()) {
		case Opcodes.NEW:
			final Type instanceType = Type.getObjectType(typeInsnNode.desc);
			final ObjectVariable objectVariable = new ObjectVariable(getType(instanceType));
			expressionStack.push(objectVariable);
			break;
		case Opcodes.ANEWARRAY:
			final Type parameterType = Type.getObjectType(typeInsnNode.desc);
			final NumberLiteral arrayLength = (NumberLiteral) expressionStack.pop();
			final ArrayVariable arrayVariable = new ArrayVariable(getArrayType(parameterType),
					arrayLength.getValue().intValue());
			expressionStack.push(arrayVariable);
			break;
		default:
			LOGGER.warn("TypeInsnNode with OpCode {} was ignored.", typeInsnNode.getOpcode());
		}
	}

	/**
	 * Reads the current ASTORE instruction, using elements from the given {@code expressionStack}.
	 * 
	 * @param storeInsn
	 *            the store instruction
	 * @param expressionStack
	 *            the stack of {@link Expression}
	 */
	private void readArrayStoreInstruction(final AbstractInsnNode storeInsn, final Stack<Expression> expressionStack) {
		final Expression element = expressionStack.pop();
		final NumberLiteral elementIndex = (NumberLiteral) expressionStack.pop();
		final ArrayVariable targetArray = (ArrayVariable) expressionStack.pop();
		targetArray.setElement(elementIndex.getValue().intValue(), element);
	}

}
