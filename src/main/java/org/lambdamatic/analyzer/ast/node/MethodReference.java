/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast.node;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.lambdamatic.analyzer.exception.AnalyzeException;
import org.lambdamatic.analyzer.utils.ReflectionUtils;

/**
 * A method call: {@code expression.methodName(arguments)}
 * 
 * @author Xavier Coulon
 *
 */
public class MethodReference extends ComplexExpression {

  /** the class on which the method call is applied (may change if evaluated). */
  private Expression source;

  /**
   * the underlying Java {@link Method} or {@link Constructor} that is called.
   */
  private final Executable javaMethod;

  /**
   * the actual return type (preserving data from generics if available).
   */
  private final Class<?> returnType;

  /** the optional captured arguments retrieved while analyzing the associated method call. */
  private List<CapturedArgument> capturedArguments;

  /**
   * Full constructor.
   * 
   * @param sourceExpression the expression on which the method call is applied.
   * @param javaMethod the name of the called method.
   * @param capturedArguments the optional captured arguments retrieved while analyzing the
   *        associated method call
   */
  public MethodReference(final Expression sourceExpression, final Executable javaMethod,
      final List<CapturedArgument> capturedArguments) {
    this(generateId(), sourceExpression, javaMethod, ReflectionUtils.getReturnType(javaMethod),
        capturedArguments, false);
  }

  /**
   * Full constructor.
   * 
   * @param id the synthetic id of this {@link Expression}
   * @param sourceExpression the expression on which the method call is applied
   * @param javaMethod the actual {@link Method} or {@link Constructor}
   * @param capturedArguments the optional captured arguments retrieved while analyzing the
   *        associated method call
   * @param returnType the returned Java type of the underlying method
   * @param inverted flag to indicate if this expression is inverted
   */
  public MethodReference(final int id, final Expression sourceExpression,
      final Executable javaMethod, final Class<?> returnType,
      final List<CapturedArgument> capturedArguments, final boolean inverted) {
    super(id, inverted);
    setSourceExpression(sourceExpression);
    this.javaMethod = javaMethod;
    this.returnType = returnType;
    this.capturedArguments = capturedArguments;
  }

  private void setSourceExpression(final Expression sourceExpression) {
    this.source = sourceExpression;
    this.source.setParent(this);
  }

  @Override
  public MethodReference duplicate(int id) {
    return new MethodReference(id, getSource().duplicate(), this.javaMethod, this.returnType,
        this.capturedArguments, isInverted());
  }

  @Override
  public ExpressionType getType() {
    return ExpressionType.METHOD_REFERENCE;
  }

  @Override
  public boolean anyElementMatches(ExpressionType type) {
    return this.source.anyElementMatches(type);
  }

  /**
   * Returns the return type of the method. If the return type is a primitive type, its associated
   * Java Wrapper class is returned (eg: 'int' gives 'java.lang.Integer')
   * 
   * @see org.lambdamatic.analyzer.ast.node.Expression#getJavaType()
   */
  @Override
  public Class<?> getJavaType() {
    return getReturnType();
  }

  /**
   * @return the returned {@link Class} of the underlying Java {@link Method}.
   */
  public Class<?> getReturnType() {
    return this.returnType;
  }

  @Override
  public ComplexExpression getParent() {
    return (ComplexExpression) super.getParent();
  }

  /**
   * @return the source expression, on which this method invocation is performed (ie, on which the
   *         method is called).
   */
  public Expression getSource() {
    return this.source;
  }

  /**
   * @return the name of the method.
   */
  public String getMethodName() {
    return this.javaMethod.getName();
  }

  /**
   * @return the underlying Java {@link Method} or {@link Constructor}.
   * @throws AnalyzeException if not method was found.
   */
  public Executable getJavaMethod() {
    return this.javaMethod;
  }

  @Override
  public int getNumberOfBytecodeInstructions() {
    return 1 + this.getSource().getNumberOfBytecodeInstructions();
  }

  /**
   * Will attempt to evaluate this {@link MethodReference} and return its result, even if the
   * arguments contains {@link CapturedArgument}.
   * 
   * @return the underlying Java method result.
   * @throws AnalyzeException if the underlying Java {@link Method} could not be found or invoked.
   */
  public Object evaluate() {
    final List<Object> args = new ArrayList<>();
    final Object source = this.source.getValue();
    try {
      this.javaMethod.setAccessible(true);
      if (this.javaMethod instanceof Constructor) {
        return ((Constructor<?>) this.javaMethod).newInstance(args.toArray());
      }
      return ((Method) this.javaMethod).invoke(source, args.toArray());

    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException
        | InstantiationException e) {
      throw new AnalyzeException(
          "Failed to invoke method '" + this.javaMethod.getName() + "' on '" + source + "'", e);
    }
  }

  /**
   * @return the value of {@code this} Expression.
   */
  public Object getValue() {
    if (this.source.getType() == ExpressionType.CLASS_LITERAL) {
      return evaluate();
    }
    return evaluate();
  }

  @Override
  public void accept(final ExpressionVisitor visitor) {
    this.source.accept(visitor);
    visitor.visit(this);
  }

  /**
   * Replace the given {@code oldArgumoldExpressionent} with the given {@code newExpression} if it
   * is part of this {@link MethodReference} arguments or if it is the source.
   * 
   */
  public void replaceElement(final Expression oldExpression, final Expression newExpression) {
    if (oldExpression == this.source) {
      setSourceExpression(newExpression);
    }
  }

  @Override
  public MethodReference inverse() {
    return new MethodReference(generateId(), this.source, this.javaMethod, this.returnType,
        this.capturedArguments, !isInverted());
  }

  @Override
  public boolean canBeInverted() {
    return true;
  }

  @Override
  public String toString() {
    if (this.javaMethod instanceof Constructor) {
      return (isInverted() ? "!" : "") + this.source.getJavaType().getName() + "::new";
    }
    return (isInverted() ? "!" : "") + this.source.getJavaType().getName() + "::"
        + this.javaMethod.getName();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((getType() == null) ? 0 : getType().hashCode());
    result = prime * result + (isInverted() ? 1231 : 1237);
    result = prime * result + ((this.javaMethod == null) ? 0 : this.javaMethod.hashCode());
    result = prime * result + ((this.source == null) ? 0 : this.source.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    MethodReference other = (MethodReference) obj;
    if (isInverted() != other.isInverted()) {
      return false;
    }
    if (this.javaMethod == null) {
      if (other.javaMethod != null) {
        return false;
      }
    } else if (!this.javaMethod.getName().equals(other.javaMethod.getName())) {
      return false;
    } else if (!Arrays.deepEquals(this.javaMethod.getParameterTypes(),
        other.javaMethod.getParameterTypes())) {
      return false;
    }
    if (this.source == null) {
      if (other.source != null) {
        return false;
      }
    } else if (!this.source.equals(other.source)) {
      return false;
    }
    return true;
  }

  /**
   * Deletes this {@link MethodReference} from the Expression tree.
   */
  public void delete() {
    // replace this MethodElement with the source expression if the parent exists
    if (getParent() != null) {
      // preserve the inversion
      if (this.isInverted()) {
        getParent().replaceElement(this, getSource().inverse());
      } else {
        getParent().replaceElement(this, getSource());
      }
    }
  }

  /**
   * @return <code>true</code> if the method can be evaluated (ie, replace by its return value),
   *         <code>false</code> otherwise.
   */
  public boolean canEvaluate() {
    return false;
  }

}

