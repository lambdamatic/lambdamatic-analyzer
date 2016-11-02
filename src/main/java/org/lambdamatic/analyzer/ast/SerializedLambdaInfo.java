/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.lambdamatic.analyzer.ast.node.CapturedArgument;
import org.lambdamatic.analyzer.utils.ClassUtils;
import org.lambdamatic.analyzer.utils.ReflectionUtils;
import org.objectweb.asm.Type;

/**
 * Info about the Lambda Expression location.
 * 
 * @see SerializedLambda
 */
public class SerializedLambdaInfo {

  /** the implementation class. */
  private final Class<?> implClass;

  /** the signature of the implementation method. */
  private final String implMethodDesc;

  /** the underlying Java method or constructor to be called when invoking the lambda expression. */
  private final Executable implExecutable;

  /**
   * the (potentially empty) list of actual {@link CapturedArgument}.
   */
  private final List<CapturedArgument> capturedArguments;

  /**
   * The signature of the functional interface method. Usefull to know if a result must be returned
   * when using method references in Lambda expressions.
   */
  private final String instantiatedMethodType;

  /**
   * Full constructor
   * 
   * @param serializedLambda the fully {@link SerializedLambda} carrying all the required info.
   */
  public SerializedLambdaInfo(final SerializedLambda serializedLambda) {
    this(serializedLambda.getInstantiatedMethodType(),
        Type.getObjectType(serializedLambda.getImplClass()).getClassName(),
        serializedLambda.getImplMethodName(), serializedLambda.getImplMethodSignature(),
        serializedLambda.getImplMethodKind(), getCapturedArguments(serializedLambda));
  }

  /**
   * Constructor.
   * 
   * @param instantiatedMethodType The signature of the primary functional interface method after
   *        type variables are substituted with their instantiation from the capture site
   * 
   * @param instantiatedMethodType the signature of the functional interface method. Useful to know
   *        if a result must be returned when using method references in Lambda expressions
   * @param implClassName the fully qualified name of the Lambda implementation Class
   * @param implMethodName the name of the Lambda implementation method
   * @param implMethodSignature the signature of the Lambda implementation method
   * @param implMethodKind Method handle kind for the implementation method
   * @param capturedArguments the captured arguments when calling the Lambda expression
   */
  SerializedLambdaInfo(final String instantiatedMethodType, final String implClassName,
      final String implMethodName, final String implMethodSignature, final int implMethodKind,
      final List<CapturedArgument> capturedArguments) {
    this.instantiatedMethodType = instantiatedMethodType;
    this.implClass = ClassUtils.getClass(implClassName);
    this.implMethodDesc = implMethodSignature;
    this.implExecutable =
        ReflectionUtils.getDeclaredMethod(this.implClass, implMethodName, implMethodSignature);
    this.capturedArguments = capturedArguments;
  }

  /**
   * Returns the list of {@link CapturedArgument} for the {@link SerializedLambda}.
   * 
   * @param serializedLambda the {@link SerializedLambda} to analyze.
   * @return the list of {@link CapturedArgument}
   */
  public static List<CapturedArgument> getCapturedArguments(
      final SerializedLambda serializedLambda) {
    final List<CapturedArgument> capturedArguments = new ArrayList<>();
    for (int i = 0; i < serializedLambda.getCapturedArgCount(); i++) {
      capturedArguments.add(new CapturedArgument(serializedLambda.getCapturedArg(i)));
    }
    return capturedArguments;
  }

  /**
   * @return the (potentially empty) list of actual {@link CapturedArgument}.
   */
  public List<CapturedArgument> getCapturedArguments() {
    return this.capturedArguments;
  }

  /**
   * @return the signature of the functional interface method. Usefull to know if a result must be
   *         returned when using method references in Lambda expressions.
   */
  public String getInstantiatedMethodType() {
    return this.instantiatedMethodType;
  }

  /**
   * @return the implementation class.
   */
  public Class<?> getImplClass() {
    return this.implClass;
  }

  /**
   * @return the underlying Java {@link Method} or {@link Constructor} to be called when invoking
   *         the lambda expression.
   */
  public Executable getImplMethod() {
    return this.implExecutable;
  }

  /**
   * @return the signature of the implementation method.
   */
  public String getImplMethodDesc() {
    return this.implMethodDesc;
  }

  /**
   * @return the fully qualified location of the Lambda Expression implementation.
   */
  public String getImplMethodId() {
    return this.implClass.getName() + "." + this.implExecutable.getName() + "("
        + this.implMethodDesc + ")";
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.implClass == null) ? 0 : this.implClass.hashCode());
    result = prime * result + ((this.implExecutable == null) ? 0 : this.implExecutable.hashCode());
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
    SerializedLambdaInfo other = (SerializedLambdaInfo) obj;
    if (this.implClass == null) {
      if (other.implClass != null) {
        return false;
      }
    } else if (!this.implClass.equals(other.implClass)) {
      return false;
    }
    if (this.implExecutable == null) {
      if (other.implExecutable != null) {
        return false;
      }
    } else if (!this.implExecutable.equals(other.implExecutable)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "SerializedLambdaInfo for " + this.implClass.getName() + "."
        + this.implExecutable.getName();
  }

}
