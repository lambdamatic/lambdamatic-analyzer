/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.utils;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.lambdamatic.analyzer.exception.AnalyzeException;
import org.objectweb.asm.Type;

/**
 * Utility class to around Reflection API.
 * 
 * @author Xavier Coulon
 *
 */
public class ReflectionUtils {

  /**
   * Looks for the Java {@link Method} matching the given arguments
   * 
   * @param javaType the Java {@link Class} to which the method belongs to.
   * @param methodName the name of the method to find
   * @param parameterTypes the type of the parameters
   * @return the Java {@link Method}
   * @throws AnalyzeException if the {@link Method} was not found or in case of
   *         {@link SecurityException}.
   */
  public static Method getMethod(final Class<?> javaType, final String methodName,
      final List<Class<?>> parameterTypes) {
    try {
      return javaType.getMethod(methodName, parameterTypes.toArray(new Class<?>[0]));
    } catch (NoSuchMethodException | SecurityException e) {
      final String message = MessageFormat.format("Failed to locate method {0}.{1}({2})",
          javaType.getName(), methodName,
          parameterTypes.stream().map(t -> t.getName()).collect(Collectors.joining(", ")));
      throw new AnalyzeException(message, e);
    }
  }

  /**
   * Finds the {@link Method} or {@link Constructor} with the given {@code methodName} and
   * {@code methodSignature} in the given {@code className}.
   * 
   * @param parentClass the class to analyze
   * @param methodName the name of the method to find
   * @param methodSignature the signature of the method to look-up
   * @return the corresponding {@link Method} or {@link Constructor}
   * @throws AnalyzeException if something happened and the method could not be located
   */
  public static Executable getDeclaredMethod(final Class<?> parentClass, final String methodName,
      final String methodSignature) {
    final Class<?>[] parameterTypes = Stream.of(Type.getArgumentTypes(methodSignature)).map(t -> {
      try {
        if (t.getSort() == Type.ARRAY) {
          return Class.forName(ClassUtils.toJLSClassName(t.getClassName()));
        }
        return Class.forName(t.getClassName());
      } catch (ClassNotFoundException e) {
        final String message =
            MessageFormat.format("Failed to load argument of type {0} in method {1}.{2} (desc={3})",
                t.getClassName(), parentClass.getName(), methodName, methodSignature);
        throw new AnalyzeException(message, e);
      }
    }).toArray(size -> new Class[size]);
    try {
      // constructor methods
      if (methodName.equals("<init>")) {
        return parentClass.getConstructor(parameterTypes);
      }
      return parentClass.getDeclaredMethod(methodName, parameterTypes);
    } catch (NoSuchMethodException | SecurityException e) {
      final String message = MessageFormat.format("Failed to locate method {0}.{1}({2})",
          parentClass.getName(), methodName,
          Stream.of(parameterTypes).map(t -> t.getName()).collect(Collectors.joining(", ")));
      throw new AnalyzeException(message, e);
    }
  }

  /**
   * Finds and returns the method with the given name and given argument types on the given
   * {@code source} Object.
   * 
   * @param fieldName the name of the field to find
   * @param source the source (class instance) on which the method should be found, or {@code null}
   *        if the method to find is static
   * @return the {@link Method}
   * @throws NoSuchFieldException if no field with the given name could be find
   */
  public static Field getFieldToInvoke(final Object source, final String fieldName)
      throws NoSuchFieldException {
    if (source instanceof Class) {
      return getFieldToInvoke((Class<?>) source, fieldName);
    }
    return getFieldToInvoke(source.getClass(), fieldName);
  }

  // FIXME: we should cover all cases: method in superclass and all variants of superclass of
  // any/all arguments.
  private static Field getFieldToInvoke(final Class<?> clazz, final String fieldName)
      throws NoSuchFieldException {
    return clazz.getField(fieldName);
  }

  /**
   * @param executable the {@link Executable} to analyze.
   * @return the return type if the given {@code executable} is a {@link Method} or the declaring
   *         class it is a Constructor.
   */
  public static Class<?> getReturnType(final Executable executable) {
    if (executable instanceof Constructor) {
      return ((Constructor<?>) executable).getDeclaringClass();
    }
    return ((Method) executable).getReturnType();

  }
}
