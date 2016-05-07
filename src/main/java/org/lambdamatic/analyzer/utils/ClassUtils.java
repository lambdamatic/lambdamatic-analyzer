/*******************************************************************************
 * Copyright (c) 2016 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.utils;

import java.util.stream.IntStream;

/**
 * Utility class for {@link Class} objects.
 */
public class ClassUtils {

  /**
   * Private constructor for this utility class.
   */
  private ClassUtils() {
    // does nothing
  }

  /**
   * Loads the class from the given {@code className}.
   * 
   * @param className the fully qualified name of the class to load
   * @return the matching {@link Class}
   * @throws ClassNotFoundException if the class was not found
   */
  public static Class<?> getClass(final String className) throws ClassNotFoundException {
    switch (className) {
      case "boolean":
        return boolean.class;
      case "byte":
        return byte.class;
      case "char":
        return char.class;
      case "short":
        return short.class;
      case "int":
        return int.class;
      case "long":
        return long.class;
      case "double":
        return double.class;
      case "float":
        return float.class;
      default:
        return Class.forName(toJLSClassName(className), true,
            Thread.currentThread().getContextClassLoader());
    }
  }

  /**
   * Converts the given {@code className} to a JLS Class Name. For example:
   * <ul>
   * <li>{@code java.lang.String} becomes {@code Ljava.lang.String;}</li>
   * <li>{@code java.lang.String[]} becomes {@code L[java.lang.String;}</li>
   * <li>{@code java.lang.String[][]} becomes {@code L[[java.lang.String;}</li>
   * <li>etc...</li>
   * </ul>
   * 
   * @param className the class Name to convert
   * @return the normalized class name
   */
  public static String toJLSClassName(final String className) {
    final StringBuilder classNameBuffer = new StringBuilder();
    final IntPair<String> rawClassName = countArraySize(className); 
    if (rawClassName.getRight() > 0) {
      IntStream.range(0, rawClassName.getRight()).forEach(i -> classNameBuffer.append("["));
      classNameBuffer.append("L").append(rawClassName.getLeft()).append(';');
      return classNameBuffer.toString();
    }
    return className;
  }
  
  private static IntPair<String> countArraySize(final String className) {
    if (className.contains("[]")) {
      final String rawClassName = className.replaceFirst("\\[\\]", "");
      final IntPair<String> nestedResult = countArraySize(rawClassName);
      return new IntPair<>(nestedResult.getLeft(), nestedResult.getRight() + 1);
    }
    return new IntPair<>(className, 0);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Boolean} type or a {@code boolean}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Boolean} class or the
   *         primitive {@code boolean} type.
   */
  public static boolean isBoolean(final Class<?> clazz) {
    return clazz.equals(Boolean.class) || clazz.equals(boolean.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Byte} type or a {@code byte}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Byte} class or the primitive
   *         {@code byte} type.
   */
  public static boolean isByte(final Class<?> clazz) {
    return clazz.equals(Byte.class) || clazz.equals(byte.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Short} type or a {@code short}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Short} class or the
   *         primitive {@code short} type.
   */
  public static boolean isShort(final Class<?> clazz) {
    return clazz.equals(Short.class) || clazz.equals(short.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Integer} type or a {@code int}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Integer} class or the
   *         primitive {@code int} type.
   */
  public static boolean isInteger(final Class<?> clazz) {
    return clazz.equals(Integer.class) || clazz.equals(int.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Long} type or a {@code long}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Long} class or the primitive
   *         {@code long} type.
   */
  public static boolean isLong(final Class<?> clazz) {
    return clazz.equals(Long.class) || clazz.equals(long.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Double} type or a {@code double}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Double} class or the
   *         primitive {@code double} type.
   */
  public static boolean isDouble(final Class<?> clazz) {
    return clazz.equals(Double.class) || clazz.equals(double.class);
  }

  /**
   * Checks if the given {@code clazz} corresponds to a {@link Float} type or a {@code float}
   * primitive type.
   * 
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Float} class or the
   *         primitive {@code float} type.
   */
  public static boolean isFloat(final Class<?> clazz) {
    return clazz.equals(Float.class) || clazz.equals(float.class);
  }

}
