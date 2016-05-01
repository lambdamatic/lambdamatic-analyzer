/*******************************************************************************
 * Copyright (c) 2016 Red Hat.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.utils;

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
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Boolean} class or the
   *         primitive {@code boolean} type.
   */
  public static boolean isBoolean(final Class<?> clazz) {
    return clazz.equals(Boolean.class) || clazz.equals(boolean.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Byte} class or the
   *         primitive {@code boolean} type.
   */
  public static boolean isByte(final Class<?> clazz) {
    return clazz.equals(Byte.class) || clazz.equals(byte.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Short} class or the
   *         primitive {@code short} type.
   */
  public static boolean isShort(final Class<?> clazz) {
    return clazz.equals(Short.class) || clazz.equals(short.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Integer} class or the
   *         primitive {@code int} type.
   */
  public static boolean isInteger(final Class<?> clazz) {
    return clazz.equals(Integer.class) || clazz.equals(int.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Long} class or the
   *         primitive {@code long} type.
   */
  public static boolean isLong(final Class<?> clazz) {
    return clazz.equals(Long.class) || clazz.equals(long.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Double} class or the
   *         primitive {@code double} type.
   */
  public static boolean isDouble(final Class<?> clazz) {
    return clazz.equals(Double.class) || clazz.equals(double.class);
  }
  
  /**
   * @param clazz the {@link Class} to inspect
   * @return <code>true</code> if the given {@code clazz} is the {@link Float} class or the
   *         primitive {@code float} type.
   */
  public static boolean isFloat(final Class<?> clazz) {
    return clazz.equals(Float.class) || clazz.equals(float.class);
  }

}
