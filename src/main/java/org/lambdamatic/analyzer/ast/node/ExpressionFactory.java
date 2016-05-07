/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast.node;

import java.lang.reflect.Field;

import org.lambdamatic.analyzer.exception.AnalyzeException;
import org.lambdamatic.analyzer.utils.ClassUtils;

/**
 * {@link Expression} factory.
 * 
 * @author Xavier Coulon
 *
 */
public class ExpressionFactory {

  /**
   * Private constructor of the utility class.
   */
  private ExpressionFactory() {}

  /**
   * Converts the given {@code value} to an {@link Expression}.
   * 
   * @param value the value to convert into an expression.
   * @return the {@link Expression} wrapping the given value.
   */
  public static Expression getExpression(final Object value) {
    if (value instanceof Expression) {
      return (Expression) value;
    } else if (value == null) {
      return new NullLiteral();
    } else if (value instanceof Boolean) {
      return new BooleanLiteral((Boolean) value);
    } else if (value instanceof Character) {
      return new CharacterLiteral((Character) value);
    } else if (value instanceof Number) {
      return new NumberLiteral((Number) value, true);
    } else if (value instanceof Enum<?>) {
      return new EnumLiteral((Enum<?>) value);
    } else if (value instanceof String) {
      return new StringLiteral(value.toString());
    } else if (value instanceof Field) {
      final Field field = (Field) value;
      return new FieldAccess(new ClassLiteral(field.getDeclaringClass()), field.getName());
    } else if (value.getClass().isArray()) {
      final Class<?> componentType = value.getClass().getComponentType();
      // value is already an array of Expression, just need to wrap it in an ArrayVariable
      if (Expression.class.isAssignableFrom(componentType)) {
        return new ArrayVariable(componentType, (Expression[]) value);
      }
      // wrap each element in an expression and add it into an ArrayVariable
      final Object[] values = (Object[]) value;
      final ArrayVariable arrayVariable = new ArrayVariable(componentType, values.length);
      for (int i = 0; i < values.length; i++) {
        arrayVariable.setElement(i, getExpression(values[i]));
      }
      return arrayVariable;
    }
    return new ObjectInstance(value);
  }

  /**
   * Converts the given {@code numberLiteral} to a literal {@link Expression} matching the given
   * targetTypeName
   * 
   * @param numberLiteral the {@link Expression} to convert.
   * @param targetTypeName the name of the target type
   * @return the literal {@link Expression} converted in the given type.
   */
  public static Expression getLiteral(final NumberLiteral numberLiteral,
      final String targetTypeName) {
    final Number value = numberLiteral.getValue();
    try {
      final Class<?> targetClass = ClassUtils.getClass(targetTypeName);
      if (targetClass.equals(Character.class) || targetClass.equals(char.class)) {
        return new CharacterLiteral((char) value.intValue());
      } else if (targetClass.equals(boolean.class) || targetClass.equals(Boolean.class)) {
        if (value.intValue() == 0) {
          return new BooleanLiteral(false);
        }
        return new BooleanLiteral(true);
      }
      return numberLiteral;
    } catch (final ClassNotFoundException e) {
      throw new AnalyzeException("Failed to retrieve class with name " + targetTypeName, e);
    }
  }

}
