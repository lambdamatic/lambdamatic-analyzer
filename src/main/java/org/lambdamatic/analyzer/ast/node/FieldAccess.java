/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast.node;

import org.lambdamatic.analyzer.exception.AnalyzeException;

/**
 * A field access: {@code source.fieldName}
 * 
 * @author xcoulon
 *
 */
public class FieldAccess extends ComplexExpression {

  /** the Expression on containing the field to access (may change if it is evaluated). */
  private Expression source;

  /** the name of the accessed field. */
  private final String fieldName;

  private final Class<?> javaType;

  /**
   * Full constructor
   * <p>
   * Note: the synthetic {@code id} is generated and the inversion flag is set to {@code false}.
   * </p>
   * 
   * @param sourceExpression the source containing the field to access
   * @param fieldName the name of the accessed field
   * @param javaType the type of the fieldType 
   */
  public FieldAccess(final Expression sourceExpression, final String fieldName, final Class<?> javaType) {
    this(sourceExpression, fieldName, javaType, false);
  }

  /**
   * Full constructor
   * <p>
   * Note: the synthetic {@code id} is generated and the inversion flag is set to {@code false}.
   * </p>
   * 
   * @param sourceExpression the source containing the field to access
   * @param fieldName the name of the accessed field
   * @param javaType the type of the field 
   * @param inverted if this {@link FieldAccess} expression is inverted
   */
  public FieldAccess(final Expression sourceExpression, final String fieldName, final Class<?> javaType,
      final boolean inverted) {
    this(generateId(), sourceExpression, fieldName, javaType, inverted);
  }

  /**
   * Full constructor with given id.
   * 
   * @param id the synthetic id of this {@link Expression}.
   * @param sourceExpression the source {@link Expression} from which the field is accessed.
   * @param fieldName the name of the field to access
   * @param javaType the type of the field
   * @param inverted the inversion flag of this {@link Expression}.
   */
  public FieldAccess(final int id, final Expression sourceExpression, final String fieldName, final Class<?> javaType,
      final boolean inverted) {
    super(id, inverted);
    setSourceExpression(sourceExpression);
    this.fieldName = fieldName;
    this.javaType = javaType;
  }

  private void setSourceExpression(final Expression expression) {
    this.source = expression;
    this.source.setParent(this);
  }

  @Override
  public ComplexExpression getParent() {
    return (ComplexExpression) super.getParent();
  }

  @Override
  public void accept(final ExpressionVisitor visitor) {
    this.source.accept(visitor);
    visitor.visit(this);
  }

  @Override
  public void replaceElement(final Expression oldSourceExpression,
      final Expression newSourceExpression) {
    if (oldSourceExpression == this.source) {
      setSourceExpression(newSourceExpression);
    }
  }

  @Override
  public boolean anyElementMatches(ExpressionType type) {
    return this.source.anyElementMatches(type);
  }

  /**
   * @return the source on containing the field to access.
   */
  public Expression getSource() {
    return this.source;
  }

  @Override
  public FieldAccess duplicate(int id) {
    return new FieldAccess(id, getSource().duplicate(), this.fieldName, this.javaType, isInverted());
  }

  /**
   * @return an inverted instance of the current element, only if the underlying Java type is
   *         {@link Boolean}.
   */
  @Override
  public FieldAccess inverse() {
    if (getJavaType() == Boolean.class || getJavaType() == boolean.class) {
      return new FieldAccess(this.source, this.fieldName, this.javaType, !isInverted());
    }
    throw new UnsupportedOperationException("Field access on '" + getFieldName()
        + "' with Java type '" + getJavaType() + "' does not support inversion.");
  }

  @Override
  public ExpressionType getExpressionType() {
    return ExpressionType.FIELD_ACCESS;
  }

  @Override
  public Class<?> getJavaType() {
     return this.javaType;
  }

  /**
   * @return the fieldName.
   */
  public String getFieldName() {
    return this.fieldName;
  }

  /**
   * Returns the underlying Java field.
   */
  @Override
  public Object getValue() {
    try {
      return this.source.getJavaType().getField(getFieldName());
    } catch (NoSuchFieldException | SecurityException e) {
      throw new AnalyzeException("Cannot retrieve field named '" + this.fieldName + "' on class "
          + this.source.getJavaType(), e);
    }
  }

  @Override
  public boolean canBeInverted() {
    return false;
  }

  @Override
  public String toString() {
    return this.source.toString() + "." + this.fieldName;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((fieldName == null) ? 0 : fieldName.hashCode());
    result = prime * result + ((javaType == null) ? 0 : javaType.hashCode());
    result = prime * result + ((source == null) ? 0 : source.hashCode());
    return result;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof FieldAccess)) {
      return false;
    }
    FieldAccess other = (FieldAccess) obj;
    if (fieldName == null) {
      if (other.fieldName != null) {
        return false;
      }
    } else if (!fieldName.equals(other.fieldName)) {
      return false;
    }
    if (javaType == null) {
      if (other.javaType != null) {
        return false;
      }
    } else if (!javaType.equals(other.javaType)) {
      return false;
    }
    if (source == null) {
      if (other.source != null) {
        return false;
      }
    } else if (!source.equals(other.source)) {
      return false;
    }
    return true;
  }

}