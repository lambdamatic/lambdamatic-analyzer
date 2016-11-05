/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.ast.node;

/**
 * Variable passed during the call to the Lambda Expression serialized method.
 * 
 * @author Xavier Coulon
 *
 */
public class LocalVariable extends Expression {

  /** The variable index (in the bytecode). */
  private final int index;

  /** The variable name. */
  private final String name;

  /**
   * The variable type's associated {@link Class}.
   */
  private final Class<?> variableType;

  /**
   * Constructor when local variable is provided
   * <p>
   * Note: the synthetic {@code id} is generated and the inversion flag is set to {@code false}.
   * </p>
   * 
   * @param index The variable index (as specified in the method bytecode).
   * @param name The variable name.
   * @param variableType The variable type.
   */
  public LocalVariable(final int index, final String name, final Class<?> variableType) {
    this(generateId(), index, name, variableType, false);
    if (variableType == null) {
      throw new IllegalArgumentException("Type of local variable '" + name + "' must not be null");
    }

  }

  /**
   * Full constructor.
   * 
   * @param id the synthetic id of this {@link Expression}.
   * @param index the variable index (as defined in the bytecode)
   * @param name The variable name
   * @param type the fully qualified type name of the variable.
   * @param inverted the inversion flag of this {@link Expression}.
   */
  public LocalVariable(final int id, final int index, final String name, final Class<?> type,
      final boolean inverted) {
    super(id, inverted);
    this.index = index;
    this.name = name;
    this.variableType = type;
  }

  @Override
  public LocalVariable duplicate(int id) {
    return new LocalVariable(id, this.index, this.name, this.variableType, isInverted());
  }

  /**
   * @return index the variable index (as defined in the bytecode).
   */
  public int getIndex() {
    return this.index;
  }

  /**
   * @return the variable name.
   */
  public String getName() {
    return this.name;
  }

  /**
   * @return the variable type.
   */
  public Class<?> getVariableType() {
    return this.variableType;
  }

  @Override
  public ExpressionType getType() {
    return ExpressionType.LOCAL_VARIABLE;
  }

  @Override
  public ComplexExpression getParent() {
    return (ComplexExpression) super.getParent();
  }

  @Override
  public Class<?> getJavaType() {
    return this.variableType;
  }

  @Override
  public boolean canBeInverted() {
    return false;
  }

  @Override
  public String toString() {
    return this.name;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((getType() == null) ? 0 : getType().hashCode());
    result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
    result = prime * result + ((this.variableType == null) ? 0 : this.variableType.hashCode());
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
    LocalVariable other = (LocalVariable) obj;
    if (this.name == null) {
      if (other.name != null) {
        return false;
      }
    } else if (!this.name.equals(other.name)) {
      return false;
    }
    if (this.variableType == null) {
      if (other.variableType != null) {
        return false;
      }
    } else if (!this.variableType.equals(other.variableType)) {
      return false;
    }
    return true;
  }

}


