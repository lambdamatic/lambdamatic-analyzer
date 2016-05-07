/*******************************************************************************
 * Copyright (c) 2016 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/

package org.lambdamatic.analyzer.utils;

/**
 * Utility class to handle a pair of objects in a single reference, for example, as the result of a
 * method call.
 * @param <L> the type of the left-side value
 */
public class IntPair<L> {

  /** the left-side value. */
  private final L left;
  
  /** the right-side value. */
  private final int right;

  /**
   * Constructor.
   * @param left the left-side value
   * @param right the right-side value
   */
  public IntPair(final L left, final int right) {
    this.left = left;
    this.right = right;
  }

  /**
   * @return the left-side value.
   */
  public L getLeft() {
    return left;
  }

  /**
   * @return the right-side value.
   */
  public int getRight() {
    return right;
  }
  
  
}
