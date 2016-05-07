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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class ClassUtilsTest {
  
  /**
   * @return the dataset to use for the tests.
   */
  @Parameters(name = "[{index}] expect {1}")
  public static Object[][] data()  {
    return new Object[][] {
        new Object[] { "java.lang.String", "java.lang.String"},
        new Object[] { "java.lang.String[]", "[Ljava.lang.String;"},
        new Object[] { "java.lang.String[][]", "[[Ljava.lang.String;"}
    };
  }

  @Parameter(0)
  public String input;

  @Parameter(1)
  public String expectation;
  
  @Test
  public void shouldNormalizeToJLSClassName() throws ClassNotFoundException {
    // given
    // when
    final String result = ClassUtils.toJLSClassName(input);
    // then
    assertThat(result).isEqualTo(expectation);
    final Class<?> clazz = Class.forName(result);
    assertThat(clazz).isNotNull();
  }
}
