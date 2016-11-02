/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/
package org.lambdamatic.analyzer;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.util.Collections;

import org.junit.Test;
import org.lambdamatic.SerializableSupplier;
import org.lambdamatic.analyzer.ast.node.ClassLiteral;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.MethodReference;
import org.lambdamatic.analyzer.ast.node.ObjectInstanciation;
import org.lambdamatic.analyzer.ast.node.ReturnStatement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sample.model.TestPojo;

/**
 * Running test in an isolated class to simplify the bytecode reading.
 * 
 * @author Xavier Coulon
 *
 */
public class IsolatedLambdaBytecodeAnalyzerTest {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(IsolatedLambdaBytecodeAnalyzerTest.class);

  private final LambdaExpressionAnalyzer analyzer = LambdaExpressionAnalyzer.getInstance();

  @Test
  public void shouldParseExpression() throws IOException, NoSuchMethodException, SecurityException {
    // given
    final SerializableSupplier<TestPojo> expression =
        (SerializableSupplier<TestPojo>) (TestPojo::new);

    // when
    final LambdaExpression resultExpression = analyzer.analyzeExpression(expression);
    // then
    LOGGER.info("Result: {}", resultExpression);
    assertThat(resultExpression.getBody()).containsExactly(new ReturnStatement(
        new MethodReference(new ClassLiteral(TestPojo.class), TestPojo.class.getConstructor(), Collections.emptyList())));
  }

}

