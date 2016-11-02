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

package org.lambdamatic.analyzer;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.lambdamatic.SerializableFunction;
import org.lambdamatic.SerializableSupplier;
import org.lambdamatic.analyzer.ast.node.ClassLiteral;
import org.lambdamatic.analyzer.ast.node.Expression;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.MethodReference;
import org.lambdamatic.analyzer.ast.node.ObjectInstance;
import org.lambdamatic.analyzer.ast.node.ObjectInstanciation;
import org.lambdamatic.analyzer.ast.node.ReturnStatement;
import org.lambdamatic.analyzer.ast.node.Statement;
import org.lambdamatic.analyzer.ast.node.StringLiteral;
import org.lambdamatic.testutils.TestWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sample.model.TestPojo;

/**
 * Parameterized tests with many use cases of comparison.
 * 
 * @author Xavier Coulon
 */
@RunWith(Parameterized.class)
public class SerializableSupplierExpressionBytecodeAnalyzerTest {

  private final LambdaExpressionAnalyzer analyzer = LambdaExpressionAnalyzer.getInstance();

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SerializableConsumerExpressionBytecodeAnalyzerTest.class);

  @Rule
  public TestWatcher watcher = new TestWatcher();

  /**
   * Utility method that makes the JUnit parameters declaration much more readable.
   * 
   * @param lambdaExpression the {@link FunctionalInterface} to encode
   * @param expression the expected result
   * @return an array containing the 2 arguments.
   */
  private static Object[] match(final SerializableSupplier<TestPojo> lambdaExpression,
      final Expression expression) {
    return new Object[] {lambdaExpression, expression};
  }

  /**
   * Utility method that makes the JUnit parameters declaration much more readable.
   * 
   * @param lambdaExpression the {@link FunctionalInterface} to encode
   * @param expressions the expected result
   * @return an array containing the 2 arguments.
   */
  private static Object[] match(final SerializableSupplier<TestPojo> lambdaExpression,
      final Expression[] expressions) {
    return new Object[] {lambdaExpression, expressions};
  }

  /**
   * @return the dataset to use for the tests.
   * @throws SecurityException 
   * @throws NoSuchMethodException 
   */
  @Parameters(name = "[{index}] expect {1}")
  public static Object[][] data() throws NoSuchMethodException, SecurityException  {
    return new Object[][] {
      match(TestPojo::new, new MethodReference(new ClassLiteral(TestPojo.class), TestPojo.class.getConstructor(), Collections.emptyList())),
      match(() -> new TestPojo(), new ObjectInstanciation(TestPojo.class)),
    };
  }
  
  @Parameter(value = 0)
  public SerializableSupplier<TestPojo> expression;

  @Parameter(value = 1)
  public Object expectation;

  @SuppressWarnings("unchecked")
  @Test
  public void shouldParseLambdaExpression() throws IOException {
    // when
    final LambdaExpression resultExpression = analyzer.analyzeExpression(expression);
    // then
    LOGGER.info("Result: {}", resultExpression);
    if (expectation instanceof Expression) {
      final Expression expectationExpression = (Expression) expectation;
      assertThat(resultExpression.getBody())
          .containsExactly(new ReturnStatement(expectationExpression));
    } else if (expectation instanceof List) {
      final List<Statement> expectationStatements = new ArrayList<>();
      for (Expression expression : (List<Expression>) expectation) {
        expectationStatements.add(new ReturnStatement(expression));
      }
      assertThat(resultExpression.getBody())
          .containsExactly(expectationStatements.toArray(new Statement[0]));
    }
  }
}
