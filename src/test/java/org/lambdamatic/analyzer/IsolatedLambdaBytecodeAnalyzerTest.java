/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/
package org.lambdamatic.analyzer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.lambdamatic.testutils.JavaMethods.Date_getTime;
import static org.lambdamatic.testutils.JavaMethods.Object_equals;
import static org.lambdamatic.testutils.JavaMethods.Object_notify;

import java.io.IOException;

import org.junit.Test;
import org.lambdamatic.SerializableConsumer;
import org.lambdamatic.analyzer.ast.node.Expression;
import org.lambdamatic.analyzer.ast.node.ExpressionStatement;
import org.lambdamatic.analyzer.ast.node.FieldAccess;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.LocalVariable;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.StringLiteral;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sample.model.EmbeddedTestPojo;
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
    final SerializableConsumer<TestPojo> expression = (SerializableConsumer<TestPojo>) (t -> {
      t.field.notify();
      t.dateValue.getTime();
    });
    // when
    final LambdaExpression resultExpression = analyzer.analyzeExpression(expression);
    // then
    final LocalVariable testPojo = new LocalVariable(0, "t", TestPojo.class);
    final FieldAccess e_dot_field = new FieldAccess(testPojo, "field");
    final MethodInvocation e_dot_field_dot_notify =
        new MethodInvocation(e_dot_field, Object_notify);
    final FieldAccess e_dot_dateValue = new FieldAccess(testPojo, "dateValue");
    final MethodInvocation e_dot_dateValue_dot_getTime =
        new MethodInvocation(e_dot_dateValue, Date_getTime);
    // verification
    LOGGER.info("Result: {}", resultExpression);
    assertThat(resultExpression.getBody()).containsExactly(
        new ExpressionStatement(e_dot_field_dot_notify),
        new ExpressionStatement(e_dot_dateValue_dot_getTime));
//    final ExpressionStatement statement = (ExpressionStatement) resultExpression.getBody().get(0);
//    final MethodInvocation elementListGetFirstMethodInvocation =
//        (MethodInvocation) statement.getExpression();
//    final FieldAccess fieldAccess = (FieldAccess) elementListGetFirstMethodInvocation.getSource();
//    assertThat(((MethodInvocation) (fieldAccess.getSource())).getReturnType())
//        .isEqualTo(EmbeddedTestPojo.class);

  }
}

