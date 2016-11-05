/*******************************************************************************
 * Copyright (c) 2015 Red Hat. All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which accompanies this
 * distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat - Initial Contribution
 *******************************************************************************/
package org.lambdamatic.analyzer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.lambdamatic.testutils.JavaMethods.ArrayUtil_toArray;
import static org.lambdamatic.testutils.JavaMethods.Date_getTime;
import static org.lambdamatic.testutils.JavaMethods.Object_equals;
import static org.lambdamatic.testutils.JavaMethods.Object_notify;
import static org.lambdamatic.testutils.JavaMethods.TestPojo_elementMatch;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.assertj.core.util.Arrays;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.lambdamatic.SerializableConsumer;
import org.lambdamatic.analyzer.ast.node.ArrayVariable;
import org.lambdamatic.analyzer.ast.node.Assignment;
import org.lambdamatic.analyzer.ast.node.CapturedArgument;
import org.lambdamatic.analyzer.ast.node.ClassLiteral;
import org.lambdamatic.analyzer.ast.node.Expression;
import org.lambdamatic.analyzer.ast.node.ExpressionStatement;
import org.lambdamatic.analyzer.ast.node.FieldAccess;
import org.lambdamatic.analyzer.ast.node.LambdaExpression;
import org.lambdamatic.analyzer.ast.node.LocalVariable;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.MethodReference;
import org.lambdamatic.analyzer.ast.node.NumberLiteral;
import org.lambdamatic.analyzer.ast.node.ObjectInstance;
import org.lambdamatic.analyzer.ast.node.Operation;
import org.lambdamatic.analyzer.ast.node.Operation.Operator;
import org.lambdamatic.analyzer.ast.node.ReturnStatement;
import org.lambdamatic.analyzer.ast.node.Statement;
import org.lambdamatic.analyzer.ast.node.StringLiteral;
import org.lambdamatic.testutils.JavaMethods;
import org.lambdamatic.testutils.TestWatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sample.model.EmbeddedTestPojo;
import com.sample.model.TestInterface;
import com.sample.model.TestPojo;

/**
 * Parameterized tests with many use cases of comparison.
 * 
 * @author Xavier Coulon
 */
@RunWith(Parameterized.class)
public class SerializableConsumerExpressionBytecodeAnalyzerTest {

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
  private static Object[] matchOnTestPojo(final SerializableConsumer<TestPojo> lambdaExpression,
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
  private static Object[] matchOnTestPojo(final SerializableConsumer<TestPojo> lambdaExpression,
      final Expression[] expressions) {
    return new Object[] {lambdaExpression, expressions};
  }

  /**
   * Utility method that makes the JUnit parameters declaration much more readable.
   * 
   * @param lambdaExpression the {@link FunctionalInterface} to encode
   * @param expression the expected result
   * @return an array containing the 2 arguments.
   */
  private static Object[] matchOnTestInterface(
      final SerializableConsumer<TestInterface> lambdaExpression, final Expression expression) {
    return new Object[] {lambdaExpression, expression};
  }

  /**
   * @return the dataset to use for the tests.
   * @throws SecurityException
   * @throws NoSuchMethodException
   */
  @Parameters(name = "[{index}] expect {1}")
  public static Object[][] data() throws NoSuchMethodException, SecurityException {
    final String foo = "foo";
    final TestPojo anotherPojo = new TestPojo();
    final LocalVariable testPojo = new LocalVariable(0, "t", TestPojo.class);
    final FieldAccess testPojo_dot_stringValue =
        new FieldAccess(testPojo, "stringValue", String.class);
    final FieldAccess testPojo_dot_primitiveIntValue =
        new FieldAccess(testPojo, "primitiveIntValue", int.class);
    final FieldAccess testPojo_dot_field = new FieldAccess(testPojo, "field", String.class);
    final FieldAccess testPojo_dot_dateValue = new FieldAccess(testPojo, "dateValue", Date.class);
    final FieldAccess testPojo_dot_elementList =
        new FieldAccess(testPojo, "elementList", List.class);
    final FieldAccess e_dot_field =
        new FieldAccess(new LocalVariable(0, "e", TestPojo.class), "field", String.class);
    final LambdaExpression e_dot_field_equals_foo = new LambdaExpression(
        new ReturnStatement(
            new MethodInvocation(e_dot_field, Object_equals, new StringLiteral("foo"))),
        TestPojo.class, "e");
    final EmbeddedTestPojo embeddedTestPojo = new EmbeddedTestPojo();
    final MethodInvocation e_dot_field_dot_notify =
        new MethodInvocation(e_dot_field, Object_notify);
    final FieldAccess e_dot_dateValue = new FieldAccess(testPojo, "dateValue", Date.class);
    final MethodInvocation e_dot_dateValue_dot_getTime =
        new MethodInvocation(e_dot_dateValue, Date_getTime);

    return new Object[][] {
        matchOnTestPojo(t -> ArrayUtil.toArray(t.stringValue, t.dateValue),
            new MethodInvocation(new ClassLiteral(ArrayUtil.class), ArrayUtil_toArray,
                new ArrayVariable(Object[].class,
                    new FieldAccess(testPojo, "stringValue", String.class),
                    new FieldAccess(testPojo, "dateValue", Date.class)))),
        matchOnTestPojo(t -> ArrayUtil.toArray(t.field),
            new MethodInvocation(new ClassLiteral(ArrayUtil.class), ArrayUtil_toArray,
                new ArrayVariable(Object[].class, testPojo_dot_field))),
        matchOnTestPojo(t -> ArrayUtil.toArray(t.field, t.dateValue, t.elementList),
            new MethodInvocation(new ClassLiteral(ArrayUtil.class), ArrayUtil_toArray,
                new ArrayVariable(Object[].class, testPojo_dot_field, testPojo_dot_dateValue,
                    testPojo_dot_elementList))),
        // Verify nested Lambda expression
        matchOnTestPojo(t -> t.elementMatch(e -> e.field.equals("foo")),
            new MethodInvocation(testPojo, TestPojo_elementMatch, e_dot_field_equals_foo)),
        matchOnTestPojo(t -> t.elementMatch(e -> e.field.equals(foo)),
            new MethodInvocation(testPojo, TestPojo_elementMatch, e_dot_field_equals_foo)),
        matchOnTestPojo(t -> t.elementMatch(e -> e.field.equals(anotherPojo.getStringValue())),
            new MethodInvocation(testPojo, TestPojo_elementMatch, e_dot_field_equals_foo)),
        matchOnTestPojo(t -> t.elementMatch(e -> e.field.equals(anotherPojo.stringValue)),
            new MethodInvocation(testPojo, TestPojo_elementMatch, e_dot_field_equals_foo)),
        // verify assignation statement(s)
        matchOnTestPojo(t -> {
          t.stringValue = "foo";
        }, new Assignment(testPojo_dot_stringValue, new StringLiteral("foo"))),
        matchOnTestPojo(t -> {
          t.stringValue = "foo";
          t.field = "baz";
        }, Arrays.array(new Assignment(testPojo_dot_stringValue, new StringLiteral("foo")),
            new Assignment(testPojo_dot_field, new StringLiteral("baz")))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue++;
        }, new Assignment(testPojo_dot_primitiveIntValue,
            new Operation(Operator.ADD, testPojo_dot_primitiveIntValue, new NumberLiteral(1)))),

        matchOnTestPojo(t -> {
          t.primitiveIntValue += 2;
        }, new Assignment(testPojo_dot_primitiveIntValue,
            new Operation(Operator.ADD, testPojo_dot_primitiveIntValue, new NumberLiteral(2)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue = t.primitiveIntValue + 3;
        }, new Assignment(testPojo_dot_primitiveIntValue,
            new Operation(Operator.ADD, testPojo_dot_primitiveIntValue, new NumberLiteral(3)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue--;
        }, new Assignment(testPojo_dot_primitiveIntValue, new Operation(Operator.SUBTRACT,
            testPojo_dot_primitiveIntValue, new NumberLiteral(1)))),

        matchOnTestPojo(t -> {
          t.primitiveIntValue -= 2;
        }, new Assignment(testPojo_dot_primitiveIntValue, new Operation(Operator.SUBTRACT,
            testPojo_dot_primitiveIntValue, new NumberLiteral(2)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue = t.primitiveIntValue - 3;
        }, new Assignment(testPojo_dot_primitiveIntValue, new Operation(Operator.SUBTRACT,
            testPojo_dot_primitiveIntValue, new NumberLiteral(3)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue *= 2;
        }, new Assignment(testPojo_dot_primitiveIntValue, new Operation(Operator.MULTIPLY,
            testPojo_dot_primitiveIntValue, new NumberLiteral(2)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue = t.primitiveIntValue * 3;
        }, new Assignment(testPojo_dot_primitiveIntValue, new Operation(Operator.MULTIPLY,
            testPojo_dot_primitiveIntValue, new NumberLiteral(3)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue /= 2;
        }, new Assignment(testPojo_dot_primitiveIntValue,
            new Operation(Operator.DIVIDE, testPojo_dot_primitiveIntValue, new NumberLiteral(2)))),
        matchOnTestPojo(t -> {
          t.primitiveIntValue = t.primitiveIntValue / 3;
        }, new Assignment(testPojo_dot_primitiveIntValue,
            new Operation(Operator.DIVIDE, testPojo_dot_primitiveIntValue, new NumberLiteral(3)))),

        matchOnTestPojo(t -> {
          t.stringValue = "foo";
          t.primitiveIntValue++;
          t.elementList.add(embeddedTestPojo);
        }, Arrays.array(new Assignment(testPojo_dot_stringValue, new StringLiteral("foo")),
            new Assignment(testPojo_dot_primitiveIntValue,
                new Operation(Operator.ADD, testPojo_dot_primitiveIntValue, new NumberLiteral(1))),
            new MethodInvocation(testPojo_dot_elementList, JavaMethods.List_add,
                new CapturedArgument(embeddedTestPojo)))),
        matchOnTestPojo(t -> {
          t.field.notify();
          t.dateValue.getTime();
        }, Arrays.array(e_dot_field_dot_notify, e_dot_dateValue_dot_getTime)),
        matchOnTestPojo(System.out::println,
            new MethodReference(PrintStream.class,
                System.out.getClass().getMethod("println", Object.class),
                java.util.Arrays.asList(new CapturedArgument(System.out)))),
        matchOnTestPojo(t -> System.out.println("bonjour"),
            new MethodInvocation(new ObjectInstance(System.out),
                System.out.getClass().getMethod("println", String.class),
                new StringLiteral("bonjour"))),
        matchOnTestInterface(TestInterface::execute,
            new MethodReference(TestInterface.class,
                TestInterface.class.getMethod("execute"), Collections.emptyList())),};
  }

  @Parameter(value = 0)
  public SerializableConsumer<TestPojo> expression;

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
          .containsExactly(new ExpressionStatement(expectationExpression));
    } else if (expectation instanceof List) {
      final List<Statement> expectationStatements = new ArrayList<>();
      for (Expression expression : (List<Expression>) expectation) {
        expectationStatements.add(new ExpressionStatement(expression));
      }
      assertThat(resultExpression.getBody())
          .containsExactly(expectationStatements.toArray(new Statement[0]));
    }
  }

}
