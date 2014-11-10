/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 ******************************************************************************/

package org.lambdamatic.analyzer.ast;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;
import org.lambdamatic.analyzer.ast.node.CapturedArgument;
import org.lambdamatic.analyzer.ast.node.FieldAccess;
import org.lambdamatic.analyzer.ast.node.InfixExpression;
import org.lambdamatic.analyzer.ast.node.InfixExpression.InfixOperator;
import org.lambdamatic.analyzer.ast.node.LocalVariable;
import org.lambdamatic.analyzer.ast.node.MethodInvocation;
import org.lambdamatic.analyzer.ast.node.StringLiteral;

import com.sample.model.TestPojo;
import com.sample.model.User;

/**
 * @author Xavier Coulon <xcoulon@redhat.com>
 *
 */
public class ExpressionRewriterTest {

	@Test
	public void shouldSubstituteOneCapturedArgumentInMethodInvocation() {
		// given
		final LocalVariable user = new LocalVariable("u", User.class);
		final MethodInvocation getStringValueMethod = new MethodInvocation(new CapturedArgument(new TestPojo()), "getStringValue");
		final MethodInvocation equalsGetStringValue = new MethodInvocation(user, "equals", getStringValueMethod);
		// when
		final ExpressionRewriter expressionRewriter = new ExpressionRewriter();
		equalsGetStringValue.accept(expressionRewriter);
		// then
		final MethodInvocation expectedResult = new MethodInvocation(user, "equals", new StringLiteral("foo"));
		assertThat(equalsGetStringValue).isEqualTo(expectedResult);
	}

	@Test
	public void shouldSubstituteCapturedArgumentInTwoMethodInvocations() {
		// given
		final LocalVariable user = new LocalVariable("u", User.class);
		final MethodInvocation equalsMethodInvocation = new MethodInvocation(user, "equals", new MethodInvocation(new CapturedArgument(new TestPojo()), "getStringValue"));
		final MethodInvocation equalsFieldAccess = new MethodInvocation(user, "equals", new FieldAccess(new CapturedArgument(new TestPojo()), "field"));
		final InfixExpression expression = new InfixExpression(InfixOperator.CONDITIONAL_OR, equalsMethodInvocation, equalsFieldAccess);
		// when
		final ExpressionRewriter expressionRewriter = new ExpressionRewriter();
		expression.accept(expressionRewriter);
		// then
		final InfixExpression expectedResult = new InfixExpression(InfixOperator.CONDITIONAL_OR, new MethodInvocation(user, "equals",
				new StringLiteral("foo")), new MethodInvocation(user, "equals", new StringLiteral("bar")));
		assertThat(expression).isEqualTo(expectedResult);
	}

	@Test
	public void shouldNotSubstituteInfixExpressionOperands() {
		// given
		final LocalVariable testPojo = new LocalVariable("t", TestPojo.class);
		final MethodInvocation equalsFooMethod = new MethodInvocation(testPojo, "equals", new StringLiteral("foo"));
		// when
		final ExpressionRewriter expressionRewriter = new ExpressionRewriter();
		equalsFooMethod.accept(expressionRewriter);
		// then
		assertThat(equalsFooMethod).isEqualTo(equalsFooMethod);
	}
}

