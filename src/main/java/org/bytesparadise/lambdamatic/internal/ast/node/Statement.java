/**
 * 
 */
package org.bytesparadise.lambdamatic.internal.ast.node;

/**
 * Abstract base class of AST nodes that represent (bytecode) statements.
 *
 * @author xcoulon
 *
 */
public abstract class Statement extends ASTNode {

	public enum StatementType {
		EXPRESSION_STMT, IF_STMT, RETURN_STMT; 
	}

	/** The parent statement in the AST, or null if this statement is the root of the AST.*/
	private Statement parent;

	public abstract StatementType getStatementType();

	/**
	 * Sets the parent of this statement in the AST.
	 * @param parent the parent statement
	 */
	void setParent(final Statement parent) {
		this.parent = parent;
	}
	
	/**
	 * @return the parent statement in the AST, or null if this statement is the root.
	 */
	public Statement getParent() {
		return parent;
	}
	
	/**
	 * The given visitor is notified of the type of {@link Statement} is it currently visiting.
	 * The visitor may also accept to visit the children {@link Statement} of this node.
	 * @param visitor the {@link Statement} visitor
	 */
	public void accept(final StatementVisitor visitor) {
		visitor.visit(this);
	}

	
}
