/**
 * 
 */
package org.lambdamatic.analyzer.ast;

import java.lang.invoke.SerializedLambda;
import java.util.ArrayList;
import java.util.List;

import org.lambdamatic.analyzer.ast.node.CapturedArgumentRef;
import org.objectweb.asm.Type;

/**
 * Info about the Lambda Expression location.
 * @author Xavier Coulon <xcoulon@redhat.com>
 *
 * @see SerializedLambda
 */
public class SerializedLambdaInfo {

	/** the fully qualified name of the implementation class. */
	private final String implClassName;

	/** the name of the implementation method. */
	private final String implMethodName;

	/** the signature of the implementation method. */
	private final String implMethodDesc;
	
	/** the (potentially empty) list of {@link CapturedArgumentRef}. */
	private final List<CapturedArgumentRef> capturedArgs;

	/**
	 * Full constructor
	 * @param serializedLambda the fully {@link SerializedLambda} carrying all the required info.
	 */
	public SerializedLambdaInfo(final SerializedLambda serializedLambda) {
		super();
		this.implClassName = Type.getObjectType(serializedLambda.getImplClass()).getClassName();
		this.implMethodName = serializedLambda.getImplMethodName();
		this.implMethodDesc = serializedLambda.getImplMethodSignature();
		this.capturedArgs = new ArrayList<>();
		for (int i = 0; i < serializedLambda.getCapturedArgCount(); i++) {
			this.capturedArgs.add(new CapturedArgumentRef(i, serializedLambda.getCapturedArg(i)));
		}
	}

	/**
	 * Full constructor
	 * @param implClassName the fully qualified name of the implementation class.
	 * @param implMethodName the name of the implementation method.
	 * @param implMethodDesc the signature of the implementation method.
	 * @param capturedArgs the (potentially empty) list of {@link CapturedArgumentRef}.
	 */
	public SerializedLambdaInfo(final String implClassName, final String implMethodName, final String implMethodDesc,
			final List<CapturedArgumentRef> capturedArgs) {
		super();
		this.implClassName = Type.getObjectType(implClassName).getClassName();
		this.implMethodName = implMethodName;
		this.implMethodDesc = implMethodDesc;
		this.capturedArgs = capturedArgs;
	}

	/**
	 * @return the fully qualified name of the implementation class.
	 */
	public String getImplClassName() {
		return implClassName;
	}

	/**
	 * @return the name of the implementation method.
	 */
	public String getImplMethodName() {
		return implMethodName;
	}

	/**
	 * @return the signature of the implementation method.
	 */
	public String getImplMethodDesc() {
		return implMethodDesc;
	}

	/**
	 * @return the (potentially empty) list of {@link CapturedArgumentRef}.
	 */
	public List<CapturedArgumentRef> getCapturedArgs() {
		return capturedArgs;
	}

	/**
	 * @return the fully qualified location of the Lambda Expression implementation.
	 */
	public String getImplMethodId() {
		return this.implClassName + "." + this.implMethodName + "(" + this.implMethodDesc + ")";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((implClassName == null) ? 0 : implClassName.hashCode());
		result = prime * result + ((implMethodDesc == null) ? 0 : implMethodDesc.hashCode());
		result = prime * result + ((implMethodName == null) ? 0 : implMethodName.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SerializedLambdaInfo other = (SerializedLambdaInfo) obj;
		if (implClassName == null) {
			if (other.implClassName != null)
				return false;
		} else if (!implClassName.equals(other.implClassName))
			return false;
		if (implMethodDesc == null) {
			if (other.implMethodDesc != null)
				return false;
		} else if (!implMethodDesc.equals(other.implMethodDesc))
			return false;
		if (implMethodName == null) {
			if (other.implMethodName != null)
				return false;
		} else if (!implMethodName.equals(other.implMethodName))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "SerializedLambdaInfo [implClassName=" + implClassName + ", implMethodName=" + implMethodName
				+ ", implMethodDesc=" + implMethodDesc + ", capturedArgs=" + capturedArgs + "]";
	}
	
}
