/**
 * 
 */
package org.lambdamatic.analyzer.ast;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.lambdamatic.analyzer.exception.AnalyzeException;

/**
 * Utility class to around Reflection API
 * 
 * @author Xavier Coulon <xcoulon@redhat.com>
 *
 */
public class ReflectionUtils {

	/**
	 * Finds and returns the closest matching method with the given name and given argument types on the given
	 * {@code source} Object
	 * 
	 * @param source
	 *            the Class instance on which the method should be found
	 * @param methodName
	 *            the name of the declared method to find
	 * @param argTypes
	 *            the argument types
	 * @return the {@link Method}
	 * @throws AnalyzeException
	 *             if no method matched
	 */
	public static Method findJavaMethod(final Object source, final String methodName, final Class<?>... argTypes) {
		if (source instanceof Class) {
			return findJavaMethod((Class<?>) source, methodName, argTypes);
		}
		return findJavaMethod(source.getClass(), methodName, argTypes);
	}

	/**
	 * Finds and returns the closest matching method with the given name and given argument types on the given
	 * {@code sourceClass} {@link Class}
	 * 
	 * @param sourceClass
	 *            the Class on which the method should be found
	 * @param methodName
	 *            the name of the declared method to find
	 * @param argTypes
	 *            the argument types
	 * @return the {@link Method}
	 * @throws AnalyzeException
	 *             if no method matched
	 */
	public static Method findJavaMethod(final Class<?> sourceClass, final String methodName, final Class<?>... argTypes) {
		methods_loop: for (Method method : sourceClass.getMethods()) {
			if (!method.getName().equals(methodName) || method.getParameterTypes().length != argTypes.length) {
				continue methods_loop;
			}
			// perfect match or check if superclass/superinterfaces of the given parameter types could match
			for (int i = 0; i < argTypes.length; i++) {
				final Class<?> givenParameterType = argTypes[i];
				final Class<?> methodParameterType = method.getParameterTypes()[i];
				final boolean isSameType = methodParameterType.equals(givenParameterType);
				final boolean isSubType = methodParameterType.isAssignableFrom(givenParameterType);
				final boolean isMatchingVarArg = (i == argTypes.length - 1)
						&& methodParameterType.isArray()
						&& (methodParameterType.getComponentType().equals(givenParameterType) || methodParameterType
								.getComponentType().isAssignableFrom(givenParameterType));
				if (isSameType && !isSubType && !isMatchingVarArg) {
					continue methods_loop;
				}
			}
			// the method matches
			return method;
		}
		throw new AnalyzeException("Could not find a method named '" + methodName + "' in class "
				+ sourceClass.getName() + " with parameters matching "
				+ String.join(", ", Arrays.asList(argTypes).stream().map(Class::getName).collect(Collectors.toList())));
	}

	/**
	 * Finds and returns the method with the given name and given argument types on the given {@code source} Object
	 * 
	 * @param methodName
	 *            the name of the declared method to find
	 * @param source
	 *            the source (class instance) on which the method should be found, or {@code null} if the method to find
	 *            is static
	 * @param argTypes
	 *            the argument types
	 * @return the {@link Method}
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws NoSuchFieldException
	 */
	public static Field getFieldToInvoke(final Object source, final String fieldName) throws NoSuchFieldException,
			SecurityException {
		if (source instanceof Class) {
			return getFieldToInvoke((Class<?>) source, fieldName);
		}
		return getFieldToInvoke(source.getClass(), fieldName);
	}

	// FIXME: we should cover all cases: method in superclass and all variants of superclass of any/all arguments.
	private static Field getFieldToInvoke(final Class<?> clazz, final String fieldName) throws NoSuchFieldException,
			SecurityException {
		return clazz.getField(fieldName);
	}

	public static <T> T[] cast(final Object value, final Class<T> componentType) {
		final Object[] values = (Object[]) value;
		@SuppressWarnings("unchecked")
		final T[] array = (T[]) Array.newInstance(componentType, values.length);
		System.arraycopy(values, 0, array, 0, values.length);
		return array;
	}

}
