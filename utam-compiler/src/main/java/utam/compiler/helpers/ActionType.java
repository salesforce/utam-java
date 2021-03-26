package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;

import java.util.List;

/**
 * action applied to an element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ActionType {

    /**
     * type returned by the action
     *
     * @return return type
     */
    TypeProvider getReturnType();

    /**
     * list of parameters types
     *
     * @return list
     */
    List<TypeProvider> getParametersTypes();

    /**
     * value of "apply" property in JSON, ex. "click"
     *
     * @return string with apply property
     */
    String getApplyString();

    /**
     * because "getClass()" is reserved method in Java, for this method invoked method will be getClassAttribute();
     * in other cases method name is same as "apply" property value
     *
     * @return string with method name to invoke
     */
    default String getInvokeMethodName() {
        return this.getApplyString();
    }
}
