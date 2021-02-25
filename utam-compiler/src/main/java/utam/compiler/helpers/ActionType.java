package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;

import java.util.List;

/**
 * interface for action types
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ActionType {

    TypeProvider getReturnType();

    List<TypeProvider> getParametersTypes();

    boolean isListAction();

    String getApplyString();

    String getInvokeMethodName();
}
