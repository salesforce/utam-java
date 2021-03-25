package utam.compiler.helpers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.TypeProvider;

/**
 * context of the method, keeps track of elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  private final String methodName;
  // to keep track of element usages
  private final Map<String, ElementContext> elementNames = new HashMap<>();
  private final TypeProvider methodReturnType;

  public MethodContext(String methodName, TypeProvider methodReturnType) {
    this.methodName = methodName;
    this.methodReturnType = methodReturnType;
  }

  public String getName() {
    return methodName;
  }

  public TypeProvider getReturnType(TypeProvider defaultReturn) {
    if (methodReturnType == null) {
      return defaultReturn;
    }
    return methodReturnType;
  }

  public TypeProvider getReturnType(List<ComposeMethodStatement> statements,
      TypeProvider defaultReturn) {
    //if return type not set in JSON, get one from last statement
    if (methodReturnType == null) {
      ComposeMethodStatement lastStatement = statements.get(statements.size() - 1);
      return lastStatement.getReturnType(defaultReturn);
    }
    return getReturnType(defaultReturn);
  }

  public boolean hasElement(String name) {
    return elementNames.containsKey(name);
  }

  public void setElementUsage(ElementContext context) {
    elementNames.put(context.getName(), context);
  }
}
