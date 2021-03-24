package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * context of the method, keeps track of elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  private final String methodName;
  private TypeProvider methodReturnType;
  // to keep track of element usages
  private final Map<String, ElementContext> elementNames = new HashMap<>();

  public MethodContext(String methodName, TypeProvider methodReturnType) {
    this.methodName = methodName;
    this.methodReturnType = methodReturnType;
  }

  //for tests
  public MethodContext() {
    this("test", null);
  }

  public String getName() {
    return methodName;
  }

  public TypeProvider getReturnType() {
    if(methodReturnType == null) {
      throw new UtamError(String.format("method '%s': return type is not set", methodName));
    }
    return methodReturnType;
  }

  public TypeProvider getReturnType(TypeProvider defaultReturn) {
    if(methodReturnType == null) {
      methodReturnType = defaultReturn;
    }
    return getReturnType();
  }

  public TypeProvider getReturnType(List<ComposeMethodStatement> statements, TypeProvider defaultReturn) {
    //if return type not set in JSON, get one from last statement
    if(methodReturnType == null) {
      methodReturnType = statements.get(statements.size() - 1).getReturnType(defaultReturn);
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
