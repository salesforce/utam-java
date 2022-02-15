package utam.compiler.helpers;

import static utam.compiler.UtamCompilerIntermediateError.getJsonNodeType;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationTypesConfigJava.isPageObjectType;

import com.fasterxml.jackson.databind.JsonNode;
import utam.compiler.UtamCompilerIntermediateError;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper for return properties in a method or in a statement
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public abstract class ReturnType {

  private final boolean isReturnList;

  ReturnType(Boolean isReturnList) {
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
  }

  abstract TypeProvider getReturnType(TranslationContext translationContext);

  /**
   * Gets a value indicating whether a return type is set
   *
   * @return true if the return type is set; otherwise, false
   */
  public abstract boolean isReturnTypeSet();

  /**
   * Gets the return type or a default value
   *
   * @param translatorContext the translation context
   * @param typeProvider      the default type provider
   * @return the return type if defined; otherwise return the default type
   */
  public TypeProvider getReturnTypeOrDefault(TranslationContext translatorContext,
      TypeProvider typeProvider) {
    TypeProvider definedType = getReturnTypeOrNull(translatorContext);
    return definedType == null ? typeProvider : definedType;
  }

  /**
   * Gets the return type or null
   *
   * @param translatorContext the translation context
   * @return the return type if defined; otherwise return null
   */
  public TypeProvider getReturnTypeOrNull(TranslationContext translatorContext) {
    TypeProvider returns = getReturnType(translatorContext);
    if (isReturnList) {
      return wrapAsList(returns);
    }
    return returns;
  }

  /**
   * Gets a value indicating whether to return a list
   *
   * @return true if the return type is a list; otherwise false
   */
  boolean isReturnAllSet() {
    return isReturnList;
  }

  /**
   * return type for a method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class MethodReturnType extends StatementReturnType {

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode     the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName   the method name
     */
    public MethodReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, methodName);
    }

    /**
     * constructor for VOID return type
     *
     * @param methodName the method name
     */
    public MethodReturnType(String methodName) {
      this(null, null, methodName);
    }

    @Override
    RuntimeException getReturnAllError() {
      return new UtamCompilerIntermediateError(returnTypeJsonNode, "UIM002", methodName);
    }

    @Override
    RuntimeException getTypeProcessingError() {
      String value = returnTypeJsonNode == null? "null" : returnTypeJsonNode.toPrettyString();
      return new UtamCompilerIntermediateError(returnTypeJsonNode, "UM001", methodName, value);
    }
  }

  /**
   * basic return type for an abstract method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class MethodBasicReturnType extends ReturnType {

    private final TypeProvider basicReturnType;

    /**
     * this constructor is used for return type of an abstract method when it returns basic type
     *
     * @param returnType   return type (basic union type in this case)
     * @param isReturnList true if return type should be wrapped as a list
     */
    public MethodBasicReturnType(TypeProvider returnType, Boolean isReturnList) {
      super(isReturnList);
      this.basicReturnType = returnType;
    }

    @Override
    TypeProvider getReturnType(TranslationContext translationContext) {
      return basicReturnType;
    }

    @Override
    public boolean isReturnTypeSet() {
      return true;
    }
  }

  /**
   * statement return type for a compose method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class StatementReturnType extends ReturnType {

    final JsonNode returnTypeJsonNode;
    final String methodName;

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode     the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName   the method name
     */
    public StatementReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(isReturnList);
      this.returnTypeJsonNode = typeNode;
      this.methodName = methodName;
      if (isReturnList != null && !isReturnTypeSet()) {
        throw getReturnAllError();
      }
    }

    RuntimeException getReturnAllError() {
      return new UtamCompilerIntermediateError(returnTypeJsonNode, "UMA003", methodName);
    }

    RuntimeException getTypeProcessingError() {
      return new UtamCompilerIntermediateError(returnTypeJsonNode, "UMA002", methodName, returnTypeJsonNode.toPrettyString());
    }

    @Override
    public boolean isReturnTypeSet() {
      return returnTypeJsonNode != null && !returnTypeJsonNode.isNull();
    }

    @Override
    TypeProvider getReturnType(TranslationContext context) {
      if (returnTypeJsonNode == null || returnTypeJsonNode.isNull()) {
        return null;
      }
      if (returnTypeJsonNode.isTextual()) {
        String typeValue = returnTypeJsonNode.textValue();
        if (isPrimitiveType(typeValue)) {
          return PrimitiveType.fromString(typeValue);
        }
        if (isPageObjectType(typeValue)) {
          return context.getType(typeValue);
        }
        if (VOID.getSimpleName().equals(typeValue)) {
          return VOID;
        }
        throw getTypeProcessingError();
      } else {
        String validationContext = String.format("method \"%s\"", methodName);
        throw new UtamCompilerIntermediateError(returnTypeJsonNode, "U0001", validationContext,
            "returnType", getJsonNodeType(returnTypeJsonNode));
      }
    }
  }
}
