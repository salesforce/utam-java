package utam.compiler.helpers;

import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationTypesConfigJava.isPageObjectType;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.function.Function;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper for return properties in a method or in a statement
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class ReturnType {

  static final String ERR_UNSUPPORTED_RETURN_TYPE = "%s: invalid return type '%s'";
  static final String ERR_RETURN_ALL_REDUNDANT =
      "%s: 'returnAll' property can't be set without setting return type";

  private final String validationContextStr;
  private final boolean isReturnList;
  private final boolean isReturnTypeSet;

  private final Function<TranslationContext, TypeProvider> typeProvider;

  /**
   * Initializes a new instance of the ReturnType class
   *
   * @param typeNode     the JSON node of the type
   * @param isReturnList a value indiciating whether the return type is a list
   * @param methodName   the method name
   */
  public ReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
    this.validationContextStr = String.format("method '%s'", methodName);
    this.typeProvider = getTypeProvider(typeNode);
    this.isReturnTypeSet = typeNode != null && !typeNode.isNull();
    if (isReturnList != null && !isReturnTypeSet) {
      throw new UtamCompilationError(
          String.format(ERR_RETURN_ALL_REDUNDANT, validationContextStr));
    }
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
  }

  /**
   * this constructor is used for return type of an abstract method when it returns basic type
   *
   * @param returnType   return type (basic union type in this case)
   * @param isReturnList true if return type should be wrapped as a list
   * @param methodName   method name
   */
  public ReturnType(TypeProvider returnType, Boolean isReturnList, String methodName) {
    this.validationContextStr = String.format("abstract method '%s'", methodName);
    this.typeProvider = translationContext -> returnType;
    this.isReturnTypeSet = true;
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
  }

  /**
   * Initializes a new instance of the ReturnType class
   *
   * @param methodName the method name
   */
  public ReturnType(String methodName) {
    this((JsonNode) null, null, methodName);
  }

  /**
   * Gets a value indicating whether a return type is set
   *
   * @return true if the return type is set; otherwise, false
   */
  public boolean isReturnTypeSet() {
    return isReturnTypeSet;
  }

  private Function<TranslationContext, TypeProvider> getTypeProvider(JsonNode typeNode) {
    return translatorContext -> {
      if (typeNode == null || typeNode.isNull()) {
        return null;
      }
      if (typeNode.isTextual()) {
        String typeValue = typeNode.textValue();
        if (isPrimitiveType(typeValue)) {
          return PrimitiveType.fromString(typeValue);
        }
        if (isPageObjectType(typeValue)) {
          return translatorContext.getType(typeValue);
        }
        if (VOID.getSimpleName().equals(typeValue)) {
          return VOID;
        }
      }
      throw new UtamCompilationError(
          String.format(ERR_UNSUPPORTED_RETURN_TYPE, validationContextStr,
              typeNode.toPrettyString()));
    };
  }

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
    TypeProvider returns = typeProvider.apply(translatorContext);
    if (isReturnList) {
      return wrapAsList(returns);
    }
    return returns;
  }

  /**
   * Gets a value indicating whether to return a list
   * @return true if the return type is a list; otherwise false
   */
  boolean isReturnAllSet() {
    return isReturnList;
  }
}
