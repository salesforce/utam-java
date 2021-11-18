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
  static final String ERR_RETURN_ALL_REDUNDANT_FOR_SELF =
      "%s: 'returnAll' property can't be set for 'returnType' self";

  private final String validationContextStr;
  private final boolean isReturnList;
  private final boolean isReturnTypeSet;
  private final boolean isReturnSelf;

  private final Function<TranslationContext, TypeProvider> typeProvider;

  public ReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
    this.validationContextStr = String.format("method '%s'", methodName);
    this.typeProvider = getTypeProvider(typeNode);
    this.isReturnTypeSet = typeNode != null && !typeNode.isNull();
    if (isReturnList != null && !isReturnTypeSet) {
      throw new UtamCompilationError(
          String.format(ERR_RETURN_ALL_REDUNDANT, validationContextStr));
    }
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
    this.isReturnSelf = isReturnTypeSet && typeNode.isTextual() && "self".equals(typeNode.asText());
    if (isReturnList != null && isReturnSelf) {
      throw new UtamCompilationError(
          String.format(ERR_RETURN_ALL_REDUNDANT_FOR_SELF, validationContextStr));
    }
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
    this.isReturnSelf = false;
  }

  public ReturnType(String methodName) {
    this((JsonNode) null, null, methodName);
  }

  public boolean isReturnSelf() {
    return isReturnSelf;
  }

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
        if ("self".equals(typeValue)) {
          return translatorContext.getSelfType();
        }
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

  public TypeProvider getReturnTypeOrDefault(TranslationContext translatorContext,
      TypeProvider typeProvider) {
    TypeProvider definedType = getReturnTypeOrNull(translatorContext);
    return definedType == null ? typeProvider : definedType;
  }

  public TypeProvider getReturnTypeOrNull(TranslationContext translatorContext) {
    TypeProvider returns = typeProvider.apply(translatorContext);
    if (isReturnList) {
      return wrapAsList(returns);
    }
    return returns;
  }

  boolean isReturnAllSet() {
    return isReturnList;
  }
}
