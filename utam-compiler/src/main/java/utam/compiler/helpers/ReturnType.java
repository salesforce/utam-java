package utam.compiler.helpers;

import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationTypesConfigJava.isPageObjectType;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
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

  final String validationContextStr;
  private final boolean isReturnList;
  private final boolean isReturnTypeSet;
  private final boolean isReturnSelf;
  final String methodName;

  private final Function<TranslationContext, TypeProvider> typeProvider;

  public ReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
    this.validationContextStr = String.format("method '%s'", methodName);
    this.methodName = methodName;
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

  public ReturnType(String methodName) {
    this(null, null, methodName);
  }

  public boolean isReturnSelf() {
    return isReturnSelf;
  }

  public boolean isReturnTypeSet() {
    return isReturnTypeSet;
  }

  Function<TranslationContext, TypeProvider> getTypeProvider(JsonNode typeNode) {
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

  public static class AbstractMethodReturnType extends ReturnType {

    static final String ERR_INVALID_ARRAY_VALUES =
        "%s: returnType array '%s' must contain string values with basic types";

    public AbstractMethodReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, methodName);
    }

    private String getArrayError(JsonNode typeNode) {
      return String.format(ERR_INVALID_ARRAY_VALUES, validationContextStr, typeNode.toPrettyString());
    }

    @Override
    Function<TranslationContext, TypeProvider> getTypeProvider(JsonNode typeNode) {
      if(typeNode != null && typeNode.isArray()) {
        return translationContext -> {
          List<String> unionTypes = new ArrayList<>();
          for (JsonNode valueNode : typeNode) {
            if (!valueNode.isTextual()) {
              throw new UtamCompilationError(getArrayError(typeNode));
            }
            String valueStr = valueNode.textValue();
            if (!BasicElementInterface.isBasicType(valueStr)) {
              throw new UtamCompilationError(getArrayError(typeNode));
            }
            unionTypes.add(valueStr);
          }
          if(unionTypes.isEmpty()) {
            throw new UtamCompilationError(getArrayError(typeNode));
          }
          // for interfaces - create type using method name as elements are not declared
          return BasicElementUnionType.asBasicType(methodName, unionTypes.toArray(String[]::new), false);
        };
      }
      return super.getTypeProvider(typeNode);
    }
  }
}
