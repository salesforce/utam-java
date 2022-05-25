package utam.compiler.helpers;

import static utam.compiler.UtamCompilerIntermediateError.getJsonNodeType;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_RETURN;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_RETURN_LIST;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT_RETURN;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT_RETURN_LIST;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationTypesConfigJava.isPageObjectType;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.function.Supplier;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper for return properties in a method or in a statement
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public abstract class ReturnType {

  final boolean isReturnList;
  final JsonNode returnTypeJsonNode;
  private final String returnTypeString;

  /**
   * create a new instance of the ReturnType class
   *
   * @param typeNode          the JSON node of the type
   * @param isReturnList      a value indicating whether the return type is a list
   * @param validationContext validation context for error message
   */
  ReturnType(JsonNode typeNode, Boolean isReturnList, String validationContext) {
    this.returnTypeJsonNode = typeNode;
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
    if (!isEmptyNode(typeNode)) {
      if (returnTypeJsonNode.isTextual()) {
        this.returnTypeString = returnTypeJsonNode.textValue();
      } else {
        throw new UtamCompilerIntermediateError(returnTypeJsonNode, 10, validationContext,
            "returnType", getJsonNodeType(returnTypeJsonNode));
      }
    } else {
      this.returnTypeString = null;
    }
  }

  ReturnType(TypeProvider basicReturnType, Boolean isReturnList) {
    this.returnTypeJsonNode = null;
    this.returnTypeString = basicReturnType.getSimpleName();
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
  }

  /**
   * build object for VOID return type
   *
   * @param methodName the method name
   */
  public static ReturnType getVoidReturn(String methodName) {
    return new AbstractMethodReturnType(null, null, methodName);
  }

  /**
   * Gets a value indicating whether a return type is set
   *
   * @return true if the return type is set; otherwise, false
   */
  public final boolean isReturnTypeSet() {
    return returnTypeString != null;
  }

  /**
   * if method or statement returns page object instance, use different parameter for the method
   *
   * @return true if page object is returned
   */
  public boolean isPageObject() {
    return PAGE_OBJECT_TYPE_NAME.equals(returnTypeString)
        || ROOT_PAGE_OBJECT_TYPE_NAME.equals(returnTypeString);
  }

  /**
   * Gets the return type
   *
   * @param context the translation context
   * @return the return type if defined
   */
  public TypeProvider getReturnType(TranslationContext context) {
    if (returnTypeString == null) {
      return null;
    }
    if (PAGE_OBJECT_TYPE_NAME.equals(returnTypeString)) {
      return isReturnList ? PAGE_OBJECT_RETURN_LIST : PAGE_OBJECT_RETURN;
    }
    if (ROOT_PAGE_OBJECT_TYPE_NAME.equals(returnTypeString)) {
      return isReturnList ? ROOT_PAGE_OBJECT_RETURN_LIST : ROOT_PAGE_OBJECT_RETURN;
    }
    if (isPrimitiveType(returnTypeString)) {
      TypeProvider type = PrimitiveType.fromString(returnTypeString);
      return isReturnList ? wrapAsList(type) : type;
    }
    if (isPageObjectType(returnTypeString)) {
      TypeProvider type = context.getType(returnTypeString);
      return isReturnList ? wrapAsList(type) : type;
    }
    if (VOID.getSimpleName().equals(returnTypeString)) {
      return VOID;
    }
    throw getUnsupportedTypeError(context).get();
  }

  abstract Supplier<RuntimeException> getUnsupportedTypeError(TranslationContext context);

  /**
   * Gets the return type or a default value
   *
   * @param context     the translation context
   * @param defaultType the default type to return if none is set
   * @return the return type if defined; otherwise return the default type
   */
  public TypeProvider getReturnTypeOrDefault(TranslationContext context, TypeProvider defaultType) {
    TypeProvider definedType = getReturnType(context);
    return definedType == null ? defaultType : definedType;
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
   * Return type at the method level for an abstract method (in compose method return type is only
   * supported inside the statement)
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class AbstractMethodReturnType extends ReturnType {

    private final String methodName;

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode     the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName   the method name
     */
    public AbstractMethodReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, String.format("method \"%s\"", methodName));
      if (this.isReturnList && !isReturnTypeSet()) {
        throw new UtamCompilerIntermediateError(402, methodName);
      }
      this.methodName = methodName;
    }

    @Override
    Supplier<RuntimeException> getUnsupportedTypeError(TranslationContext context) {
      String errorMessage = context.getErrorMessage(403, methodName, returnTypeJsonNode.toPrettyString());
      return () -> new UtamCompilationError(returnTypeJsonNode, errorMessage);
    }
  }

  /**
   * basic return type for an abstract method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class AbstractMethodBasicReturnType extends ReturnType {

    private final TypeProvider basicReturnType;

    /**
     * this constructor is used for return type of an abstract method when it returns basic type
     *
     * @param returnType   return type (basic union type in this case)
     * @param isReturnList true if return type should be wrapped as a list
     */
    public AbstractMethodBasicReturnType(TypeProvider returnType, Boolean isReturnList) {
      super(returnType, isReturnList);
      this.basicReturnType = returnType;
    }

    @Override
    public TypeProvider getReturnType(TranslationContext context) {
      return isReturnList ? wrapAsList(basicReturnType) : basicReturnType;
    }

    @Override
    Supplier<RuntimeException> getUnsupportedTypeError(TranslationContext context) {
      throw new IllegalStateException(); // never happens, type is provided in constructor
    }
  }

  /**
   * statement return type for a compose method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static final class StatementReturnType extends ReturnType {

    private final String methodName;

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode     the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName   the method name
     */
    public StatementReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, String.format("method \"%s\"", methodName));
      this.methodName = methodName;
      if (this.isReturnList && !isReturnTypeSet()) {
        throw new UtamCompilerIntermediateError(603, methodName);
      }
    }

    @Override
    Supplier<RuntimeException> getUnsupportedTypeError(TranslationContext context) {
      String errorMessage = context.getErrorMessage(602, methodName, returnTypeJsonNode.toPrettyString());
      return () -> new UtamCompilationError(returnTypeJsonNode, errorMessage);
    }
  }
}
