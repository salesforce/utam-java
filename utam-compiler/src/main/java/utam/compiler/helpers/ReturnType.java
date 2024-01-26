package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT_TYPE_NAME;
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
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper for return properties in a method or in a statement
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public abstract class ReturnType {

  /** Instance of the void return type. Used for beforeLoad MethodContext and in tests */
  public static final ReturnType RETURN_VOID = new ReturnType() {};

  final boolean isReturnList;
  private final JsonNode returnTypeJsonNode;
  final String returnTypeString;
  final String methodName;

  /**
   * Create a new instance of the ReturnType class
   *
   * @param typeNode the JSON node of the type
   * @param isReturnList a value indicating whether the return type is a list
   * @param methodName method name with return type
   */
  ReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
    this.returnTypeJsonNode = typeNode;
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
    this.methodName = methodName;
    if (!isEmptyNode(returnTypeJsonNode)) {
      String validationContext = String.format("method \"%s\"", methodName);
      this.returnTypeString =
          VALIDATION.validateNotNullOrEmptyString(
              returnTypeJsonNode, validationContext, "returnType");
    } else {
      this.returnTypeString = null;
    }
  }

  /**
   * Instance of ReturnType for abstract method that returns basic type
   *
   * @param basicReturnType basic type
   * @param isReturnList boolean to return list
   */
  ReturnType(TypeProvider basicReturnType, Boolean isReturnList) {
    this.returnTypeJsonNode = null;
    this.returnTypeString = basicReturnType.getSimpleName();
    this.isReturnList = Boolean.TRUE.equals(isReturnList);
    this.methodName = null;
  }

  /** Instance of void return type */
  private ReturnType() {
    this.returnTypeJsonNode = null;
    this.returnTypeString = null;
    this.isReturnList = false;
    this.methodName = null;
  }

  /**
   * Check if returnAll is set for null or void return type to later throw an error
   *
   * @return boolean
   */
  final boolean isReturnAllRedundant() {
    return Boolean.TRUE.equals(isReturnList)
        && (returnTypeString == null || "void".equals(returnTypeString));
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
    if (FRAME_ELEMENT_TYPE_NAME.equals(returnTypeString)) {
      return FRAME_ELEMENT;
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
    String errMessage = getUnsupportedReturnTypeErr();
    throw new UtamCompilationError(returnTypeJsonNode, errMessage);
  }

  String getUnsupportedReturnTypeErr() {
    // for VOID or Basic return types this error never happens, otherwise overridden
    return "";
  }

  /**
   * Gets the return type or a default value
   *
   * @param context the translation context
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

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName the method name
     */
    public AbstractMethodReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, methodName);
      if (isReturnAllRedundant()) {
        String returnAllRedundantErr = VALIDATION.getErrorMessage(402, methodName);
        throw new UtamCompilationError(typeNode, returnAllRedundantErr);
      }
    }

    @Override
    String getUnsupportedReturnTypeErr() {
      return VALIDATION.getErrorMessage(403, methodName, returnTypeString);
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
     * @param returnType return type (basic union type in this case)
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
  }

  /**
   * statement return type for a compose method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static final class StatementReturnType extends ReturnType {

    /**
     * Initializes a new instance of the ReturnType class
     *
     * @param typeNode the JSON node of the type
     * @param isReturnList a value indicating whether the return type is a list
     * @param methodName the method name
     */
    public StatementReturnType(JsonNode typeNode, Boolean isReturnList, String methodName) {
      super(typeNode, isReturnList, methodName);
      if (isReturnAllRedundant()) {
        String returnAllRedundantErr = VALIDATION.getErrorMessage(603, methodName);
        throw new UtamCompilationError(typeNode, returnAllRedundantErr);
      }
    }

    @Override
    String getUnsupportedReturnTypeErr() {
      return VALIDATION.getErrorMessage(602, methodName, returnTypeString);
    }
  }
}
