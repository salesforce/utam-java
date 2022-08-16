/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.nodeToString;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.types.BasicElementInterface.isBasicType;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.function.Supplier;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.ReturnType.AbstractMethodBasicReturnType;
import utam.compiler.helpers.ReturnType.AbstractMethodReturnType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.InterfaceMethod;
import utam.compiler.representation.InterfaceMethod.AbstractBasicElementGetter;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * abstract method declaration
 *
 * @author elizaveta.ivanova
 * @since 238
 */
class UtamInterfaceMethod extends UtamMethod {

  private final JsonNode returnTypeNode;
  private final Boolean isReturnList;

  @JsonCreator
  UtamInterfaceMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty(value = "returnType") JsonNode returnTypeNode,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty("description") JsonNode descriptionNode) {
    super(name, descriptionNode, argsNode);
    this.returnTypeNode = returnTypeNode;
    this.isReturnList = isReturnList;
  }

  /**
   * Check if "returnType" of an interface method returns basic type
   *
   * @param returnTypeNode JSON node with return type
   * @param context        context to build error message
   * @return true if it is
   */
  private boolean isReturnBasicType(JsonNode returnTypeNode, TranslationContext context) {
    if (isEmptyNode(returnTypeNode)) {
      return false;
    }
    if (returnTypeNode.isTextual() && isBasicType(returnTypeNode.textValue())) {
      return true;
    }
    if (returnTypeNode.isArray()) {
      for (JsonNode valueNode : returnTypeNode) {
        if (!valueNode.isTextual() || !isBasicType(valueNode.textValue())) {
          String unsupportedArrayValueErr = context.getErrorMessage(401, name, nodeToString(returnTypeNode));
          throw new UtamCompilationError(returnTypeNode, unsupportedArrayValueErr);
        }
      }
      return true;
    }
    return false;
  }

  @Override
  PageObjectMethod getMethod(TranslationContext context) {
    boolean isReturnsBasicType = isReturnBasicType(returnTypeNode, context);
    final ReturnType returnTypeObject;
    if (isReturnsBasicType) {
      String errMessage = context.getErrorMessage(401, name, nodeToString(returnTypeNode));
      Supplier<RuntimeException> errorProducer = () -> new UtamCompilationError(returnTypeNode, errMessage);
      String[] basicUnionType = processBasicTypeNode(returnTypeNode, errorProducer);
      TypeProvider unionReturnType = asBasicOrUnionType(name, basicUnionType, false);
      returnTypeObject = new AbstractMethodBasicReturnType(unionReturnType, isReturnList);
    } else {
      returnTypeObject = new AbstractMethodReturnType(returnTypeNode, isReturnList, context, name);
    }
    TypeProvider methodReturnType = returnTypeObject.getReturnTypeOrDefault(context, VOID);
    MethodContext methodContext = new MethodContext(name, returnTypeObject, context, argsNode, true);
    ParametersContext parametersContext = methodContext.getParametersContext();
    setMethodLevelParameters(context, methodContext);
    List<MethodParameter> parameters = parametersContext.getParameters();
    UtamMethodDescription description = getDescription(context);
    return isReturnsBasicType ? new AbstractBasicElementGetter(name, parameters, methodReturnType,
        description)
        : new InterfaceMethod(name, methodReturnType, parameters, description);
  }
}
