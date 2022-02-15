/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.types.BasicElementInterface.isBasicType;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.ReturnType.MethodBasicReturnType;
import utam.compiler.helpers.ReturnType.MethodReturnType;
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
   * check if "returnType" in an interface method returns basic type
   *
   * @param returnTypeNode the node containing the return type
   * @return true if it is
   */
  private boolean isReturnBasicType(JsonNode returnTypeNode) {
    if (returnTypeNode == null || returnTypeNode.isNull()) {
      return false;
    }
    if (returnTypeNode.isTextual() && isBasicType(returnTypeNode.textValue())) {
      return true;
    }
    if (returnTypeNode.isArray()) {
      for (JsonNode valueNode : returnTypeNode) {
        if (!valueNode.isTextual() || !isBasicType(valueNode.textValue())) {
          throw new UtamCompilerIntermediateError(returnTypeNode, "UIM001", name,
              returnTypeNode.toPrettyString());
        }
      }
      return true;
    }
    return false;
  }

  @Override
  PageObjectMethod getMethod(TranslationContext context) {
    boolean isReturnsBasicType = isReturnBasicType(returnTypeNode);
    final ReturnType returnTypeObject;
    String typeNodeValue = returnTypeNode == null ? "null" : returnTypeNode.toPrettyString();
    if (isReturnsBasicType) {
      String[] basicUnionType = processBasicTypeNode(returnTypeNode,
          node -> new UtamCompilerIntermediateError(node, "UIM001",
              name, typeNodeValue));
      TypeProvider unionReturnType = asBasicOrUnionType(name, basicUnionType, false);
      returnTypeObject = new MethodBasicReturnType(unionReturnType, isReturnList);
    } else {
      returnTypeObject = new MethodReturnType(returnTypeNode, isReturnList, name);
    }
    TypeProvider methodReturnType = returnTypeObject.getReturnTypeOrDefault(context, VOID);
    MethodContext methodContext = new MethodContext(name, returnTypeObject, context, argsNode, true);
    ParametersContext parametersContext = methodContext.getParametersContext();
    setMethodLevelParameters(context, methodContext);
    List<MethodParameter> parameters = parametersContext.getParameters();
    return isReturnsBasicType ? new AbstractBasicElementGetter(name, parameters, methodReturnType,
        description)
        : new InterfaceMethod(name, methodReturnType, parameters, description);
  }
}
