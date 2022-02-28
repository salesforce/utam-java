/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.representation.ElementMethod.getElementLocationCode;
import static utam.compiler.representation.ElementMethod.getScopeElementCode;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.FrameElement;

/**
 * generate code of getter method for frame element
 *
 * @author james.evans
 * @since 236
 */
public class FrameMethod implements PageObjectMethod {

  /**
   * A type provider representing a frame element
   */
  public static final TypeProvider FRAME_ELEMENT = new TypeUtilities.FromClass(FrameElement.class);
  private final String methodName;
  private final List<String> methodCode = new ArrayList<>();
  private final List<MethodParameter> parameters;
  private final boolean isPublic;
  private final UtamMethodDescription description;

  /**
   * Initializes a new instance of the FrameMethod class
   * @param element           the element
   * @param isPublic          a value indicating whether the element is public
   * @param locatorParameters the list of parameters to use in locating the element
   */
  public FrameMethod(ElementContext element, boolean isPublic, List<MethodParameter> locatorParameters, UtamMethodDescription description) {
    methodCode.add(getScopeElementCode(element.getScopeElement()));
    String scopeVariableName = element.getScopeElement().getName();
    String locationWithParameters = getElementLocationCode(element.getName(), locatorParameters);
    methodCode.add(String.format("return basic(%s, %s).buildFrame()",
        scopeVariableName,
        locationWithParameters));
    this.methodName = getElementGetterMethodName(element.getName(), isPublic);
    this.parameters = element.getParameters();
    this.isPublic = isPublic;
    this.description = description;
  }

  @Override
  public MethodDeclaration getDeclaration() {
    List<TypeProvider> imports = Stream.of(FRAME_ELEMENT).collect(Collectors.toList());
    return new MethodDeclarationImpl(methodName, parameters, FRAME_ELEMENT, imports, description);
  }

  @Override
  public List<String> getCodeLines() {
    return methodCode;
  }

  @Override
  public List<TypeProvider> getClassImports() {
    List<TypeProvider> imports = new ArrayList<>();
    ParameterUtils.setImports(imports, getDeclaration().getImports());
    ParameterUtils.setImport(imports, BASIC_ELEMENT);
    return imports;
  }

  @Override
  public boolean isPublic() {
    return isPublic;
  }
}
