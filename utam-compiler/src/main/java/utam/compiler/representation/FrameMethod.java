/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.representation.ElementMethod.getElementLocationCode;
import static utam.compiler.representation.ElementMethod.setupScopeElement;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.representation.JavadocObject.MethodJavadoc;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * generate code of getter method for frame element
 *
 * @author james.evans
 * @since 236
 */
public class FrameMethod implements PageObjectMethod {

  private final String methodName;
  private final List<String> methodCode = new ArrayList<>();
  private final MethodParametersTracker parametersTracker;
  private final boolean isPublic;
  private final UtamMethodDescription description;
  private final List<TypeProvider> imports = Stream.of(FRAME_ELEMENT).collect(Collectors.toList());
  private final List<TypeProvider> classImports =
      Stream.of(FRAME_ELEMENT, BASIC_ELEMENT).collect(Collectors.toList());

  /**
   * Initializes a new instance of the FrameMethod class
   *
   * @param element the element
   * @param isPublic a value indicating whether the element is public
   * @param locatorParameters the list of parameters to use in locating the element
   * @param description the description of the method
   */
  public FrameMethod(
      ElementContext element,
      boolean isPublic,
      List<MethodParameter> locatorParameters,
      UtamMethodDescription description) {
    this.parametersTracker =
        new MethodParametersTracker(String.format("element '%s'", element.getName()));
    methodCode.add(setupScopeElement(element.getScopeElement(), parametersTracker));
    parametersTracker.setMethodParameters(locatorParameters);
    String scopeVariableName = element.getScopeElement().getName();
    String locationWithParameters = getElementLocationCode(element.getName(), locatorParameters);
    methodCode.add(
        String.format(
            "return basic(%s, %s).buildFrame()", scopeVariableName, locationWithParameters));
    this.methodName = getElementGetterMethodName(element.getName(), isPublic);
    this.isPublic = isPublic;
    this.description = description;
    ParameterUtils.setImport(classImports, BASIC_ELEMENT);
  }

  @Override
  public MethodDeclaration getDeclaration() {
    List<MethodParameter> methodParameters = parametersTracker.getMethodParameters();
    JavadocObject javadoc =
        new MethodJavadoc(methodName, FRAME_ELEMENT, methodParameters, description);
    return new MethodDeclarationImpl(methodName, methodParameters, FRAME_ELEMENT, imports, javadoc);
  }

  @Override
  public List<String> getCodeLines() {
    return methodCode;
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public boolean isPublic() {
    return isPublic;
  }
}
