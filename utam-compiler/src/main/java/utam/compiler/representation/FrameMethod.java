/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.representation.ElementMethod.getElementMethodCode;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.FrameElement;
import utam.core.framework.base.FrameElementImpl;

/**
 * generate code of getter method for frame element
 *
 * @author james.evans
 * @since 236
 */
public class FrameMethod implements PageObjectMethod {

  public static final TypeProvider FRAME_ELEMENT = new TypeUtilities.FromClass(FrameElement.class);
  static final TypeProvider FRAME_IMPL_CLASS = new TypeUtilities.FromClass(FrameElementImpl.class);
  private final String methodName;
  private final String methodCode;
  private final List<MethodParameter> parameters;
  private final boolean isPublic;

  /**
   * construct frame
   *
   * @param element  element context
   * @param isPublic frame can be private if used only in compose
   */
  public FrameMethod(ElementContext element, boolean isPublic) {
    this.methodCode = getElementMethodCode(element, FRAME_IMPL_CLASS, false);
    this.methodName = getElementGetterMethodName(element.getName(), isPublic);
    this.parameters = element.getParameters();
    this.isPublic = isPublic;
  }

  @Override
  public MethodDeclaration getDeclaration() {
    List<TypeProvider> imports = Stream.of(FRAME_ELEMENT).collect(Collectors.toList());
    return new MethodDeclarationImpl(methodName, parameters, FRAME_ELEMENT, imports);
  }

  @Override
  public List<String> getCodeLines() {
    return  Stream.of(methodCode).collect(Collectors.toList());
  }

  @Override
  public List<TypeProvider> getClassImports() {
    List<TypeProvider> imports = new ArrayList<>(getDeclaration().getImports());
    imports.add(FRAME_IMPL_CLASS);
    return imports;
  }

  @Override
  public boolean isPublic() {
    return isPublic;
  }
}
