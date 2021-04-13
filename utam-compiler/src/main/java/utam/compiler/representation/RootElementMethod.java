/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;

/**
 * getter for root element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class RootElementMethod {

  /**
   * BasePageObject.getRootElement returns Actionable, need type cast for other types
   *
   * @param rootElementType type to return
   * @return string code of the method
   */
  private static String buildRootMethodCode(TypeProvider rootElementType) {
    if (rootElementType.isSameType(actionable)) {
      return "this.getRootElement()";
    }
    return String.format("(%s) this.getRootElement()", rootElementType.getSimpleName());
  }

  public static class Protected extends Public {

    public Protected() {
      super(actionable);
    }

    @Override
    public boolean isPublic() {
      return false;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          "getRootElement",
          EMPTY_PARAMETERS,
          this.returnType,
          Stream.of(this.returnType).collect(Collectors.toList()));
    }
  }

  public static class Private extends Public {

    public Private(TypeProvider returnType) {
      super(returnType);
    }

    @Override
    public boolean isPublic() {
      return false;
    }
  }

  public static class Public implements PageObjectMethod {

    final TypeProvider returnType;
    final List<String> codeLines;

    public Public(TypeProvider returnType) {
      this.returnType = returnType;
      this.codeLines = Stream.of(buildRootMethodCode(returnType)).collect(Collectors.toList());
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          "getRoot",
          EMPTY_PARAMETERS,
          this.returnType,
          Stream.of(this.returnType).collect(Collectors.toList()));
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public List<String> getCodeLines() {
      return codeLines;
    }

    @Override
    public boolean isPublic() {
      return true;
    }
  }
}
