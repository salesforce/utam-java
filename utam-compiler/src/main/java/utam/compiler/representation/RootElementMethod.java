/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * getter for root element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class RootElementMethod {

  public static class Protected extends Public {

    public Protected() {
      super(ROOT_ELEMENT_TYPE);
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

    private static final List<String> codeLines = Collections
        .singletonList("return this.getRootElement()");
    final TypeProvider returnType;

    public Public(TypeProvider returnType) {
      this.returnType = returnType;
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
