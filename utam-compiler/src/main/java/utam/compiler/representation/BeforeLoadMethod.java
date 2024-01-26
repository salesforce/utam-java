/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription.UtamEmptyMethodDescription;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TypeUtilities.UnimportableType;
import utam.compiler.representation.JavadocObject.MethodJavadoc;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * Represents beforeLoad method that is used to set additional criteria to be satisfied, before the
 * load method completes.
 *
 * @author igor.khorev
 * @since 234
 */
public class BeforeLoadMethod implements PageObjectMethod {

  private static final TypeProvider OBJECT_RETURN_TYPE = new UnimportableType("Object");

  private final String name;
  private final List<String> code = new ArrayList<>();
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();

  /**
   * Initializes a new instance of the BeforeLoadMethod class.
   *
   * @param methodContext the method context of the before load method
   * @param statements the statements of the method
   */
  public BeforeLoadMethod(MethodContext methodContext, List<ComposeMethodStatement> statements) {
    this.name = methodContext.getName();
    statements.forEach(
        statement -> {
          code.addAll(statement.getCodeLines());
          imports.addAll(statement.getImports());
          classImports.addAll(statement.getClassImports());
        });
    // load method always return Object, so if statements did not, we add it explicitly
    if (!code.get(code.size() - 1).startsWith("return")) {
      code.add("return this");
    }
  }

  @Override
  public MethodDeclaration getDeclaration() {
    JavadocObject javadoc =
        new MethodJavadoc(
            name, OBJECT_RETURN_TYPE, new ArrayList<>(), new UtamEmptyMethodDescription());
    return new MethodDeclarationImpl(name, new ArrayList<>(), OBJECT_RETURN_TYPE, imports, javadoc);
  }

  @Override
  public List<String> getCodeLines() {
    return code;
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public boolean isPublic() {
    return true;
  }
}
