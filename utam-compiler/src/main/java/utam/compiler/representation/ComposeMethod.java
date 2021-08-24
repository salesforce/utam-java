/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * business method is a sequence of internal method calls
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class ComposeMethod implements PageObjectMethod {

  private final String name;
  private final List<MethodParameter> parameters;
  private final List<String> code = new ArrayList<>();
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();
  private final String comments;
  private final TypeProvider returns;

  public ComposeMethod(MethodContext methodContext, List<ComposeMethodStatement> statements,
      List<MethodParameter> parameters, String comments) {
    this.name = methodContext.getName();
    //if return type not set in JSON, get one from last statement
    this.returns = methodContext.getReturnType(statements, VOID);
    statements.forEach(
        statement -> {
          code.addAll(statement.getCodeLines());
          imports.addAll(statement.getImports());
          classImports.addAll(statement.getClassImports());
        });
    this.comments = comments;
    if(methodContext.hasMethodArgs()) {
      List<MethodParameter> distinctParams = new ArrayList<>();
      for (MethodParameter param : parameters) {
        if (!distinctParams.contains(param)) {
          distinctParams.add(param);
        }
      }
      this.parameters = new ArrayList<>(parameters);
    } else {
      this.parameters = new ArrayList<>(parameters);
    }
  }

  static String getElementLocatorString(ElementContext elementContext) {
    if(elementContext.getParameters().isEmpty()) {
      return String.format("this.%s", elementContext.getName());
    }
    String parameters = getParametersValuesString(elementContext.getParameters());
    return String.format("this.%s.setParameters(%s)", elementContext.getName(), parameters);
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, parameters, returns, imports, comments);
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public List<String> getCodeLines() {
    return code;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

}
