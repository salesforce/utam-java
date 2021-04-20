/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.helpers.MethodContext;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

import java.util.ArrayList;
import java.util.List;

import static utam.compiler.helpers.TypeUtilities.VOID;

/**
 * Represents beforeLoad method that is used to set additional criteria to be satisfied,
 * before the load method completes.

 * @author igor.khorev
 * @since 234
 */
public class BeforeLoadMethod implements PageObjectMethod {

    private final String name;
    private final List<MethodParameter> parameters;
    private final List<String> code = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final List<TypeProvider> imports = new ArrayList<>();
    private final String comments;

    public BeforeLoadMethod(MethodContext methodContext, List<ComposeMethodStatement> statements,
                         List<MethodParameter> parameters, String comments) {
        this.name = methodContext.getName();
        this.parameters = new ArrayList<>(parameters);
        statements.forEach(
                statement -> {
                    code.addAll(statement.getCodeLines());
                    imports.addAll(statement.getImports());
                    classImports.addAll(statement.getClassImports());
                });
        this.comments = comments;
    }

    @Override
    public MethodDeclaration getDeclaration() {
        return new MethodDeclarationImpl(name, parameters, VOID, imports, comments);
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
