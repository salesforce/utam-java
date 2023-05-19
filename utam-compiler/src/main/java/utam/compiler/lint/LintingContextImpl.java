/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import java.util.ArrayList;
import java.util.List;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.PageObjectLinting;

/**
 * Context of the linting run
 *
 * @author elizaveta.ivanova
 * @since 242
 */
class LintingContextImpl implements LintingContext {

  private final List<PageObjectLinting> contexts = new ArrayList<>();
  private final List<LintingError> errors = new ArrayList<>();

  @Override
  public void addPageObject(PageObjectLinting context) {
    contexts.add(context);
  }

  @Override
  public List<PageObjectLinting> getAllPageObjects() {
    return contexts;
  }

  @Override
  public List<LintingError> getErrors() {
    return errors;
  }
}
