/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.guardrails.GlobalValidation.getErrorPrefix;
import static utam.compiler.helpers.ElementContext.Self.SELF_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.guardrails.GlobalValidation.ValidationSubject;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class GlobalValidationTests {

  @Test
  public void testValidateNoError() {
    ElementContext elementContext = SELF_ELEMENT;
    GlobalValidation globalValidation = new GlobalValidation(GuardrailsMode.ERROR);
    globalValidation.setPageObjectElements("first", Collections.singletonList(elementContext));
    globalValidation.setPageObjectElements("second", Collections.singletonList(elementContext));
    globalValidation.validate();
  }

  @Test
  public void testValidateThrowIfConfiguredWithErrorMode() {
    Locator selector = getCssSelector("css");
    ElementContext customElement =
        new ElementContext.Custom("name1", new TypeUtilities.FromString("test.Type"), selector);
    ElementContext basicElement = new ElementContext.Basic("name2", actionable, selector);

    GlobalValidation globalValidation = new GlobalValidation(GuardrailsMode.ERROR);
    globalValidation.setPageObjectElements("first", Collections.singletonList(basicElement));
    globalValidation.setPageObjectElements("second", Collections.singletonList(customElement));

    UtamError e = expectThrows(UtamError.class, globalValidation::validate);
    assertThat(e.getMessage(), startsWith(getErrorPrefix(new ValidationSubject("first", basicElement),
        new ValidationSubject("second", customElement))));
  }

  @Test
  public void testValidateWarningIfConfiguredWithWarningMode() {
    Locator selector = getCssSelector("css");
    ElementContext customElement =
        new ElementContext.Custom("name1", new TypeUtilities.FromString("test.Type"), selector);
    ElementContext basicElement = new ElementContext.Basic("name2", actionable, selector);

    GlobalValidation globalValidation = new GlobalValidation(GuardrailsMode.WARNING);
    globalValidation.setPageObjectElements("first", Collections.singletonList(basicElement));
    globalValidation.setPageObjectElements("second", Collections.singletonList(customElement));

    // warning in console
    globalValidation.validate();
  }
}
