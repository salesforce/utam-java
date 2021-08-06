/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamSelector;
import utam.core.declarative.representation.MethodParameter;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 226
 */
public class LocatorCodeGenerationTests {

  @Test
  public void testGetBuilderString() {

    final String prefix = LocatorBy.class.getSimpleName();
    final TranslationContext translationContext = getTestTranslationContext();

    UtamSelector selector = new UtamSelector("css", null, null, null);
    LocatorCodeGeneration helper = selector.getCodeGenerationHelper(translationContext);
    assertThat(helper.getBuilderString(), is(equalTo(prefix + ".byCss(\"css\")")));

    selector = new UtamSelector(null, "accessId", null, null);
    helper = selector.getCodeGenerationHelper(translationContext);
    assertThat(helper.getBuilderString(), is(equalTo(prefix + ".byAccessibilityId(\"accessId\")")));

    selector = new UtamSelector(null, null, "chain", null);
    helper = selector.getCodeGenerationHelper(translationContext);
    assertThat(helper.getBuilderString(), is(equalTo(prefix + ".byClassChain(\"chain\")")));

    selector = new UtamSelector(null, null, null, "new UiSelector().checkable()");
    helper = selector.getCodeGenerationHelper(translationContext);
    assertThat(helper.getBuilderString(), is(equalTo(prefix + ".byUiAutomator(\"new UiSelector().checkable()\")")));
  }

  @Test
  public void testWithStringArgsByValue() {
    String json = "{ \"css\" : \"stringArgSelector[%s]\" , \"args\" : [{ \"value\" : \"str\"}] }";
    UtamSelector selector = getDeserializedObject(json, UtamSelector.class);
    LocatorCodeGeneration context = selector.getCodeGenerationHelper(getTestTranslationContext());
    assertThat(context.getLocator(), is(equalTo(LocatorBy.byCss("stringArgSelector[%s]"))));
    assertThat(context.getBuilderString(), is(equalTo("LocatorBy.byCss(String.format(\"stringArgSelector[%s]\", \"str\"))")));
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"str\"")));
  }

  @Test
  public void testWithNumberArgsByName() {
    String json = "{ \"css\" : \"selector[%d]\" , \"args\" : [{ \"name\" : \"num1\", \"type\" : \"number\"}] }";
    UtamSelector selector = getDeserializedObject(json, UtamSelector.class);
    LocatorCodeGeneration context = selector.getCodeGenerationHelper(getTestTranslationContext());
    assertThat(context.getBuilderString(), is(equalTo("LocatorBy.byCss(String.format(\"selector[%d]\", num1))")));
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getDeclaration(), is(equalTo("Integer num1")));
  }
}
