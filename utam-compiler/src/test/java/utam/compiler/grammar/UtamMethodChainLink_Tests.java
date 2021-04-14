/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ChainMethod;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamMethodChainLink.ERR_WRONG_CARDINALITY_FOR_FIRST_LINK;
import static utam.compiler.grammar.UtamMethodChainLink.ERR_WRONG_RETURN_TYPE_FOR_FIRST_LINK;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UtamMethodChainLink_Tests {

  /** The getChainStatement method should return the proper value */
  @Test
  public void testGetChainStatement() {
    final String ELEMENT_NAME = "elementName";
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("selector")).testTraverse(context);
    UtamMethodChainLink link = new UtamMethodChainLink(ELEMENT_NAME, false, TEST_URI);
    assertThat(link.getChainStatement(context, null), is(instanceOf(ChainMethod.Link.class)));
    link.getChainStatement(context, null);
  }

  /** The getChainStatement method should return the proper value with a list action */
  @Test
  public void testGetChainStatementWithList() {
    final String ELEMENT_NAME = "elementName";
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("selector")).testTraverse(context);
    UtamMethodChainLink link = new UtamMethodChainLink(ELEMENT_NAME, true, TEST_URI);
    assertThat(link.getChainStatement(context, null), is(instanceOf(ChainMethod.Link.class)));
  }

  @Test
  public void testThrowsIncorrectType() {
    final String ELEMENT_NAME = "elementName";
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {new UtamElement(ELEMENT_NAME, TEST_URI, new UtamSelector("selector"))};
    object.compile(context);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                new UtamMethodChainLink(ELEMENT_NAME, false, "wrong type")
                    .getChainStatement(context, context.getElement(ELEMENT_NAME)));
    assertThat(e.getMessage(), containsString("type should have format"));
    String type = "utam-me/pageObjects/me/Me";
    e =
        expectThrows(
            UtamError.class,
            () ->
                new UtamMethodChainLink(ELEMENT_NAME, false, type)
                    .getChainStatement(context, context.getElement(ELEMENT_NAME)));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    ERR_WRONG_RETURN_TYPE_FOR_FIRST_LINK,
                    ELEMENT_NAME,
                    TEST_URI_PROCESSED,
                    getURIasTypeName(type)))));
  }

  @Test
  public void testThrowsIncorrectCardinalityExpectingReturnList() {
    final String ELEMENT_NAME = "elementName";
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {new UtamElement(ELEMENT_NAME, TEST_URI, new UtamSelector("selector"))};
    object.compile(context);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                new UtamMethodChainLink(ELEMENT_NAME, true, TEST_URI)
                    .getChainStatement(context, context.getElement(ELEMENT_NAME)));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(ERR_WRONG_CARDINALITY_FOR_FIRST_LINK, ELEMENT_NAME, "not "))));
  }

  @Test
  public void testThrowsIncorrectCardinalityExpectingReturnSingleElement() {
    final String ELEMENT_NAME = "elementName";
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {new UtamElement(ELEMENT_NAME, TEST_URI, new UtamSelector("selector", true))};
    object.compile(context);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                new UtamMethodChainLink(ELEMENT_NAME, false, TEST_URI)
                    .getChainStatement(context, context.getElement(ELEMENT_NAME)));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(ERR_WRONG_CARDINALITY_FOR_FIRST_LINK, ELEMENT_NAME, ""))));
  }
}
