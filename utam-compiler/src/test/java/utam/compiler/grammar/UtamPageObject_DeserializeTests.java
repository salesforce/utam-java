/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.selenium.element.LocatorBy;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamPageObject.*;
import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

/**
 * Provides deserialization tests for the UtamPageObject class
 *
 * @author james.evans
 */
public class UtamPageObject_DeserializeTests {

  private static UtamPageObject createRootElementNode(String jsonStr) {
    return getDeserializedObject(jsonStr, UtamPageObject.class);
  }

  /**
   * A UtamPageObject object should be able to be created through deserialization NOTE: All
   * deserialization cases should not be tested in this file, only required and default values
   */
  @Test
  public void testDeserializationDefaultValues() {
    String json = "{}";
    UtamPageObject pageObject = getDeserializedObject(json, UtamPageObject.class);
    assertThat(pageObject, is(not(nullValue())));
    assertThat(pageObject.implementsType, is(nullValue()));
    assertThat(pageObject.methods, is(nullValue()));
    assertThat(pageObject.platform, is(nullValue()));
    assertThat(pageObject.isAbstract, is(equalTo(false)));
    assertThat(pageObject.isRootPageObject, is(equalTo(false)));
  }

  /** An empty root node should be valid */
  @Test
  public void testEmptyRootNode() {
    String json = "{}";
    UtamPageObject root = createRootElementNode(json);
    assertThat(root.getBaseType().getFullName(), containsString(PAGE_OBJECT.getFullName()));
    TranslationContext context = getTestTranslationContext();
    root.compile(context);
    assertThat(
        context.getRootElement().getType().isSameType(ROOT_ELEMENT_TYPE),
        is(equalTo(true)));
  }

  /** A root node with a selector property should be valid */
  @Test
  public void testValidRootNodeWithSelector() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";

    UtamPageObject rootElementNode = createRootElementNode(json);
    assertThat(rootElementNode.isExposeRootElement, is(false));
    assertThat(rootElementNode.rootLocator, is(equalTo(LocatorBy.byCss("rootSelector"))));
  }

  /** The traverse method should traverse the JSON node */
  @Test
  public void testTraverse() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";
    UtamPageObject rootElementNode = createRootElementNode(json);
    TranslationContext context = getTestTranslationContext();
    rootElementNode.compile(context);
  }

  /**
   * The traverse method should traverse the JSON node when public is set to true and the element
   * has a name
   */
  @Test
  public void testTraverseWithChildElement() {
    String json =
        "{"
            + "  \"type\": \"clickable\","
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true,"
            + "  \"exposeRootElement\": true,"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childNode\","
            + "      \"selector\": {"
            + "        \"css\": \".childSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";

    UtamPageObject root = createRootElementNode(json);
    TranslationContext context = getTestTranslationContext();
    root.compile(context);
    assertThat(root.elements.length, is(equalTo(1)));
    assertThat(
        context.getRootElement().getType().getSimpleName(),
        is(equalTo("RootElement")));
  }

  /** Tests that a root element with a platform using invalid value throws the proper exception */
  @Test
  public void testRootNodeWithPlatformThrows() {
    String json = "{\"platform\": \"webview\"" + "}";
    Exception e =
        expectThrows(
            IllegalArgumentException.class, () -> createRootElementNode(json).getAnnotations());
    assertThat(e.getMessage(), containsString("Unknown platform type"));
  }

  /** A valid root node should be able to be created when the root property is not specified */
  @Test
  public void testRootCreationWithoutRootPropertyErr() {
    String json =
            "{"
                    + "  \"selector\": {"
                    + "    \"css\": \"rootSelector\""
                    + "  }"
                    + "}";
    UtamError e = expectThrows(UtamError.class, () -> createRootElementNode(json));
    assertThat(e.getCause().getMessage(), containsString(ERR_ROOT_REDUNDANT_SELECTOR));
  }

  /**
   * Tests that an element marked as a root element, but without a selector throws the proper
   * exception
   */
  @Test
  public void testRootNodeWithRootPropertyWithoutSelectorThrows() {
    String json = "{\"root\": true" + "}";
    UtamError e = expectThrows(UtamError.class, () -> createRootElementNode(json));
    assertThat(e.getCause().getMessage(), containsString(ERR_ROOT_MISSING_SELECTOR));
  }

  /**
   * Tests that an invalid root node with a type property, but no name property throws the proper
   * exception
   */
  @Test
  public void testRootNodeWithTypeWithoutNamePropertyThrows() {
    String json = "{\"type\": \"invalid\"}";
    UtamError e = expectThrows(UtamError.class, () -> createRootElementNode(json).compile(getTestTranslationContext()));
    assertThat(e.getCause().getMessage(), containsString(String.format(ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE, "[invalid]")));
  }

  /**
   * Tests that an element marked with a root selector, but without a root property throws the
   * proper exception
   */
  @Test
  public void testRootNodeWithSelectorWithoutRootPropertyThrows() {
    String json =
            "{"
                    + "  \"name\": \"customName\","
                    + "  \"selector\": {"
                    + "    \"css\": \".invalidWithoutRootProperty\""
                    + "  }"
                    + "}";
    UtamError e = expectThrows(UtamError.class, () -> createRootElementNode(json));
    assertThat(e.getCause().getMessage(), containsString(ERR_ROOT_REDUNDANT_SELECTOR));
  }

  /**
   * Creating a root node should throw the appropriate exception when the root property is not a
   * boolean value
   */
  @Test
  public void testRootNodeWithNonBooleanRootPropertyThrows() {
    String json =
            "{"
                    + "  \"selector\": {"
                    + "    \"css\": \"rootSelector\""
                    + "  },"
                    + "  \"root\": \"invalid\""
                    + "}";
    UtamError e = expectThrows(UtamError.class, () -> createRootElementNode(json));
    assertThat(
            e.getCause().getMessage(),
            containsString(String.format(JACKSON_WRONG_PROPERTY_TYPE, "boolean", "invalid")));
  }

  /**
   * Tests that a JsonDeserializer constructed using a Reader object will throw the proper exception
   * if there is an error in the JSON
   */
  @Test
  public void testConstructorWithReaderAndInvalidJsonThrows() {
    String json = "{" + "  \"public\": true" + "}";
    assertThrows(UtamError.class, () -> createRootElementNode(json));
  }
}
