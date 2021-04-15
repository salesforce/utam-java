/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.*;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.Validation.ERR_FORMAT_GLOBAL;
import static utam.compiler.helpers.Validation.ERR_FORMAT_LOCAL;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the Validation class
 *
 * @author james.evans
 */
public class ValidationTests {

  static final String ELEMENT_SELECTOR = ".fakeSelector";
  static final TypeProvider ELEMENT_TYPE =
      new TypeUtilities.FromString("FakeElementType", "test.FakeElementType");
  private static final String FIRST_PAGE_OBJECT_URI = "customPageObject";
  private static final String SECOND_PAGE_OBJECT_URI = "secondPageObject";
  private static final String ELEMENT_NAME = "fakeElementName";
  private static final List<String> HARDCODED_SELECTORS = List.of(
          "option[value='hardcoded']",
          "button[title='hardcoded']");
  private static final List<String> VALID_SELECTORS = List.of(
          "option[value='%s']",
          "button[title='%s']");

  private static Validation getValidation(Map<String, ElementContext> map) {
    return new Validation(FIRST_PAGE_OBJECT_URI, map);
  }

  private static ElementContext getBasicElement(String name, Locator selector) {
    return new ElementContext.Basic(
        null,
        name,
        actionable,
        selector,
        false,
        EMPTY_PARAMETERS);
  }

  private static void testGlobalDuplicates(
      ElementContext elementContext, Collection<ElementContext> elementsInSecondPageObjects) {
    // this can't be singleton
    Map<String, Collection<ElementContext>> cache = new HashMap<>();
    cache.put(SECOND_PAGE_OBJECT_URI, elementsInSecondPageObjects);
    Validation.testGlobalDuplicates(FIRST_PAGE_OBJECT_URI, elementContext, cache);
  }

  @Test
  public void testIsSameSelector() {
    Locator TEST = getCssSelector(ELEMENT_SELECTOR);
    assertThat(Validation.isSameSelector(TEST, TEST), is(equalTo(true)));
    assertThat(Validation.isSameSelector(getCssSelector(""), TEST), is(equalTo(false)));
    assertThat(Validation.isSameSelector(TEST, getCssSelector("")), is(equalTo(false)));
    assertThat(Validation.isSameSelector(TEST, getCssSelector("xxxx")), is(equalTo(false)));
    assertThat(
        Validation.isSameSelector(getCssSelector("label"), getCssSelector("label")),
        is(equalTo(true)));
  }

  /**
   * Verifies isLabelHardcoded() method returns correct value
   * for every selector type that can be potentially hardcoded.
   */
  @Test
  public void testIsLabelHardcoded() {
    var ref = new Object() {
      Locator TEST;
    };
    HARDCODED_SELECTORS.forEach(selector ->
            assertThat(Validation.isLabelHardcoded(ref.TEST = getCssSelector(selector)), is(equalTo(true))));
    VALID_SELECTORS.forEach(selector ->
            assertThat(Validation.isLabelHardcoded(ref.TEST = getCssSelector(selector)), is(equalTo(false))));
  }

  /** The testLocalDuplicates method should succeed with a unique element */
  @Test
  public void testTestLocalDuplicatesWithUniqueElement() {
    ElementContext first =
        new ElementContext.Basic(ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));

    ElementContext second =
        new ElementContext.Basic(
            "fakeOtherElementName", ELEMENT_TYPE, getCssSelector(".fakeOtherSelector"));

    getValidation(Collections.singletonMap(FIRST_PAGE_OBJECT_URI, first))
        .testLocalDuplicates(second);
    testGlobalDuplicates(second, Collections.singletonList(first));
  }

  /**
   * The testLocalDuplicates method with a duplicate element should throw the appropriate exception
   */
  @Test
  public void testTestHtmlElementsWithSameSelector() {
    ElementContext first =
        new ElementContext.Basic(ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));
    ElementContext second =
        new ElementContext.Basic(
            "fakeOtherElementName", ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));

    getValidation(Collections.singletonMap(FIRST_PAGE_OBJECT_URI, first))
        .testLocalDuplicates(second);
    testGlobalDuplicates(second, Collections.singletonList(first));
  }

  @Test
  public void testErr_COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext.Custom first =
        new ElementContext.Custom("first", new TypeUtilities.FromString("test.First"), selector);
    ElementContext.Custom second =
        new ElementContext.Custom(
            "second", new TypeUtilities.FromString("test.Second"), selector);
    Map<String, ElementContext> map = Collections.singletonMap("first", first);
    UtamError e =
        expectThrows(UtamError.class, () -> getValidation(map).testLocalDuplicates(second));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_FORMAT_LOCAL, "second", "first", FIRST_PAGE_OBJECT_URI)));
    assertThat(
        e.getMessage(),
        containsString(Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES.err));

    e =
        expectThrows(
            UtamError.class, () -> testGlobalDuplicates(first, Collections.singletonList(second)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_FORMAT_GLOBAL,
                "first",
                FIRST_PAGE_OBJECT_URI,
                "second",
                SECOND_PAGE_OBJECT_URI)));
    assertThat(
        e.getMessage(),
        containsString(Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES.err));
  }

  @Test
  public void testErr_COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext.Basic first = new ElementContext.Basic("first", actionable, selector);
    ElementContext.Custom second =
        new ElementContext.Custom(
            "second", new TypeUtilities.FromString("test.Second"), selector);
    Map<String, ElementContext> map = Collections.singletonMap("first", first);
    UtamError e =
        expectThrows(UtamError.class, () -> getValidation(map).testLocalDuplicates(second));
    assertThat(
        e.getMessage(),
        containsString(Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR.err));

    e =
        expectThrows(
            UtamError.class, () -> testGlobalDuplicates(second, Collections.singletonList(first)));
    assertThat(
        e.getMessage(),
        containsString(Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR.err));
  }

  @Test
  public void testErr_DUPLICATE_WITH_ROOT_SELECTOR() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext.Basic first = new ElementContext.Basic("first", actionable, selector);
    ElementContext.Root second =
        new ElementContext.Root(new TypeUtilities.FromString("test.Second"), actionable, selector);
    Map<String, ElementContext> map = Collections.singletonMap("first", first);
    UtamError e =
        expectThrows(UtamError.class, () -> getValidation(map).testLocalDuplicates(second));
    assertThat(
        e.getMessage(), containsString(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR.err));

    e =
        expectThrows(
            UtamError.class, () -> testGlobalDuplicates(second, Collections.singletonList(first)));
    assertThat(
        e.getMessage(), containsString(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR.err));
  }

  @Test
  public void validateRootWithEmptySelector() {
    ElementContext.Root root = new ElementContext.Root(new TypeUtilities.FromString("test.Type"));
    // root w same enclosing type
    assertThat(
        root.validate(new ElementContext.Root(new TypeUtilities.FromString("test.Type"))),
        is(Validation.ErrorType.NONE));

    // root with different enclosing type
    assertThat(
        root.validate(new ElementContext.Root(new TypeUtilities.FromString("test.AnotherType"))),
        is(Validation.ErrorType.NONE));

  }

  @Test
  public void validateRootWithNonEmptySelector() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    TypeProvider elementType = actionable;
    ElementContext.Root root =
        new ElementContext.Root(new TypeUtilities.FromString("test.Type"), elementType, selector);
    assertThat(
        root.validate(new ElementContext.Root(new TypeUtilities.FromString("test.Type"), elementType, selector)),
        is(Validation.ErrorType.NONE));
    assertThat(
        root.validate(
            new ElementContext.Root(new TypeUtilities.FromString("test.AnotherType"), elementType, selector)),
        is(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR));
    assertThat(
        root.validate(
            new ElementContext.Custom(
                "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(Validation.ErrorType.NONE));

    assertThat(
        root.validate(
            new ElementContext.Custom(
                "name", new TypeUtilities.FromString("test.WrongType"), selector)),
        is(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR));
  }

  @Test
  public void validateComponentElement() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    TypeProvider elementType = actionable;
    ElementContext.Custom self =
        new ElementContext.Custom("name", new TypeUtilities.FromString("test.Type"), selector);
    assertThat(
        self.validate(getBasicElement("html", selector)),
        is(Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR));
    assertThat(
        self.validate(getBasicElement("html", getCssSelector("other"))),
        is(Validation.ErrorType.NONE));

    assertThat(
        self.validate(new ElementContext.Root(new TypeUtilities.FromString("test.Type"), elementType, selector)),
        is(Validation.ErrorType.NONE));
    assertThat(
        self.validate(
            new ElementContext.Root(new TypeUtilities.FromString("test.AnotherType"), elementType, selector)),
        is(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR));

    assertThat(
        self.validate(
            new ElementContext.Custom(
                "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(Validation.ErrorType.NONE));
    assertThat(
        self.validate(
            new ElementContext.Custom(
                "name", new TypeUtilities.FromString("test.AnotherType"), selector)),
        is(Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES));
  }

  @Test
  public void validateHtmlElement() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext self = getBasicElement("name", selector);
    assertThat(
        self.validate(new ElementContext.Root(new TypeUtilities.FromString("test.Type"), actionable, selector)),
        is(Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR));
    assertThat(
        self.validate(
            new ElementContext.Custom(
                "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR));
    assertThat(
        self.validate(getBasicElement("name2", getCssSelector("other"))),
        is(Validation.ErrorType.NONE));
  }
}
