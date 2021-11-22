/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.base.PageMarker;
import org.testng.annotations.Test;
import utam.core.framework.base.ElementMarker;

import java.util.Set;
import java.util.stream.Collectors;
import utam.core.selenium.element.LocatorBy;

import static utam.compiler.grammar.TestUtilities.TEST_PAGE_OBJECT;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ElementContext.EMPTY_SELECTOR;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.types.BasicElementInterface.actionable;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the ClassAnnotationProvider class
 *
 * @author james.evans
 */
public class AnnotationUtilsTests {

  private static final String ELEMENT_MARKER_ANNOTATION_CLASS = ElementMarker.class.getName();
  private static final String PAGE_OBJECT_ANNOTATION_CLASS = PageMarker.class.getName();

  private static ElementContext getBasicElement(String value) {
    return new ElementContext.Basic(
        null,
        value,
        actionable,
        EMPTY_SELECTOR,
        EMPTY_PARAMETERS,
        false);
  }

  /** The getSelectorAnnotation method should return the proper value */
  @Test
  public void testGetSelectorAnnotation() {
    AnnotationProvider provider =
        AnnotationUtils.getPageObjectAnnotation(getCssSelector(".fakeSelector"));
    assertThat(
        provider.getAnnotationText(), is(equalTo("@PageMarker.Find(css = \".fakeSelector\")")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(PAGE_OBJECT_ANNOTATION_CLASS));
  }

  /** The getSelectorAnnotation method should return the proper value */
  @Test
  public void testGetSelectorAnnotationWithQuotedText() {
    AnnotationProvider provider =
        AnnotationUtils.getPageObjectAnnotation(getCssSelector(".fakeSelector[title=\"foo\"]"));
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@PageMarker.Find(css = \".fakeSelector[title=\\\"foo\\\"]\")")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(PAGE_OBJECT_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: css selector */
  @Test
  public void testGetFindAnnotationCss() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(
            getCssSelector(".fakeSelector"), getBasicElement("fakeElement"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(css = \".fakeSelector\", scope = \"fakeElement\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  @Test
  public void testGetFindAnnotationNullable() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(
            getCssSelector(".fakeSelector"), getBasicElement("fakeElement"), true, true);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(css = \".fakeSelector\", scope = \"fakeElement\", expand = true, nullable = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: Accessibility ID selector */
  @Test
  public void testGetFindAnnotationAccessId() {
    Locator selector = LocatorBy.byAccessibilityId("fakeSelector");
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(selector, getBasicElement("fakeElement"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(accessid = \"fakeSelector\", scope = \"fakeElement\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: iOS Class Chain selector */
  @Test
  public void testGetFindAnnotationClassChain() {
    Locator selector = LocatorBy.byClassChain("fakeSelector");
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(selector, getBasicElement("fakeElement"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(classchain = \"fakeSelector\", scope = \"fakeElement\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: Android UIAutomator selector */
  @Test
  public void testGetFindAnnotationUIAutomator() {
    Locator selector = LocatorBy.byUiAutomator("new UiSelector().checked(true)");
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(selector, getBasicElement("fakeElement"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(uiautomator = \"new UiSelector().checked(true)\", scope = \"fakeElement\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method with an empty element should return the proper value */
  @Test
  public void testGetFindAnnotationWithEmptyElement() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(
            getCssSelector(".fakeSelector"),
            new ElementContext.Root(TEST_PAGE_OBJECT, null, null), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(css = \".fakeSelector\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method with a quoted string should return the proper value */
  @Test
  public void testGetFindAnnotationWithQuotedString() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(
            getCssSelector(".fakeSelector"), getBasicElement("\"scopeElement\""), false, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(css = \".fakeSelector\", scope = \"scopeElement\")")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithWeb() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation("web").getAnnotationText(),
        is(equalTo("@PageMarker.Switch(PlatformType.WEB)")));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithNative() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation("native")
            .getAnnotationText(),
        is(equalTo("@PageMarker.Switch(PlatformType.NATIVE)")));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithEmptyValue() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation("").getAnnotationText(), is(emptyString()));
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation(null).getAnnotationText(), is(emptyString()));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithInvalidValueThrows() {
    IllegalArgumentException e =
        expectThrows(
            IllegalArgumentException.class,
            () -> AnnotationUtils.getPagePlatformAnnotation("unknown"));
    assertThat(e.getMessage(), containsString("Unknown platform type 'unknown'"));
  }

  private Set<String> getImportedTypes(AnnotationProvider provider) {
    return provider.getImportTypes().stream()
        .map(TypeProvider::getFullName)
        .collect(Collectors.toSet());
  }
}
