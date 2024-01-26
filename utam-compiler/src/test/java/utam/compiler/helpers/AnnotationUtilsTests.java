/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static utam.compiler.grammar.TestUtilities.getCssSelector;

import java.util.Set;
import java.util.stream.Collectors;
import org.testng.annotations.Test;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.base.ElementMarker;
import utam.core.framework.base.PageMarker;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.element.LocatorBy;

/**
 * Provides tests for the ClassAnnotationProvider class
 *
 * @author james.evans
 */
public class AnnotationUtilsTests {

  private static final String ELEMENT_MARKER_ANNOTATION_CLASS = ElementMarker.class.getName();
  private static final String PAGE_OBJECT_ANNOTATION_CLASS = PageMarker.class.getName();

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
        AnnotationUtils.getFindAnnotation(getCssSelector(".fakeSelector"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(css = \".fakeSelector\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  @Test
  public void testGetFindAnnotationNullable() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(getCssSelector(".fakeSelector"), true, true);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(css = \".fakeSelector\", expand = true, nullable = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: Accessibility ID selector */
  @Test
  public void testGetFindAnnotationAccessId() {
    Locator selector = LocatorBy.byAccessibilityId("fakeSelector");
    AnnotationProvider provider = AnnotationUtils.getFindAnnotation(selector, true, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(accessid = \"fakeSelector\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: iOS Class Chain selector */
  @Test
  public void testGetFindAnnotationClassChain() {
    Locator selector = LocatorBy.byClassChain("fakeSelector");
    AnnotationProvider provider = AnnotationUtils.getFindAnnotation(selector, true, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(classchain = \"fakeSelector\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method should return the proper value: Android UIAutomator selector */
  @Test
  public void testGetFindAnnotationUIAutomator() {
    Locator selector = LocatorBy.byUiAutomator("new UiSelector().checked(true)");
    AnnotationProvider provider = AnnotationUtils.getFindAnnotation(selector, true, false);
    assertThat(
        provider.getAnnotationText(),
        is(
            equalTo(
                "@ElementMarker.Find(uiautomator = \"new UiSelector().checked(true)\", expand ="
                    + " true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method with an empty element should return the proper value */
  @Test
  public void testGetFindAnnotationWithEmptyElement() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(getCssSelector(".fakeSelector"), true, false);
    assertThat(
        provider.getAnnotationText(),
        is(equalTo("@ElementMarker.Find(css = \".fakeSelector\", expand = true)")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  /** The getFindAnnotation method with a quoted string should return the proper value */
  @Test
  public void testGetFindAnnotationWithQuotedString() {
    AnnotationProvider provider =
        AnnotationUtils.getFindAnnotation(getCssSelector(".fakeSelector"), false, false);
    assertThat(
        provider.getAnnotationText(), is(equalTo("@ElementMarker.Find(css = \".fakeSelector\")")));
    assertThat(getImportedTypes(provider), containsInAnyOrder(ELEMENT_MARKER_ANNOTATION_CLASS));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithWeb() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation(PlatformType.WEB).getAnnotationText(),
        is(equalTo("@PageMarker.Switch(PlatformType.WEB)")));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithNative() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation(PlatformType.NATIVE).getAnnotationText(),
        is(equalTo("@PageMarker.Switch(PlatformType.NATIVE)")));
  }

  @Test
  public void testGetPagePlatformAnnotiationWithEmptyValue() {
    assertThat(
        AnnotationUtils.getPagePlatformAnnotation(null).getAnnotationText(), is(emptyString()));
  }

  private Set<String> getImportedTypes(AnnotationProvider provider) {
    return provider.getImportTypes().stream()
        .map(TypeProvider::getFullName)
        .collect(Collectors.toSet());
  }
}
