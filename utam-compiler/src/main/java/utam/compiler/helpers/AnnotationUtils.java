/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.base.ElementMarker;
import utam.core.framework.base.PageMarker;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.appium.LocatorAccessibilityId;
import utam.core.selenium.appium.LocatorClassChain;
import utam.core.selenium.appium.LocatorUIAutomator;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public final class AnnotationUtils {

  /** A provider for an empty annotation */
  public static final AnnotationProvider EMPTY_ANNOTATION = () -> "";

  private static final TypeProvider MARKER_CLASS = new TypeUtilities.FromClass(PageMarker.class);
  private static final List<TypeProvider> MARKER_CLASS_LIST =
      Stream.of(MARKER_CLASS).collect(Collectors.toList());

  private static final List<TypeProvider> SELECTOR_CLASS_LIST =
      Stream.of(new TypeUtilities.FromClass(ElementMarker.class)).collect(Collectors.toList());
  private static final String MARKER_CLASS_STRING = PageMarker.class.getSimpleName();
  private static final List<TypeProvider> PLATFORM_ANNOTATION_IMPORTS =
      Stream.of(new TypeUtilities.FromClass(PlatformType.class), MARKER_CLASS)
          .collect(Collectors.toList());
  public static final AnnotationProvider DEPRECATED_ANNOTATION = () -> "@Deprecated";

  /**
   * Gets the provider for the annotation for a given locator
   *
   * @param selector the locator object containing the mechanism for selecting the element
   * @return the provider for creating the annotation for selecting the element
   */
  public static AnnotationProvider getPageObjectAnnotation(Locator selector) {
    String string =
        String.format(
            "@%s.%s(%s = %s)",
            MARKER_CLASS_STRING,
            PageMarker.Find.class.getSimpleName(),
            getFindAnnotationParameterName(selector),
            getWrappedString(selector.getStringValue()));
    return new Annotation(string, MARKER_CLASS_LIST);
  }

  private static String getFindAnnotationParameterName(Locator locator) {
    if (locator instanceof LocatorAccessibilityId) {
      return "accessid";
    }
    if (locator instanceof LocatorClassChain) {
      return "classchain";
    }
    if (locator instanceof LocatorUIAutomator) {
      return "uiautomator";
    }
    return "css";
  }

  /**
   * build annotation for an element declaration
   *
   * @param locator element locator
   * @param isExpand boolean indicator to expand a shadow root of the scope element
   * @param isNullable boolean nullable indicator
   * @return annotation provider instance
   */
  public static AnnotationProvider getFindAnnotation(
      Locator locator, boolean isExpand, boolean isNullable) {
    StringBuilder res =
        new StringBuilder(
            String.format(
                "@%s.%s(%s = %s",
                ElementMarker.class.getSimpleName(),
                ElementMarker.Find.class.getSimpleName(),
                getFindAnnotationParameterName(locator),
                getWrappedString(locator.getStringValue())));
    if (isExpand) {
      res.append(", expand = true");
    }
    if (isNullable) {
      res.append(", nullable = true");
    }
    res.append(")");
    return new Annotation(res.toString(), SELECTOR_CLASS_LIST);
  }

  /**
   * Gets the annotation provider for the page platform
   *
   * @param platformType platform in JSON file
   * @return the object providing the annotation for the given platform
   */
  public static AnnotationProvider getPagePlatformAnnotation(PlatformType platformType) {
    if (platformType == null) {
      return EMPTY_ANNOTATION;
    }
    String annotation =
        String.format(
            "@%s.%s(%s)",
            MARKER_CLASS_STRING,
            PageMarker.Switch.class.getSimpleName(),
            platformType.getAnnotation());
    return new Annotation(annotation, PLATFORM_ANNOTATION_IMPORTS);
  }

  static String getWrappedString(String string) {
    if (string.startsWith("\"")) {
      return string;
    }
    return String.format("\"%s\"", string.contains("\"") ? string.replace("\"", "\\\"") : string);
  }

  static class Annotation implements AnnotationProvider {

    private final String text;
    private final List<TypeProvider> imports;

    Annotation(String text, List<TypeProvider> imports) {
      this.text = text;
      this.imports = imports;
    }

    @Override
    public String getAnnotationText() {
      return text;
    }

    @Override
    public List<TypeProvider> getImportTypes() {
      return imports;
    }
  }
}
