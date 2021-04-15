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

  public static final AnnotationProvider EMPTY_ANNOTATION = () -> "";
  private static final TypeProvider MARKER_CLASS = new TypeUtilities.FromClass(PageMarker.class);
  private static final List<TypeProvider> MARKER_CLASS_LIST =
      Stream.of(MARKER_CLASS).collect(Collectors.toList());

  private static final List<TypeProvider> SELECTOR_CLASS_LIST =
      Stream.of(new TypeUtilities.FromClass(ElementMarker.class)).collect(Collectors.toList());
  private static final String MARKER_CLASS_STRING = PageMarker.class.getSimpleName();

  public static AnnotationProvider getShadowHostAnnotation(boolean isShadowHost) {
    if (!isShadowHost) {
      return EMPTY_ANNOTATION;
    }
    return new Annotation(getShadowHostMarkerString(), MARKER_CLASS_LIST);
  }

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

  private static String getShadowHostMarkerString() {
    return String.format(
        "@%s.%s", MARKER_CLASS_STRING, PageMarker.isShadowHost.class.getSimpleName());
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

  public static AnnotationProvider getFindAnnotation(
      Locator locator, ElementContext scopeElement, boolean isExpand) {
    StringBuilder res =
        new StringBuilder(
            String.format(
                "@%s.%s(%s = %s",
                ElementMarker.class.getSimpleName(),
                ElementMarker.Find.class.getSimpleName(),
                getFindAnnotationParameterName(locator),
                getWrappedString(locator.getStringValue())));
    if (scopeElement != null && !scopeElement.isRootElement()) {
      res.append(String.format(", scope = %s", getWrappedString(scopeElement.getName())));
    }
    if (isExpand) {
      res.append(", expand = true");
    }
    res.append(")");
    return new Annotation(res.toString(), SELECTOR_CLASS_LIST);
  }

  public static AnnotationProvider getPagePlatformAnnotation(String string) {
    PlatformType pagePlatform = PlatformType.fromString(string);
    if (pagePlatform == PlatformType.NONE) {
      return EMPTY_ANNOTATION;
    }
    String annotation =
        String.format(
            "@%s.%s(%s)",
            MARKER_CLASS_STRING,
            PageMarker.Switch.class.getSimpleName(),
            pagePlatform.getAnnotation());
    return new Annotation(
        annotation,
        Stream.of(MARKER_CLASS, new TypeUtilities.FromClass(PlatformType.class))
            .collect(Collectors.toList()));
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
