package declarative.helpers;

import declarative.representation.AnnotationProvider;
import declarative.representation.TypeProvider;
import framework.base.PageMarker;
import framework.context.PlatformType;
import selenium.element.ElementMarker;
import selenium.element.Selector;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public final class AnnotationUtils {

  public static final List<AnnotationProvider> EMPTY_ANNOTATIONS = new ArrayList<>();
  static final AnnotationProvider EMPTY_ANNOTATION = () -> "";
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

  public static AnnotationProvider getPageObjectAnnotation(Selector selector) {
    String string =
        String.format(
            "@%s.%s(%s = %s)",
            MARKER_CLASS_STRING,
            PageMarker.Find.class.getSimpleName(),
            getFindAnnotationParameterName(selector.getType()),
            getWrappedString(selector.getValue()));
    return new Annotation(string, MARKER_CLASS_LIST);
  }

  private static String getShadowHostMarkerString() {
    return String.format(
        "@%s.%s", MARKER_CLASS_STRING, PageMarker.isShadowHost.class.getSimpleName());
  }

  private static String getFindAnnotationParameterName(Selector.Type type) {
    return type.name().toLowerCase();
  }

  public static AnnotationProvider getFindAnnotation(
      Selector selector, ElementContext scopeElement, boolean isExpand) {
    StringBuilder res =
        new StringBuilder(
            String.format(
                "@%s.%s(%s = %s",
                ElementMarker.class.getSimpleName(),
                ElementMarker.Find.class.getSimpleName(),
                getFindAnnotationParameterName(selector.getType()),
                getWrappedString(selector.getValue())));
    if (!scopeElement.isRootScope()) {
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

  private static String getWrappedString(String string) {
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
