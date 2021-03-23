package utam.compiler.translator;

import com.google.googlejavaformat.java.Formatter;
import com.google.googlejavaformat.java.FormatterException;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.Selector;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.core.selenium.element.LocatorUtilities.QUERY_SELECTOR;

/**
 * utilities for java code generation
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class TranslationUtilities {

  public static final String EMPTY_COMMENTS = "";
  static final String NEW_LINE = System.lineSeparator();
  static final String JAVADOC_LINE_PATTERN = "   * %s";
  static final String JAVADOC_OPEN_LINE = "  /**";
  static final String JAVADOC_CLOSE_LINE = "   */";
  static final String METHOD_JAVADOC_RETURNS_LINE = "@return %s";
  static final String METHOD_JAVADOC_PARAMETER_LINE = "@param %s %s";
  private static final String COMMENTS_SEPARATOR = NEW_LINE;
  private static final String ERR_METHOD_IS_EMPTY = "method '%s': implementation code is empty";
  private static final List<String> EMPTY_JAVADOC_LIST = new ArrayList<>();

  static List<String> getWrappedJavadoc(List<String> comments) {
    if (comments.isEmpty()) {
      return EMPTY_JAVADOC_LIST;
    }
    List<String> res = new ArrayList<>();
    res.add(JAVADOC_OPEN_LINE);
    if (comments.size() == 1) {
      // otherwise code formatter collapses javadoc into one unreadable string
      res.add(String.format(JAVADOC_LINE_PATTERN, ""));
    }
    for (String line : comments) {
      res.add(String.format(JAVADOC_LINE_PATTERN, line));
    }
    res.add(JAVADOC_CLOSE_LINE);
    return res;
  }

  static List<String> getWrappedJavadoc(String comments) {
    return getWrappedJavadoc(
        Stream.of(comments.split(COMMENTS_SEPARATOR)).collect(Collectors.toList()));
  }

  /**
   * todo this method is incomplete, it's supposed to get full chain for annotation
   *
   * @return string
   */
  public static String setHtmlElementComments(Selector selector, boolean isExpandScopeShadowRoot) {
    StringBuilder builder = new StringBuilder("javascript: $(<root selector here>)");
    appendQuery(builder, isExpandScopeShadowRoot, selector);

    return builder.toString();
  }

  private static void appendQuery(
      StringBuilder builder, boolean isExpandScopeShadowRoot, Selector selector) {
    if (isExpandScopeShadowRoot) {
      builder.append(".shadowRoot");
    }
    builder.append(String.format(QUERY_SELECTOR, handleSpecialChars(selector.getValue())));
  }

  static String handleSpecialChars(String selector) {
    return selector.replaceAll("\\*\\*", "\\\\*\\\\*\\\\");
  }

  static List<String> getMethodWrappedJavadoc(MethodDeclaration declaration) {
    return getWrappedJavadoc(getMethodJavadoc(declaration));
  }

  static List<String> getMethodJavadoc(MethodDeclaration declaration) {
    List<String> methodComments = new ArrayList<>();
    if (declaration.getComments().isEmpty()) {
      methodComments.add("method " + declaration.getName());
    } else {
      methodComments.add(declaration.getComments());
    }
    if (!declaration.getReturnType().isSameType(VOID)) {
      methodComments.add(
          String.format(METHOD_JAVADOC_RETURNS_LINE, declaration.getReturnType().getSimpleName()));
    }
    for (MethodParameter parameter : declaration.getParameters()) {
      methodComments.add(
          String.format(
              METHOD_JAVADOC_PARAMETER_LINE,
              parameter.getValue(),
              parameter.getType().getSimpleName()));
    }
    return methodComments;
  }

  static List<String> getWrappedClassJavadoc(String comments) {
    return getWrappedJavadoc(getClassJavadoc(comments));
  }

  static List<String> getClassJavadoc(String comments) {
    List<String> classJavadoc = new ArrayList<>();
    if (!comments.isEmpty()) {
      classJavadoc.add(comments);
    }
    classJavadoc.add(
        String.format(
            "@author UTAM %s",
            LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));
    return classJavadoc;
  }

  static String getImportString(TypeProvider type, String currentPackage) {
    if (type.getFullName().isEmpty()
        || type.getPackageName().isEmpty()
        || type.getFullName().startsWith("java.lang")
        || type.getPackageName().equals(currentPackage)) {
      return "";
    }
    return getStatement(String.format("import %s", type.getFullName()));
  }

  static String getStatement(String string) {
    if (string.isEmpty()) {
      return "";
    }
    if (string.endsWith("{") || string.endsWith("}")) {
      return string;
    }
    return string + ";";
  }

  static String applyJavaFormatter(List<String> in) {
    in.removeIf(String::isEmpty);
    String code = String.join(NEW_LINE, in);
    try {
      return new Formatter().formatSource(code);
    } catch (FormatterException e) {
      // add number at the beginning of the new line, helps process error message from formatter
      for (int i = 0; i < in.size(); i++) {
        in.set(i, String.format("%d > %s", i, in.get(i)));
      }
      throw new UtamError(e.getMessage() + "\n" + String.join(NEW_LINE, in));
    }
  }

  static String getPackageDeclaration(String packageName) {
    return getStatement("package " + packageName);
  }

  static String getLastStatement(PageObjectMethod m) {
    if (m.getCodeLines() == null || m.getCodeLines().isEmpty()) {
      throw new UtamError(ERR_METHOD_IS_EMPTY);
    }
    String string = m.getCodeLines().get(m.getCodeLines().size() - 1);
    if (!m.getDeclaration().getReturnType().isSameType(VOID)) {
      return getStatement(String.format("return %s", string));
    }
    return getStatement(string);
  }

  public static String getElementGetterMethodName(String elementName, boolean isPublic) {
    return "get"
            + elementName.substring(0, 1).toUpperCase()
            + elementName.substring(1)
            + (isPublic? "" : "Element");
  }
}
