/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import com.google.googlejavaformat.java.Formatter;
import com.google.googlejavaformat.java.FormatterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import utam.core.declarative.representation.TypeProvider;

/**
 * utilities for java code generation
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class TranslationUtilities {

  static final String NEW_LINE = System.lineSeparator();
  static final String JAVADOC_LINE_PATTERN = "   * %s";
  static final String JAVADOC_OPEN_LINE = "  /**";
  static final String JAVADOC_CLOSE_LINE = "   */";

  /**
   * Wrap comments into Javadoc
   *
   * @param comments comments text
   * @return javadoc as list of strings
   */
  static List<String> getWrappedJavadoc(List<String> comments) {
    if (comments.isEmpty()) {
      return comments;
    }
    List<String> res = new ArrayList<>();
    res.add(JAVADOC_OPEN_LINE);
    if (comments.size() == 1) {
      // otherwise code formatter collapses javadoc into one unreadable string
      res.add(String.format(JAVADOC_LINE_PATTERN, ""));
    }
    for (String line : comments) {
      res.add(String.format(JAVADOC_LINE_PATTERN, formatJavadoc(line)));
    }
    res.add(JAVADOC_CLOSE_LINE);
    return res;
  }

  /**
   * Replace any invalid HTML characters with their appropriate encoded entities
   *
   * @param str input string
   * @return string for javadoc content
   */
  static String formatJavadoc(String str) {
    return str.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        // iOS class chains contain "*/" in their paths, must be escaped for Javadoc generation
        .replace("*/", "*&#47");
  }

  static Set<String> getImportStrings(TypeProvider typeToImport, String currentPackage) {
    return typeToImport.getImportableTypes().stream()
        .filter(type -> isImportableType(type, currentPackage))
        .map(type -> getStatement(String.format("import %s", type.getFullName())))
        .collect(Collectors.toSet());
  }

  static boolean isImportableType(TypeProvider type, String currentPackage) {
    return isImportableType(type) && !type.getPackageName().equals(currentPackage);
  }

  /**
   * Gets a value indicating if a give type provider is an importable type
   *
   * @param type the type provider to check
   * @return true if the type provider is importable; otherwise false
   */
  public static boolean isImportableType(TypeProvider type) {
    return !type.getFullName().isEmpty()
        && !type.getPackageName().isEmpty()
        && !type.getFullName().startsWith("java.lang")
        && !type.getImportableTypes().isEmpty();
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
      throw new UtamRunnerError(
          String.format(
              "Generated Java code can't be formatted, error: \"%s\".\n"
                  + "Copy code to IDE to see the issue, most common issue is invalid element name"
                  + " that can't be made into Java variable.\n\n"
                  + "%s",
              e.getMessage(), code));
    }
  }

  static String getPackageDeclaration(String packageName) {
    return getStatement("package " + packageName);
  }

  /**
   * Gets the name of an element getter method
   *
   * @param elementName the name of the element
   * @param isPublic a value indicating whether the element is public
   * @return the name of the element getter method
   */
  public static String getElementGetterMethodName(String elementName, boolean isPublic) {
    return "get"
        + elementName.substring(0, 1).toUpperCase()
        + elementName.substring(1)
        + (isPublic ? "" : "Element");
  }
}
