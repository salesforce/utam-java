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
import java.util.Set;
import java.util.stream.Collectors;
import utam.core.framework.consumer.UtamError;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import static utam.compiler.helpers.TypeUtilities.VOID;

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

  static Set<String> getImportStrings(TypeProvider typeToImport, String currentPackage) {
    return typeToImport.getImportableTypes()
        .stream()
        .filter(type -> isImportableType(type, currentPackage))
        .map(type -> getStatement(String.format("import %s", type.getFullName())))
        .collect(Collectors.toSet());
  }

  static boolean isImportableType(TypeProvider type, String currentPackage) {
    return isImportableType(type) && !type.getPackageName().equals(currentPackage);
  }

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

  public static String getElementGetterMethodName(String elementName, boolean isPublic) {
    return "get"
            + elementName.substring(0, 1).toUpperCase()
            + elementName.substring(1)
            + (isPublic? "" : "Element");
  }
}
