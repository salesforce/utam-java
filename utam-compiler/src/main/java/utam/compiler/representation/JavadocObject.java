/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * Description at the level of the page object root or method, added as JavaDoc for generated page
 * object
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public abstract class JavadocObject {

  public static final String VERSION_TAG = "@version";

  private final List<String> javadoc;
  private final boolean isDeprecated;

  JavadocObject(List<String> javadoc, boolean isDeprecated) {
    this.javadoc = javadoc;
    this.isDeprecated = isDeprecated;
  }

  public List<String> getJavadoc() {
    return javadoc;
  }

  public boolean isDeprecated() {
    return isDeprecated;
  }

  static class EmptyJavadoc extends JavadocObject {

    EmptyJavadoc() {
      super(new ArrayList<>(), false);
    }
  }

  /**
   * Javadoc generated from method description
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class MethodJavadoc extends JavadocObject {

    public MethodJavadoc(
        String methodName,
        TypeProvider returnType,
        List<MethodParameter> parameters,
        UtamMethodDescription description) {
      super(
          buildMethodJavadoc(
              methodName,
              returnType,
              parameters,
              description.getText(),
              description.getReturnStr(),
              description.getThrowsStr(),
              description.getDeprecatedStr()),
          description.getDeprecatedStr() != null);
    }

    private static List<String> buildMethodJavadoc(
        String methodName,
        TypeProvider returnType,
        List<MethodParameter> parameters,
        List<String> text,
        String returnStr,
        String throwsStr,
        String deprecatedStr) {
      List<String> javadoc = new ArrayList<>();
      if (text == null || text.isEmpty()) {
        javadoc.add("method " + methodName);
      } else {
        javadoc.addAll(text);
      }
      if (returnStr != null) {
        javadoc.add(String.format("@return %s", returnStr));
      } else if (!returnType.isSameType(VOID)) {
        javadoc.add(String.format("@return %s", returnType.getSimpleName()));
      }
      parameters.stream()
          .filter(p -> !p.isLiteral())
          .forEach(
              parameter -> {
                final String parameterDescription =
                    parameter.getDescription() == null
                        ? parameter.getType().getSimpleName()
                        : parameter.getDescription();
                javadoc.add(
                    String.format("@param %s %s", parameter.getValue(), parameterDescription));
              });
      if (throwsStr != null) {
        javadoc.add(String.format("@throws %s", throwsStr));
      }
      if (deprecatedStr != null) {
        javadoc.add(String.format("@deprecated %s", deprecatedStr));
      }
      return javadoc;
    }
  }

  /**
   * Javadoc created for a page object
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class PageObjectJavadoc extends JavadocObject {

    public PageObjectJavadoc(
        TranslationContext context, List<String> text, String author, String deprecated) {
      super(buildPageObjectJavadoc(context, text, author, deprecated), deprecated != null);
    }

    private static List<String> buildPageObjectJavadoc(
        TranslationContext context, List<String> text, String author, String deprecated) {
      String version = context.getConfiguredVersion();
      // On Windows, sourceFileRelativePath may contain backslashes ("\"), which will
      // be misinterpreted in Javadoc comments by the Java source code formatter.
      // Replacing them with forward slashes ("/") ensures consistent generation of
      // Java files cross-platform.
      String sourceFileRelativePath = context.getJsonPath().replace("\\", "/");
      List<String> javadoc = new ArrayList<>(text);
      javadoc.add(String.format("created from JSON %s", sourceFileRelativePath));
      // add line @author team_name
      javadoc.add(String.format("@author %s", (author == null ? "UTAM" : author)));
      // add line @version, if not empty
      if (version != null && !version.isEmpty()) {
        javadoc.add(String.format("%s %s", VERSION_TAG, version));
      }
      // add line @deprecated
      if (deprecated != null) {
        javadoc.add(String.format("@deprecated %s", deprecated));
      }
      return javadoc;
    }
  }
}
