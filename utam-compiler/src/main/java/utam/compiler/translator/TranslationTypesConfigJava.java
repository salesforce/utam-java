/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.core.framework.consumer.PageObjectContextImpl.getDefaultImplType;

import java.util.Arrays;
import java.util.regex.Pattern;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.helpers.TypeUtilities.PageObjectType;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class TranslationTypesConfigJava implements TranslationTypesConfig {

  private static final String INCORRECT_PAGE_OBJECT_OR_UTILITY_TYPE =
      "type should have format '<namespace>/<pageobjects or utils>/[< optional"
          + " subPackage>/]<name>', actual was '%s'";

  // public because used in tests
  static String getWrongTypeError(String pageObjectURI) {
    return String.format(INCORRECT_PAGE_OBJECT_OR_UTILITY_TYPE, pageObjectURI);
  }

  private static String getJavaTypeName(String pageObjectURI) {
    String[] str = pageObjectURI.split(Pattern.quote("/"));
    if (str.length < 3) {
      throw new UtamError(getWrongTypeError(pageObjectURI));
    }
    String packageName = str[0].replaceAll("-", ".");
    // usually /pageObjects or /utils, but no longer enforced
    String secondPackageName = str[1].toLowerCase();
    String[] relativePath = Arrays.copyOfRange(str, 2, str.length);
    for (int i = 0; i < relativePath.length; i++) {
      if (i == relativePath.length - 1) {
        relativePath[i] = capitalizeFirstLetter(relativePath[i]);
      } else {
        relativePath[i] = relativePath[i].toLowerCase();
      }
    }
    final String relativeTypeName = String.join(".", relativePath);
    return String.format("%s.%s.%s", packageName, secondPackageName, relativeTypeName);
  }

  private static String capitalizeFirstLetter(String fileName) {
    return fileName.substring(0, 1).toUpperCase() + fileName.substring(1);
  }

  /**
   * Gets a value indicating whether a string is a Page Object type
   *
   * @param typeString the string to check
   * @return true if the string represents a Page Object type; otherwise, false
   */
  public static boolean isPageObjectType(String typeString) {
    try {
      getJavaTypeName(typeString);
      return true;
    } catch (UtamError e) {
      return false;
    }
  }

  @Override
  public TypeProvider getClassType(String pageObjectURI) {
    String implType = getDefaultImplType(getInterfaceType(pageObjectURI).getFullName());
    return new PageObjectType(implType);
  }

  @Override
  public TypeProvider getInterfaceType(String pageObjectURI) {
    return new PageObjectType(getJavaTypeName(pageObjectURI));
  }

  @Override
  public TypeProvider getUtilityType(String utilityURI) {
    return new FromString(getJavaTypeName(utilityURI));
  }
}
