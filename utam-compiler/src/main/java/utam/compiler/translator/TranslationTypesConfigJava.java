/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.core.framework.consumer.PageObjectContextImpl.getDefaultImplType;

import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class TranslationTypesConfigJava implements TranslationTypesConfig {

  private static final String INCORRECT_PAGE_OBJECT_OR_UTILITY_TYPE =
      "type should have format '<namespace>/<pageobjects or utils>/[< optional subPackage>/]<name>', actual was '%s'";

  // public because used in tests
  static String getWrongTypeError(String pageObjectURI) {
    return String.format(INCORRECT_PAGE_OBJECT_OR_UTILITY_TYPE, pageObjectURI);
  }

  static String getJavaTypeName(String pageObjectURI) {
    String[] str = pageObjectURI.split(Pattern.quote("/"));
    if (str.length < 3) {
      throw new UtamError(getWrongTypeError(pageObjectURI));
    }
    String packageName = str[0].replaceAll("-", ".");
    // usually /pageObjects or /utils, but no longer enforced
    String secondPackageName = str[1].toLowerCase();
    String[] relativePath = Arrays.copyOfRange(str, 2, str.length);
    for(int i = 0; i < relativePath.length; i++) {
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
    String[] implType = getDefaultImplType(getInterfaceType(pageObjectURI).getFullName());
    return new CustomType(implType[0], implType[1]);
  }

  @Override
  public TypeProvider getInterfaceType(String pageObjectURI) {
    return new CustomType(getJavaTypeName(pageObjectURI));
  }

  @Override
  public TypeProvider getUtilityType(String utilityURI) {
    return new CustomType(getJavaTypeName(utilityURI));
  }

  static class CustomType extends FromString {

    CustomType(String fullName) {
      super(fullName);
    }

    CustomType(String name, String fullName) {
      super(name, fullName);
    }
  }

  public static boolean isCustomType(TypeProvider type) {
    if(type == null) {
      return false;
    }
    if(type instanceof CustomType) {
      return true;
    }
    if(type instanceof ListOf) {
      return type.getBoundTypes().get(0) instanceof CustomType;
    }
    return false;
  }
}
