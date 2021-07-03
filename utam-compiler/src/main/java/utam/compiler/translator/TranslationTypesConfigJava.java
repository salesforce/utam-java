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
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class TranslationTypesConfigJava implements TranslationTypesConfig {

  private static final String ERR_WRONG_TYPE =
      "type should have format 'utam-<namespace>/%s/<package>/<name>' or 'utam-<namespace>/%s/<name>', actual was '%s'";

  // public because used in tests
  static String getWrongTypeError(String pageObjectURI, Mask maskValue) {
    return String.format(ERR_WRONG_TYPE, maskValue.name(), maskValue.name(), pageObjectURI);
  }

  static TypeProvider getJavaType(String pageObjectURI, Mask maskValue) {
    String[] str = pageObjectURI.split(Pattern.quote("/"));
    String mask = maskValue.name();
    if (str.length < 3) {
      throw new UtamError(getWrongTypeError(pageObjectURI, maskValue));
    }
    String[] prefix = str[0].split("-");
    if (prefix.length != 2 && !"utam".equals(prefix[0])) {
      throw new UtamError(getWrongTypeError(pageObjectURI, maskValue));
    }
    String packageName = str[0].replaceAll("-", ".");
    if (!mask.equals(str[1])) {
      throw new UtamError(getWrongTypeError(pageObjectURI, maskValue));
    }
    String[] relativePath = Arrays.copyOfRange(str, 2, str.length);
    for(int i = 0; i < relativePath.length; i++) {
      if (i == relativePath.length - 1) {
        relativePath[i] = capitalizeFirstLetter(relativePath[i]);
      } else {
        relativePath[i] = relativePath[i].toLowerCase();
      }
    }
    final String relativeTypeName = String.join(".", relativePath);
    return new TypeUtilities.FromString(
        String.format("%s.%s.%s", packageName, maskValue.name().toLowerCase(), relativeTypeName));
  }

  private static String capitalizeFirstLetter(String fileName) {
    return fileName.substring(0, 1).toUpperCase() + fileName.substring(1);
  }

  public static boolean isPageObjectType(String typeString) {
    try {
      getJavaType(typeString, Mask.pageObjects);
      return true;
    } catch (UtamError e) {
      return false;
    }
  }

  @Override
  public TypeProvider getClassType(String pageObjectURI) {
    String[] implType = getDefaultImplType(getInterfaceType(pageObjectURI).getFullName());
    return new FromString(implType[0], implType[1]);
  }

  @Override
  public TypeProvider getInterfaceType(String pageObjectURI) {
    return getJavaType(pageObjectURI, Mask.pageObjects);
  }

  @Override
  public TypeProvider getUtilityType(String utilityURI) {
    return getJavaType(utilityURI, Mask.utils);
  }

  enum Mask {
    pageObjects,
    utils
  }
}
