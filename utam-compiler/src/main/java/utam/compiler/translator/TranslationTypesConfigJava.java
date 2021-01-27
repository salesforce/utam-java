package utam.compiler.translator;

import declarative.translator.TranslationTypesConfig;
import utam.compiler.helpers.TypeUtilities;
import declarative.representation.TypeProvider;
import framework.consumer.UtamError;

import java.util.regex.Pattern;

import static framework.consumer.PageObjectContextImpl.getDefaultImplType;

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
    if (str.length < 3 || str.length > 4) {
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
    final String relativeTypeName = str.length == 4
        ? String.format("%s.%s", str[2].toLowerCase(), capitalizeFirstLetter(str[3]))
        : capitalizeFirstLetter(str[2]);
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
    String[] types = getDefaultImplType(getInterfaceType(pageObjectURI).getFullName());
    return new TypeUtilities.FromString(types[0], types[1]);
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
