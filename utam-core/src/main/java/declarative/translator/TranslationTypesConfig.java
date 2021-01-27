package declarative.translator;

import declarative.representation.TypeProvider;

/**
 * creates type of PO based on URI
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TranslationTypesConfig {

  /**
   * generate class type from URI
   *
   * @param pageObjectURI unique PO URI
   * @return type for class
   */
  TypeProvider getClassType(String pageObjectURI);

  /**
   * generate interface type from URI
   *
   * @param pageObjectURI po uri
   * @return type of the interface
   */
  TypeProvider getInterfaceType(String pageObjectURI);

  /**
   * get utility type
   * @param utilityURI string with utility type name
   * @return type of the utility
   */
  TypeProvider getUtilityType(String utilityURI);
}
