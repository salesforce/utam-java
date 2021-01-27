package utam.core.declarative.translator;

import java.io.IOException;
import java.io.Reader;
import java.util.Collection;

/**
 * configuration of source with JSON files
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface TranslatorSourceConfig {

  /**
   * read declarative file by name of the PO
   *
   * @param pageObjectURI name of the PO
   * @return reader for file with json declaration
   */
  Reader getDeclarationReader(String pageObjectURI) throws IOException;

  /**
   * get all configured JSON source files with page objects <br>
   * each returned string is Page Object URI like utam-global/pageObjects/global/Name
   *
   * @return collection with unique Page Objects URIs
   */
  Collection<String> getPageObjects();

  /**
   * mapping between folder where JSON is located and Page Object type name, ex. "force" maps to
   * "utam-force" <br>
   * mapping is typically stored in properties file
   *
   * @param folder folder where JSON file was found
   * @return mapping to the package
   */
  String getPackageMapping(String folder);
}
