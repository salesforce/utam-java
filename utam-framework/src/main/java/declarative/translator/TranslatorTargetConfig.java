package declarative.translator;

import declarative.representation.TypeProvider;

import java.io.IOException;
import java.io.Writer;

/**
 * configuration of source with JSON files and target for generated Java files
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface TranslatorTargetConfig {

  /**
   * writer for generated type
   *
   * @param typeProvider type of the PO, can be interface or class
   * @return writer for class
   */
  Writer getClassWriter(TypeProvider typeProvider) throws IOException;

  /**
   * get unit test writer to write unit test generated for new page object
   *
   * @param typeProvider type of the Page Object to generate test for
   * @return writer
   * @throws IOException if file does not exist
   */
  Writer getUnitTestWriter(TypeProvider typeProvider) throws IOException;

  /**
   * full path to the injection configuration files directory <br>
   * translator will write into dependency configs <br>
   * then Page Objects Provider will read from those to configure dependencies
   *
   * @return string with full path to resources folder with configs
   */
  String getInjectionConfigRootFilePath();
}
