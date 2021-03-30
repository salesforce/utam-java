package utam.core.driver;

import utam.core.element.Locator;

/**
 * document object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Document {

  String getUrl();

  void waitForDocumentReady();

  boolean containsElement(Locator locator);
}
