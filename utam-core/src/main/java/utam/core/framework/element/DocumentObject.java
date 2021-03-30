package utam.core.framework.element;

import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.element.FindContext.Type;
import utam.core.element.Locator;
import utam.core.framework.base.PageObjectsFactory;

/**
 * implementation of the document object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DocumentObject implements Document {

  static final String DOM_READY_JAVASCRIPT = "document.readyState === 'complete'";

  private static final DriverExpectations<Boolean> isDOMReady =
      new DriverExpectations<>("wait for DOM to finish loading", driver ->
          (Boolean) driver.executeScript(DOM_READY_JAVASCRIPT)
      );
  private final Driver driver;

  public DocumentObject(PageObjectsFactory factory) {
    this.driver = factory.getDriver();
  }

  @Override
  public String getUrl() {
    return driver.getUrl();
  }

  @Override
  public void waitForDocumentReady() {
    driver.waitFor(isDOMReady, null);
  }

  @Override
  public boolean containsElement(Locator locator) {
    return driver.findElements(locator, Type.NULLABLE).size() > 0;
  }
}
