package utam.core.framework.consumer;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.selenium.element.Actionable;

/**
 * used to test loader config
 */
@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectOverride extends BasePageObject implements TestLoaderConfigPageObject {

  @Override
  public Actionable getRoot() {
    return getRootElement();
  }

  @Override
  public void load() {
    // nothing
  }
}
