package utam.core.framework.consumer;

import utam.core.element.ElementLocation;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;

/**
 * used to test loader config
 */
@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectIOS extends BasePageObject implements TestLoaderConfigPageObject {

  @Override
  public ElementLocation getRoot() {
    return super.getRootLocator();
  }

  @Override
  public void load() {
    // nothing
  }
}
