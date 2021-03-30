package utam.core.framework.consumer.impl;

import utam.core.element.ElementLocation;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.TestLoaderConfigPageObject;

@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectImpl extends BasePageObject implements
    TestLoaderConfigPageObject {

  @Override
  public ElementLocation getRoot() {
    return getRootLocator();
  }

  @Override
  public void load() {
    // nothing
  }
}
