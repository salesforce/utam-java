package utam.core.framework.consumer.impl;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.selenium.element.Actionable;

@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectImpl extends BasePageObject implements TestLoaderConfigPageObject {

  @Override
  public Actionable getRoot() {
    return getRootElement();
  }

  @Override
  public void load() {
    // nothing
  }
}
