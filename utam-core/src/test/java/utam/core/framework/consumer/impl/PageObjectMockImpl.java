package utam.core.framework.consumer.impl;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.PageObjectMock;
import utam.core.selenium.element.Actionable;

@PageMarker.Find(css = "root")
public class PageObjectMockImpl extends BasePageObject implements PageObjectMock {

  @Override
  public Actionable getRoot() {
    return getRootElement();
  }

  @Override
  public void load() {
    // nothing
  }
}
