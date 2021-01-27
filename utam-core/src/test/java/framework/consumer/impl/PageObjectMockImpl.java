package framework.consumer.impl;

import framework.base.BasePageObject;
import framework.base.PageMarker;
import framework.consumer.PageObjectMock;
import selenium.element.Actionable;

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
