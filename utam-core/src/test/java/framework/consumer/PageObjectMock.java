package framework.consumer;

import framework.base.RootPageObject;
import selenium.element.Actionable;

/**
 * used in tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface PageObjectMock extends RootPageObject {
  Actionable getRoot();
}
