package utam.core.selenium.element;

import utam.core.framework.base.PageObjectBuilderImpl;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.LocationPolicy;

import static utam.core.selenium.element.LocatorUtilities.buildLocator;

/**
 * element exposed as container
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class ElementContainerImpl extends ElementImpl implements ContainerElement {

  private final PageObjectsFactory factory;
  private final Locator locator;
  private final boolean isExpandShadow;

  ElementContainerImpl(Locator locator, PageObjectsFactory factory, boolean isExpandShadow) {
    super(locator, factory.getSeleniumContext());
    this.factory = factory;
    this.isExpandShadow = isExpandShadow;
    this.locator = locator;
  }

  @Override
  public void setScope(Contained pageObject) {
    pageObject.setScope(() -> find(isExpandShadow));
  }

  @SuppressWarnings("unused")
  @Override
  public <T extends PageObject> T load(Class<T> utamType, String injectCss) {
    return load(utamType, Web.byCss(injectCss));
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamType, Selector selector) {
    LocationPolicy policy = factory.getSeleniumContext().getLocationPolicy();
    return new PageObjectBuilderImpl(factory, locator, false, buildLocator(policy, selector, isExpandShadow))
        .build(utamType);
  }

  @Override
  public boolean isExpandScopeShadow() {
    return isExpandShadow;
  }
}
