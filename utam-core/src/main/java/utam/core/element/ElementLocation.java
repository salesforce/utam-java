package utam.core.element;

import java.util.List;
import utam.core.framework.base.PageObjectsFactory;

/**
 * chain of locators for the element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface ElementLocation {

  ElementLocation scope(Locator locator, FindContext finderContext);

  ElementLocation setParameters(Object... parameters);

  String getLocatorChainString();

  Element findElement(PageObjectsFactory factory);

  List<Element> findElements(PageObjectsFactory factory);

  boolean isNullable();
}
