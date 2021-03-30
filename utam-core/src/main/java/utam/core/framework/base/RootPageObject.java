package utam.core.framework.base;

import static utam.core.element.FindContext.Type.EXISTING;

import utam.core.element.ElementLocation;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.ElementLocationChain;

/**
 * PO that can be loaded directly from context
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface RootPageObject extends PageObject {

  default ElementLocation setRootLocator() {
    if (!getClass().isAnnotationPresent(PageMarker.Find.class)) {
      throw new UtamError(String.format("root selector is not set for the page object instance %s",
          getClass().getName()));
    }
    PageMarker.Find annotation = getClass().getDeclaredAnnotation(PageMarker.Find.class);
    return new ElementLocationChain(PageMarker.getRootLocator(annotation), EXISTING);
  }
}
