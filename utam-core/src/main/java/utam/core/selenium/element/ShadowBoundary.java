package utam.core.selenium.element;

import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

/**
 * transforms search context to expand shadow root
 *
 * @author elizaveta.ivanova
 * @since 226
 */
enum ShadowBoundary implements LocatorNode.Transformer {
  NONE,
  EXPAND_SHADOW_ROOT;

  public static ShadowBoundary valueOf(boolean isExpandShadow) {
    return isExpandShadow ? ShadowBoundary.EXPAND_SHADOW_ROOT : NONE;
  }

  @Override
  public final SearchContext apply(SearchContext searchContext, WebDriverUtilities utilities) {
    if (!(searchContext instanceof WebElement)) {
      return searchContext;
    }
    if (this == NONE) {
      return searchContext;
    }
    // Context must be EXPAND_SHADOW_ROOT (enumerated type cannot be extended)
    return utilities.expandShadowRoot(searchContext);
  }
}
