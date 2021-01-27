package utam.core.selenium.element;

import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.List;
import java.util.Map;

/**
 * locator consists of multiple linked elements, this is one of them
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface LocatorNode {

  /**
   * filter to apply to found elements
   *
   * @return filter instance
   */
  Filter getFilter();

  /**
   * get next node, can be null
   *
   * @return instance of the next node or null
   */
  LocatorNode getNext();

  /**
   * get instance of the parent scope transformer
   *
   * @return instance of the scope transformer
   */
  Transformer getScopeTransformer();

  /**
   * get selector object for value
   *
   * @return selector as a string
   */
  Selector getSelector();

  /**
   * apply parameters to locator node
   *
   * @param parameters list of objects to apply as parameter
   */
  void setParameters(Locator.Parameters parameters);

  /**
   * set index for the node <br>
   * if index is 0, do nothing
   *
   * @param index index starting from 0
   */
  void setIndex(int index);

  /**
   * transforms search context of the scope
   *
   * @author elizaveta.ivanova
   * @since 230
   */
  interface Transformer {

    /**
     * apply transformation to the enclosing scope - expand shadow root
     *
     * @param searchContext enclosing scope
     * @param utilities web driver utilities
     * @return transformed scope
     */
    SearchContext apply(SearchContext searchContext, WebDriverUtilities utilities);
  }

  /**
   * filter applied to locator
   *
   * @author elizaveta.ivanova
   * @since 230
   */
  interface Filter {

    /**
     * get String representation of the filter
     *
     * @return String with filter
     */
    String getFilterString();

    /**
     * find first element that matches the filter
     *
     * @param found list of found elements to find match
     * @return pair of index (or -1) and element (or null) itself
     */
    List<Map.Entry<Integer, WebElement>> map(List<WebElement> found);

    List<WebElement> filter(List<WebElement> found);

    /**
     * check if current filter actually does anything
     *
     * @return true is it's not actionable
     */
    boolean isEmpty();
  }
}
