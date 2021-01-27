package selenium.element;

/**
 * UI element locator <br>
 * it is wrapper around selector, represents tree structure
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Locator {

  /**
   * clone current instance and add a leaf
   *
   * @param leaf locator element that will be added as a leaf
   * @return clone with added leaf
   */
  Locator add(LocatorNode leaf);

  /**
   * clone current instance and add all elements from scoped locator as leafs
   *
   * @param next locator with elements to be added
   * @return clone with added elements
   */
  Locator scope(Locator next);

  /**
   * set index for the leaf <br>
   * if index is 0, do nothing, just return same instance
   *
   * @param index index starting from 0
   * @return if index > 0 return clone with applied index
   */
  Locator setIndex(int index);

  /**
   * apply parameters to locator
   *
   * @param parameters list of objects to apply as parameter
   * @return copy of the locator with applied parameters
   */
  Locator setParameters(Parameters parameters);

  /**
   * Parameters applied to Locator, like runtime String or Index
   *
   * @since 230
   */
  interface Parameters {

    /**
     * check if parameters are present
     *
     * @return true if none is set
     */
    boolean isEmpty();

    /**
     * apply parameters to the string with selector or filter
     *
     * @param target string before applied parameters
     * @return String with applied parameters
     */
    String apply(String target);
  }
}
