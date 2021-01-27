package framework.base;

import selenium.element.Actionable;

import java.util.List;
import java.util.function.Predicate;

/**
 * builder for page object element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public interface PageObjectElementBuilder {

  /**
   * set parameters in actionable
   *
   * @param type type of the actionable
   * @param values selector parameters values, can be empty
   * @param <T> element type
   * @return instance with parameters set in selector
   */
  <T extends Actionable> T build(Class<T> type, Object... values);

  /**
   * set parameters in actionable, then find it and apply filter to return first match
   *
   * @param type type of the actionable
   * @param filter to apply to found list
   * @param values selector parameters values, can be empty
   * @param <T> element type
   * @return instance with parameters set in selector
   */
  <T extends Actionable> T build(Class<T> type, Predicate<T> filter, Object... values);

  /**
   * set parameters in actionable, then find all elements and return list
   *
   * @param type type of the actionable
   * @param values selector parameters values, can be empty
   * @param <T> element type
   * @return list of instances with index in selector
   */
  <T extends Actionable> List<T> buildList(Class<T> type, Object... values);

  /**
   * set parameters in actionable, then find it and apply filter to return all elements that match
   *
   * @param type type of the actionable
   * @param filter to apply to found list
   * @param values selector parameters values, can be empty
   * @param <T> element type
   * @return instance with parameters set in selector
   */
  <T extends Actionable> List<T> buildList(Class<T> type, Predicate<T> filter, Object... values);
}
