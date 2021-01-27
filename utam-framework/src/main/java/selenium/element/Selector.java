package selenium.element;

/**
 * combines selector string with its type into one object used to generate selector and annotation
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface Selector {

  /**
   * get String value of the selector
   * @return selector string
   */
  String getValue();

  /**
   * get selector type
   * @return one of the supported types
   */
  Type getType();

  enum Type {
    CSS,
    ACCESSID,
    CLASSCHAIN,
    UIAUTOMATOR
  }
}
