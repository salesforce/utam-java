/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.framework.base;

/**
 * base interface for all utam page objects <br>
 * methods have default implementations to mock external POs
 *
 * @author elizaveta.ivanova
 * @since 218
 */
public interface PageObject {

  /**
   * actions in this method will be performed when page object is loaded from loader <br>
   * by default it's checking for visibility of its root element
   */
  default void load() {}

  /**
   * check for presence of the page object root element inside its scope
   *
   * @return true if present, false otherwise
   */
  default boolean isPresent() {
    return false;
  }
}
