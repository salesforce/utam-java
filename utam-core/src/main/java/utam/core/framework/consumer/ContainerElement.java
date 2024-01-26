/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.util.List;
import utam.core.element.Locator;
import utam.core.framework.base.PageObject;

/**
 * Element that can be used as scope to load any UTAM page object with a given type.
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ContainerElement {

  /**
   * Load UTAM Page object using current element as scope
   *
   * @param utamPageObjectType type to load
   * @param injectRootLocator inject root into created page object instance
   * @param <T> UTAM page object type
   * @return UTAM page object instance
   */
  <T extends PageObject> T load(Class<T> utamPageObjectType, Locator injectRootLocator);

  /**
   * Load list of UTAM Page object using current element as a scope
   *
   * @param utamPageObjectType type to load
   * @param injectRootLocator inject root into created page object instance
   * @param <T> UTAM page object type
   * @return UTAM page object instance
   */
  <T extends PageObject> List<T> loadList(Class<T> utamPageObjectType, Locator injectRootLocator);

  /**
   * Inject container element as a root into external non-UTAM page object that should be scoped
   * inside the container element
   *
   * @param externalObjectInsideContainer external (non-UTAM page object that should be loaded
   *     inside UTAM container)
   * @deprecated not supported outside Salesforce engineering teams. Compatibility mode and Selenium
   *     only.
   */
  @Deprecated
  void setScope(Contained externalObjectInsideContainer);

  /**
   * Load UTAM Page Object
   *
   * @param utamType type to load
   * @param injectCss CSS to inject
   * @param <T> class type
   * @return the Page Object instance
   * @deprecated use methods with locator as a parameter
   */
  @Deprecated
  <T extends PageObject> T load(Class<T> utamType, String injectCss);
}
