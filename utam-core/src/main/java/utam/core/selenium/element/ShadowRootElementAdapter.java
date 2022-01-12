/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.element.Element;

/**
 * wraps regular adapter to search inside its shadow root
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class ShadowRootElementAdapter extends ElementAdapter {

  /**
   * Initializes a new instance of the ShadowRootElementAdapter class
   *
   * @param elementAdapter the element adaptor to use
   */
  public ShadowRootElementAdapter(Element elementAdapter) {
    super(elementAdapter);
  }
}
