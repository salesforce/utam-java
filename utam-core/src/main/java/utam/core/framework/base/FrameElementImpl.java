/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.element.Element;
import utam.core.framework.consumer.FrameElement;
import utam.core.framework.element.BasePageElement;

/**
 * element representing a frame or iframe
 *
 * @author james.evans
 * @since 236
 */
public class FrameElementImpl extends BasePageElement implements FrameElement {

  public FrameElementImpl() {
    super();
  }

  @Override
  public Element getFrameElement() {
    return getElement();
  }
}
