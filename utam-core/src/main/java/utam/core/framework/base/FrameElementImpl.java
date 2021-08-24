/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.framework.element.BasePageElement;

/**
 * element representing a frame or iframe
 *
 * @author james.evans
 * @since 236
 */
public class FrameElementImpl extends BasePageElement implements FrameElement {

  public FrameElementImpl() {}

  public static FrameElement createFrameInstance(Element element, PageObjectsFactory factory) {
    return createInstance(FrameElementImpl.class, element, factory);
  }

  @Override
  public Element getFrameElement() {
    return getElement();
  }
}
