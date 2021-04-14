/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Clickable;

import java.util.List;

/**
 * this enum links element actions with translator code <br>
 *
 * @see Clickable as of 228 every method has enum here to use in translator
 * @author elizaveta.ivanova
 * @since 226
 */
public enum ClickableActionType implements ActionType {
  /**
   * click on the element using WebElement.click <br>
   * throws exception if fails
   */
  click(),
  /**
   * executes javascript "arguments[0].click();" to trick Selenium into clicking on the element that
   * is not considered clickable by the Web Driver. It's a workaround for inconsistent behavior of
   * some browsers. Otherwise same as "click"
   */
  javascriptClick();

  // used in unit tests
  Class[] getParameterClasses() {
    return new Class[0];
  }

  // used in unit tests
  Class getElementClass() {
    return Clickable.class;
  }

  @Override
  public TypeProvider getReturnType() {
    return VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes() {
    return Collections.EMPTY_LIST;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
