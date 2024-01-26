/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.lang.reflect.Constructor;
import utam.core.framework.consumer.UtamError;

/**
 * provides imperative utility instance
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class ImperativeProvider<T extends PageObject> implements ImperativeExtension<T> {

  private static final String ERR_CANT_CREATE_UTILITY =
      "can't create instance of imperative utility %s";
  private T instance;

  /**
   * Initializes a new instance of the ImperativeProvider class
   *
   * @param instance the class instance to use
   */
  protected ImperativeProvider(T instance) {
    this.instance = instance;
  }

  /** Initializes a new instance of the ImperativeProvider class */
  protected ImperativeProvider() {}

  /**
   * Builds an imperative provider for the appropriate type
   *
   * @param type the class to instantiate as the imperative extensions class
   * @param <T> the type of the imperative extensions class
   * @return an instance of the class as an imperative provider
   */
  public static <T extends ImperativeProvider> T build(Class<T> type) {
    try {
      Constructor<T> constructor = type.getDeclaredConstructor();
      constructor.setAccessible(true);
      return constructor.newInstance();
    } catch (Exception e) {
      throw new UtamError(String.format(ERR_CANT_CREATE_UTILITY, type.getName()), e);
    }
  }

  @Override
  public final T getInstance() {
    return instance;
  }

  /**
   * Sets the instance of an imperative provider
   *
   * @param instance the instance of the imperative extensions class to use
   */
  public void setInstance(T instance) {
    this.instance = instance;
  }
}
