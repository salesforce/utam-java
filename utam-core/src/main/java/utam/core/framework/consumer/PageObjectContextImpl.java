/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;

/**
 * context to build page object from interface class
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class PageObjectContextImpl implements PageObjectContext {

  static final String ERR_GET_IMPL_BY_NAME = "can't find implementation for %s";
  static final String ERR_GET_INSTANCE_BY_NAME = "can't create instance of type '%s'";

  private final Map<Class<? extends PageObject>, Class> activeBeans;
  private final Map<Class<? extends PageObject>, Class> defaultBeans;

  /**
   * Initializes a new instance of the PageObjectContextImpl class
   *
   * @param activeBeans the active beans in the Page Object
   * @param defaultBeans beans for default profile
   */
  public PageObjectContextImpl(
      Map<Class<? extends PageObject>, Class> activeBeans,
      Map<Class<? extends PageObject>, Class> defaultBeans) {
    this.activeBeans = activeBeans;
    this.defaultBeans = defaultBeans;
  }

  /**
   * Initializes a new instance of the PageObjectContextImpl class, left for compatibility with
   * older versions
   *
   * @param activeBeans the active beans in the Page Object
   */
  public PageObjectContextImpl(Map<Class<? extends PageObject>, Class> activeBeans) {
    this(activeBeans, new HashMap<>());
  }

  /**
   * build default impl name (with prefix Impl) and try to find proper class
   *
   * @param interfaceName name of the interface
   * @return the class
   */
  private static Class getDefaultImplementation(String interfaceName) {
    String className = getDefaultImplType(interfaceName);
    try {
      return Class.forName(className);
    } catch (ClassNotFoundException e) {
      throw new UtamCoreError(String.format(ERR_GET_IMPL_BY_NAME, interfaceName));
    }
  }

  /**
   * Gets the default implementation type for an interface
   *
   * @param fullInterfaceName full interface name for which to get the default implementation
   * @return the default implementation
   */
  public static String getDefaultImplType(String fullInterfaceName) {
    String packageName = fullInterfaceName.substring(0, fullInterfaceName.lastIndexOf("."));
    String typeName = fullInterfaceName.substring(fullInterfaceName.lastIndexOf(".") + 1) + "Impl";
    return String.format("%s.impl.%s", packageName, typeName);
  }

  @Override
  public <T extends PageObject> T getBean(Class<T> type) {
    Class<? extends T> implementingClass;
    if (BasePageObject.class.isAssignableFrom(type)) { // if class and not interface is passed
      implementingClass = type;
    } else if (activeBeans.containsKey(type)) {
      implementingClass = activeBeans.get(type);
    } else if (defaultBeans.containsKey(type)) {
      implementingClass = defaultBeans.get(type);
    } else {
      implementingClass = getDefaultImplementation(type.getName());
    }
    try {
      Constructor<? extends T> constructor = implementingClass.getConstructor();
      constructor.setAccessible(true);
      return constructor.newInstance();
    } catch (NoSuchMethodException
        | IllegalAccessException
        | InstantiationException
        | InvocationTargetException e) {
      throw new UtamError(String.format(ERR_GET_INSTANCE_BY_NAME, type.getName()), e);
    }
  }
}
