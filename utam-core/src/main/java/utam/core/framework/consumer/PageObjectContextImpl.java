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

  /**
   * Error template for not being able to find a class by name
   */
  public static final String ERR_GET_CLASS_BY_NAME = "can't find class with name %s";
  static final String ERR_GET_INSTANCE_BY_NAME = "can't create instance of type '%s'";

  private final Map<Class<? extends PageObject>, Class> activeBeans;

  /**
   * Initializes a new instance of the PageObjectContextImpl class
   *
   * @param activeBeans the active beans in the Page Object
   */
  public PageObjectContextImpl(Map<Class<? extends PageObject>, Class> activeBeans) {
    this.activeBeans = activeBeans;
  }

  /**
   * Gets the class from the name
   *
   * @param className name of the class to retrieve
   * @return the class
   */
  public static Class getClassFromName(String className) {
    try {
      return Class.forName(className);
    } catch (ClassNotFoundException e) {
      throw new UtamCoreError(String.format(ERR_GET_CLASS_BY_NAME, className));
    }
  }

  /**
   * Gets the default implementation type for an interface
   *
   * @param fullInterfaceName full interface name for which to get the default implementation
   * @return the default implementation
   */
  public static String[] getDefaultImplType(String fullInterfaceName) {
    String packageName = fullInterfaceName.substring(0, fullInterfaceName.lastIndexOf("."));
    String typeName = fullInterfaceName.substring(fullInterfaceName.lastIndexOf(".") + 1) + "Impl";
    return new String[] {typeName, String.format("%s.impl.%s", packageName, typeName)};
  }

  @Override
  public <T extends PageObject> T getBean(Class<T> type) {
    Class<? extends T> implementingClass;
    if (BasePageObject.class.isAssignableFrom(type)) { // if class and not interface is passed
      implementingClass = type;
    } else if (activeBeans.containsKey(type)) {
      implementingClass = activeBeans.get(type);
    } else {
      String className = getDefaultImplType(type.getName())[1];
      implementingClass = getClassFromName(className);
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
