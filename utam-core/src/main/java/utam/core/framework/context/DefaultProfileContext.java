/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static utam.core.framework.consumer.UtamLoaderConfigImpl.DEFAULT_PROFILE;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObject;

/**
 * creates empty profile context to use for custom overrides <br>
 * does not read from configs
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class DefaultProfileContext implements ProfileContext {

  static final String ERR_GET_CLASS_BY_NAME = "can't find class with name %s";
  private static final String DEFAULT_PROFILE_KEY = DEFAULT_PROFILE.getKey();

  /** map with configured beans. key - interface class, value - name of the implementing class */
  private final Map<Class<? extends PageObject>, String> beans = new HashMap<>();

  /** initializes empty profile context */
  DefaultProfileContext() {}

  /**
   * Initializes a new instance of the ProfileContext class from Json dependencies config
   *
   * @param mapping map, key - interface class name, value - name of the implementing class
   */
  public DefaultProfileContext(Map<Object, Object> mapping) {
    for (Object interfaceName : mapping.keySet()) {
      Class interfaceClass = getClassFromName(interfaceName.toString());
      String implClassName = mapping.get(interfaceName).toString();
      beans.put(interfaceClass, implClassName);
    }
  }

  /**
   * Gets the class from the name
   *
   * @param className name of the class to retrieve
   * @return the class
   */
  static Class getClassFromName(String className) {
    try {
      return Class.forName(className);
    } catch (ClassNotFoundException e) {
      throw new UtamCoreError(String.format(ERR_GET_CLASS_BY_NAME, className));
    }
  }

  /**
   * create instance of empty profile context
   *
   * @return new profile context
   */
  public static ProfileContext getEmptyProfileContext() {
    return new DefaultProfileContext();
  }

  /**
   * collect all contexts from different modules into one map
   *
   * @param combinedConfig map to collect all configs
   * @param defaultContext default context is collected separately
   * @param moduleConfig dependencies config from specific module
   */
  public static void mergeDependencies(
      Map<String, ProfileContext> combinedConfig,
      ProfileContext defaultContext,
      Map<String, ProfileContext> moduleConfig) {
    for (String profileKey : moduleConfig.keySet()) {
      if (DEFAULT_PROFILE_KEY.equals(profileKey)) {
        mergeContexts(defaultContext, moduleConfig.get(profileKey));
        continue;
      }
      if (combinedConfig.containsKey(profileKey)) {
        mergeContexts(combinedConfig.get(profileKey), moduleConfig.get(profileKey));
      } else {
        combinedConfig.put(profileKey, moduleConfig.get(profileKey));
      }
    }
  }

  /**
   * merge two contexts with same profile from different modules
   *
   * @param alreadyLoaded previously loaded
   * @param profileContext added
   */
  private static void mergeContexts(ProfileContext alreadyLoaded, ProfileContext profileContext) {
    // add beans to already existing for same profile
    profileContext
        .getConfiguredBeans()
        .forEach(bean -> alreadyLoaded.setBean(bean, profileContext.getBeanName(bean)));
  }

  /**
   * build beans map from profile context
   *
   * @param beans target map
   * @param profileContext source context
   * @return target map
   */
  public static Map<Class<? extends PageObject>, Class> mergeBeans(
      Map<Class<? extends PageObject>, Class> beans, ProfileContext profileContext) {
    profileContext
        .getConfiguredBeans()
        .forEach(
            beanType -> {
              String name = profileContext.getBeanName(beanType);
              Class implementation = getClassFromName(name);
              beans.put(beanType, implementation);
            });
    return beans;
  }

  @Override
  public <T extends PageObject> void setBean(
      Class<? extends T> pageObjectType, String implClassName) {
    beans.put(pageObjectType, implClassName);
  }

  @Override
  public Collection<Class<? extends PageObject>> getConfiguredBeans() {
    return beans.keySet();
  }

  @Override
  public <T extends PageObject> String getBeanName(Class<T> key) {
    return beans.get(key);
  }
}
