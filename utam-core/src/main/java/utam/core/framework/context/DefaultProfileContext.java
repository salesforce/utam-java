/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;

/**
 * creates empty profile context to use for custom overrides <br> does not read from configs
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class DefaultProfileContext implements ProfileContext {

  private static final String ERR_PROFILE_FILE = "can't read profile config from file '%s'";

  // map with configured beans
  final Map<Class<? extends PageObject>, String> beans = new HashMap<>();
  private final Profile profile;
  private final String moduleName;

  /**
   * Initializes a new instance of the DefaultProfileContext class
   * @param moduleName name of the module
   * @param profile    profile used in the module
   */
  public DefaultProfileContext(String moduleName, Profile profile) {
    this.profile = profile;
    this.moduleName = moduleName;
    Properties properties = getBeansFromResource();
    properties.stringPropertyNames().forEach(type -> {
      String implClassName = properties.getProperty(type);
      // default config can have empty lines
      if (!implClassName.isEmpty()) {
        beans.put(getClassFromName(type), implClassName);
      }
    });
  }

  /**
   * Initializes a new instance of the DefaultProfileContext class, only used in unit tests
   * @param moduleName name of the module
   * @param profile    profile used in the module
   * @param properties properties to map class types
   */
  // used in tests
  protected DefaultProfileContext(String moduleName, Profile profile, Properties properties) {
    this.profile = profile;
    this.moduleName = moduleName;
    properties.stringPropertyNames().forEach(type -> {
      String implClassName = properties.getProperty(type);
      // default config can have empty lines
      if (!implClassName.isEmpty()) {
        beans.put(getClassFromName(type), implClassName);
      }
    });
  }

  private Properties getBeansFromResource() {
    ClassLoader classLoader = getClass().getClassLoader();
    String configName = profile.getConfigName(moduleName) + ".properties";
    Properties properties = new Properties();
    try {
      List<URL> configs = Collections.list(classLoader.getResources(configName));
      if (!configs.isEmpty()) {
        UtamLogger.info(String.format("Reading Page Objects config file(s) %s", configName));
        for (URL url : configs) {
          InputStream in = url.openStream();
          properties.load(in);
        }
      } else {
        UtamLogger.warning(String.format("Page Objects config file(s) %s not found", configName));
      }
    } catch (IOException e) {
      UtamLogger.warning(String.format(ERR_PROFILE_FILE, configName));
      UtamLogger.warning(e.getMessage());
    }
    return properties;
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
