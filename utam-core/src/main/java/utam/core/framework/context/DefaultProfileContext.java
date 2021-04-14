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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.UtamError;

/**
 * creates empty profile context to use for custom overrides </br> does not read from configs
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class DefaultProfileContext implements ProfileContext {

  private static final String ERR_PROFILE_FILE = "can't read profile config from file '%s'";
  // map with configured beans
  final Map<Class<? extends PageObject>, String> beans = new HashMap<>();
  private final Profile profile;

  public DefaultProfileContext(Profile profile) {
    this.profile = profile;
    Properties properties = getBeansFromResource();
    properties.stringPropertyNames().forEach(type -> {
      String implClassName = properties.getProperty(type);
      // default config can have empty lines
      if (!implClassName.isEmpty()) {
        try {
          beans.put(getClassFromName(type), implClassName);
        } catch (ClassNotFoundException e) {
          throw new UtamError(String.format("error configuring bean %s = %s", type, implClassName),
              e);
        }
      }
    });
  }

  private Properties getBeansFromResource() {
    ClassLoader classLoader = getClass().getClassLoader();
    String configName = profile.getConfigName() + ".properties";
    Properties properties = new Properties();
    try {
      Enumeration<URL> configs = classLoader.getResources(configName);
      if (configs != null) {
        for (URL url : Collections.list(configs)) {
          InputStream in = url.openStream();
          properties.load(in);
        }
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
    if (key == null || !beans.containsKey(key)) {
      return null;
    }
    return beans.get(key);
  }
}
