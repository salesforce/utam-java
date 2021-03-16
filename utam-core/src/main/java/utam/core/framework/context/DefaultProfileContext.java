package utam.core.framework.context;

import java.io.InputStream;
import java.net.URL;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.UtamError;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.function.Function;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

/**
 * profile context
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultProfileContext implements ProfileContext {

  private static final String ERR_PROFILE_FILE = "can't access profile config from file '%s'";
  private static final String ERR_CANT_READ_CONFIG = "could not read profile config: ";

  final Map<Class<? extends PageObject>, String> beans = new HashMap<>();

  public DefaultProfileContext() {
  }

  public DefaultProfileContext(Profile profile, Function<String,ResourceBundle> resourceBundleProvider) {
    this(getBeansFromResourceBundle(profile, resourceBundleProvider));
  }

  public DefaultProfileContext(Properties beansConfig) {
    setBeans(beansConfig);
  }

  public DefaultProfileContext(Profile profile, Class classLoader) {
    setBeans(getBeansFromResource(profile, classLoader));
  }

  private static Properties getBeansFromResource(Profile profile, Class classLoader) {
    String configName = profile.getConfigFileName();
    Properties properties = new Properties();
    URL url = classLoader.getClassLoader().getResource(configName);
    if(url == null) {
      UtamLogger.warning(String.format(ERR_PROFILE_FILE, configName));
      return properties;
    }
    try {
      InputStream in = url.openStream();
      properties.load(in);
    } catch (IOException e) {
      UtamLogger.warning(ERR_CANT_READ_CONFIG + e.getMessage());
    }
    return properties;
  }

  private static Properties getBeansFromResourceBundle(Profile profile, Function<String,ResourceBundle> resourceBundleProvider) {
    String configName = profile.getConfigFileName();
    Properties properties = new Properties();
    try {
      ResourceBundle resourceBundle = resourceBundleProvider.apply(configName);
      resourceBundle
              .keySet()
              .forEach(key -> properties.setProperty(key, resourceBundle.getString(key)));
    } catch (MissingResourceException e) {
      UtamLogger.warning(ERR_CANT_READ_CONFIG + e.getMessage());
    }
    return properties;
  }

  @Override
  public boolean equals(Object obj) {
    return getClass().getName().equals(obj.getClass().getName());
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
  public void setBeans(Properties properties) {
    properties.stringPropertyNames().forEach(type -> {
      String implClassName = properties.getProperty(type);
      // default config can have empty lines
      if(!implClassName.isEmpty()) {
        try {
          beans.put(getClassFromName(type), implClassName);
        } catch (ClassNotFoundException e) {
          throw new UtamError(String.format("error configuring bean %s = %s", type, implClassName),
              e);
        }
      }
    });
  }

  @Override
  public <T extends PageObject> String getBeanName(Class<T> key) {
    if (!beans.containsKey(key)) {
      return null;
    }
    return beans.get(key);
  }
}
