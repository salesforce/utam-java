package framework.context;

import framework.UtamLogger;
import framework.base.PageObject;
import framework.consumer.UtamError;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.function.Function;

import static framework.consumer.PageObjectContextImpl.getClassFromName;

/**
 * profile context
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultProfileContext implements ProfileContext {

  private static final String ERR_PROFILE_FILE = "can't read/write profiles config from file '%s'";
  private static final String PROFILE_CONFIG_PATTERN = "%s_%s_config";
  private static final String ERR_PROFILE_PATH = "profile config path is null or empty";

  final Map<Class<? extends PageObject>, String> beans = new HashMap<>();
  private final Profile profile;

  public DefaultProfileContext(Profile profile) {
    this.profile = profile;
  }

  public DefaultProfileContext(Profile profile, Function<String,ResourceBundle> resourceBundleProvider) {
    this(profile);
    setBeans(getBeansFromResourceBundle(resourceBundleProvider));
  }

  private Properties getBeansFromResourceBundle(Function<String,ResourceBundle> resourceBundleProvider) {
    String configName = getProfileConfigName();
    Properties properties = new Properties();
    try {
      ResourceBundle resourceBundle = resourceBundleProvider.apply(configName);
      resourceBundle
              .keySet()
              .forEach(key -> properties.setProperty(key, resourceBundle.getString(key)));
    } catch (MissingResourceException e) {
      UtamLogger.warning("could not read profile config: " + e.getMessage());
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
  public Profile getProfile() {
    return profile;
  }

  @Override
  public void setBeans(Properties properties) {
    properties.stringPropertyNames().forEach(type -> setBean(type, properties.getProperty(type)));
  }

  @SuppressWarnings("unchecked")
  private void setBean(String type, String classType) {
    if (classType.isEmpty()) {
      if (getProfile().isDefault()) {
        UtamLogger.warning(String.format("default dependency for type '%s' is not set", type));
        return;
      }
      throw new UtamError(
          String.format(
              "dependency for type '%s' is not set in profile '%s'",
              type, getProfile().toString()));
    }
    try {
      beans.put(getClassFromName(type), classType);
    } catch (ClassNotFoundException e) {
      throw new UtamError(String.format("error configuring bean %s = %s", type, classType), e);
    }
  }

  @Override
  public <T extends PageObject> String getBeanName(Class<T> key) {
    if (!beans.containsKey(key)) {
      return null;
    }
    return beans.get(key);
  }

  private String getProfileConfigPath(String profilesRootPath) {
    if (profilesRootPath == null || profilesRootPath.isEmpty()) {
      throw new UtamError(ERR_PROFILE_PATH);
    }
    return String.format("%s/%s.properties", profilesRootPath, getProfileConfigName());
  }

  private String getProfileConfigName() {
    return String.format(PROFILE_CONFIG_PATTERN, profile.getName(), profile.getValue());
  }

  @Override
  public Writer getInjectionConfigWriter(String profilesRootPath) {
    String profileConfigPath = getProfileConfigPath(profilesRootPath);
    try {
      return new FileWriter(profileConfigPath);
    } catch (IOException e) {
      throw new UtamError(String.format(ERR_PROFILE_FILE, profileConfigPath), e);
    }
  }
}
