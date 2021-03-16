package utam.core.framework.context;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.UtamError;

/**
 * abstract implementation for Profile Context to store beans definitions
 *
 * @author elizaveta.ivanova
 * @since 228
 */
abstract class AbstractProfileContext implements ProfileContext {

  static final String ERR_PROFILE_FILE = "can't read profile config from file '%s'";
  // map with configured beans
  final Map<Class<? extends PageObject>, String> beans = new HashMap<>();

  @Override
  public <T extends PageObject> void setBean(
      Class<? extends T> pageObjectType, String implClassName) {
    beans.put(pageObjectType, implClassName);
  }

  @Override
  public Collection<Class<? extends PageObject>> getConfiguredBeans() {
    return beans.keySet();
  }

  final void setBeans(Properties properties) {
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

  @Override
  public <T extends PageObject> String getBeanName(Class<T> key) {
    if (key == null || !beans.containsKey(key)) {
      return null;
    }
    return beans.get(key);
  }
}
