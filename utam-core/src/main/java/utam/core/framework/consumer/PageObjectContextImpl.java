package utam.core.framework.consumer;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.function.Function;

/**
 * context to build page object from interface class
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class PageObjectContextImpl implements PageObjectContext {

  private final Map<Class<? extends PageObject>, Class<? extends PageObject>> beansOverride;

  public PageObjectContextImpl(Map<Class<? extends PageObject>, Class<? extends PageObject>> overrides) {
    this.beansOverride = overrides;
  }

  public static String[] getDefaultImplType(String fullInterfaceName) {
    String packageName = fullInterfaceName.substring(0, fullInterfaceName.lastIndexOf("."));
    String typeName = fullInterfaceName.substring(fullInterfaceName.lastIndexOf(".") + 1) + "Impl";
    return new String[] {typeName, String.format("%s.impl.%s", packageName, typeName)};
  }

  @SuppressWarnings("rawtypes")
  public static Class getClassFromName(String className) throws ClassNotFoundException {
    return Class.forName(className);
  }

  @SuppressWarnings("rawtypes")
  private static String getBeanErr(Class type) {
    return String.format("bean for type '%s' is not configured", type.getName());
  }

  private static <T extends PageObject> T init(
      Class<T> type, Function<Class<T>, Class<? extends T>> typeMapping) {
    try {
      Constructor<? extends T> constructor = typeMapping.apply(type).getConstructor();
      constructor.setAccessible(true);
      return constructor.newInstance();
    } catch (NoSuchMethodException
        | IllegalAccessException
        | InstantiationException
        | InvocationTargetException e) {
      throw new UtamError(String.format("can't create instance of type '%s'", type.getName()), e);
    }
  }

  @Override
  public <T extends PageObject> T getBean(Class<T> type) {
    Function<Class<T>, Class<? extends T>> getter =
        ctype -> {
          try {
            return getImplementingClass(ctype);
          } catch (ClassNotFoundException e) {
            throw new UtamError(getBeanErr(type), e);
          }
        };
    return init(type, getter);
  }

  @SuppressWarnings("unchecked")
  private <T extends PageObject> Class<? extends T> getImplementingClass(Class<T> type)
      throws ClassNotFoundException {
    if (BasePageObject.class.isAssignableFrom(type)) { // if class and not interface is passed
      return type;
    }
    if (beansOverride.containsKey(type)) {
      return (Class<? extends T>) beansOverride.get(type);
    }
    String className = getDefaultImplType(type.getName())[1];
    return getClassFromName(className);
  }
}
