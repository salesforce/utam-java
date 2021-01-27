package framework.base;

import framework.consumer.UtamError;

import java.lang.reflect.Constructor;

/**
 * provides imperative utility instance
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class ImperativeProvider<T extends PageObject> implements ImperativeExtension<T> {

  private static final String ERR_CANT_CREATE_UTILITY = "can't create instance of imperative utility %s";
  private T instance;

  protected ImperativeProvider(T instance) {
    this.instance = instance;
  }

  protected ImperativeProvider() {}

  static <T extends ImperativeProvider> T build(Class<T> type) {
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

  void setInstance(T instance) {
    this.instance = instance;
  }
}
