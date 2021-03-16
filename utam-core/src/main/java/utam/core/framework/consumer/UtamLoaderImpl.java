package utam.core.framework.consumer;

import org.openqa.selenium.WebDriver;
import utam.core.selenium.element.Selector;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectBuilderImpl;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;

import java.time.Duration;

/**
 * implementation of UtamLoader
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderImpl implements UtamLoader {

  final AbstractUtamLoaderConfig utamConfig;

  public UtamLoaderImpl(UtamLoaderConfig utamLoaderConfig) {
    if (!(utamLoaderConfig instanceof AbstractUtamLoaderConfig)) {
      throw new UtamError(
          String.format(
              "Unsupported configuration class - instance of %s is expected to be passed to loader",
              AbstractUtamLoaderConfig.class.getSimpleName()));
    }
    this.utamConfig = (AbstractUtamLoaderConfig) utamLoaderConfig;
  }

  /**
   * creates instance of loader that does not accept dependency injection
   * @param driver driver instance
   */
  public UtamLoaderImpl(WebDriver driver) {
    this(new UtamLoaderEmptyConfig(driver));
  }

  /**
   * create instance of loader for unit tests with minimum possible timeout
   * @param driver simulator driver
   * @return loader instance
   */
  public static UtamLoader getSimulatorLoader(WebDriver driver) {
    UtamLoaderConfig config = new UtamLoaderEmptyConfig(driver);
    config.setTimeout(Duration.ofSeconds(1));
    return new UtamLoaderImpl(config);
  }

  PageObjectsFactory getFactory() {
    return utamConfig.getFactory();
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> type) {
    return new PageObjectBuilderImpl(getFactory()).build(type);
  }

  @Override
  public <T extends RootPageObject> T load(Class<T> type) {
    T instance = create(type);
    instance.load();
    return instance;
  }

  @Override
  public <T extends PageObject> T create(
          Container parent, Class<T> type, Selector injectedSelector) {
    if (parent == null) {
      throw new UtamError(
              String.format("can't build %s, container page object is null", type.getName()));
    }
    PageObjectBuilderImpl builder =
            new PageObjectBuilderImpl.UtamChild(getFactory(), parent, injectedSelector);
    return builder.build(type);
  }
}
