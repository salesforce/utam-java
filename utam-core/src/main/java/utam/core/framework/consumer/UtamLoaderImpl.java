package utam.core.framework.consumer;

import org.openqa.selenium.WebDriver;
import utam.core.selenium.element.Selector;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectBuilderImpl;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;

import java.time.Duration;

/**
 * default implementation of utam loader
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderImpl implements UtamLoader {

  final UtamLoaderConfigImpl utamConfig;

  public UtamLoaderImpl(UtamLoaderConfig utamLoaderConfig, WebDriver driver) {
    if (!(utamLoaderConfig instanceof UtamLoaderConfigImpl)) {
      throw new UtamError(
          String.format(
              "Unsupported configuration class - instance of %s is expected to be passed to loader",
              UtamLoaderConfigImpl.class.getSimpleName()));
    }
    this.utamConfig = (UtamLoaderConfigImpl) utamLoaderConfig;
    this.utamConfig.setDriver(driver);
  }

  /**
   * creates instance of loader with default config settings
   * @param driver driver instance
   */
  public UtamLoaderImpl(WebDriver driver) {
    this(new UtamLoaderConfigImpl(), driver);
  }

  /**
   * create instance of loader for unit tests with minimum possible timeout
   * @param driver simulator driver
   * @return loader instance
   */
  public static UtamLoader getSimulatorLoader(WebDriver driver) {
    UtamLoaderConfig config = new UtamLoaderConfigImpl();
    config.setTimeout(Duration.ofSeconds(1));
    return new UtamLoaderImpl(config, driver);
  }

  protected PageObjectsFactory getFactory() {
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
