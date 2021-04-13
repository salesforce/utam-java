/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.time.Duration;
import org.openqa.selenium.WebDriver;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectBuilderImpl;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;
import utam.core.selenium.element.Selector;

/**
 * implementation of UtamLoader
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderImpl implements UtamLoader {

  final UtamLoaderConfigImpl utamConfig;

  public UtamLoaderImpl(UtamLoaderConfig utamLoaderConfig) {
    if (!(utamLoaderConfig instanceof UtamLoaderConfigImpl)) {
      throw new UtamError(
          String.format(
              "Unsupported configuration class - instance of %s is expected to be passed to loader",
              UtamLoaderConfigImpl.class.getSimpleName()));
    }
    this.utamConfig = (UtamLoaderConfigImpl) utamLoaderConfig;
    this.utamConfig.setDefaultProfile();
  }

  /**
   * creates instance of loader that does not accept dependency injection
   *
   * @param driver driver instance
   */
  public UtamLoaderImpl(WebDriver driver) {
    this(new UtamLoaderConfigImpl(driver));
  }

  /**
   * create instance of loader for unit tests with minimum possible timeout
   *
   * @param driver simulator driver
   * @return loader instance
   */
  public static UtamLoader getSimulatorLoader(WebDriver driver) {
    UtamLoaderConfig config = new UtamLoaderConfigImpl(driver);
    config.setTimeout(Duration.ofSeconds(1));
    return new UtamLoaderImpl(config);
  }

  /**
   * protected access because it might need override in a child class for test context
   *
   * @return factory instance to create a page object
   */
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
