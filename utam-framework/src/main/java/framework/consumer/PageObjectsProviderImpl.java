package framework.consumer;

import framework.base.PageObject;
import framework.base.PageObjectsFactory;
import framework.base.RootPageObject;
import framework.context.Profile;
import framework.context.ProfileContext;
import org.openqa.selenium.WebDriver;
import selenium.context.SeleniumContext;
import selenium.element.Selector;

import java.time.Duration;

import static framework.UtamLogger.info;

/**
 * default implementation for provider
 *
 * @deprecated since 230, use UtamLoaderConfig and UtamLoader instead
 * @author elizaveta.ivanova
 * @since 226
 */
@Deprecated
public class PageObjectsProviderImpl implements PageObjectsProvider {

  private final UtamLoaderConfig utamConfig;
  private final UtamLoader utamLoader;

  PageObjectsProviderImpl(WebDriver driver) {
    this.utamConfig = new UtamLoaderConfigImpl();
    this.utamLoader = new UtamLoaderImpl(utamConfig, driver);
  }

  protected PageObjectsProviderImpl(UtamLoaderConfig utamLoaderConfig, UtamLoader loader) {
    this.utamConfig = utamLoaderConfig;
    this.utamLoader = loader;
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> type) {
    return utamLoader.create(type);
  }

  @Override
  public <T extends PageObject> T create(Container parent, Class<T> type, Selector inject) {
    return utamLoader.create(parent, type, inject);
  }

  @Override
  public <T extends RootPageObject> T load(Class<T> type) {
    return utamLoader.load(type);
  }

  @Override
  public PageObjectsFactory getFactory() {
    return ((UtamLoaderImpl)utamLoader).getFactory();
  }

  @Override
  public final SeleniumContext getSeleniumContext() {
    return ((UtamLoaderConfigImpl)utamConfig).getSeleniumContext();
  }

  @Override
  public void setBridgeAppTitle(String title) {
    utamConfig.setBridgeAppTitle(title);
  }

  @Override
  public void setActiveProfile(Profile key) {
    info(String.format("set active profile '%s'", key));
    utamConfig.setActiveProfile(key);
  }

  @Override
  public void setProfileContext(ProfileContext profileContext) {
    utamConfig.setProfileContext(profileContext);
  }

  @Override
  public void setTimeout(Duration timeout) {
    utamConfig.setTimeout(timeout);
  }

  @Override
  public void setLocationPolicy(LocationPolicy policy) {
    utamConfig.setLocationPolicy(policy);
  }
}
