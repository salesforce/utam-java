package utam.core.framework.consumer;

import utam.core.framework.base.PageMarker;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.element.LocatorNode;
import utam.core.selenium.element.Web;

import java.time.Duration;
import java.util.List;
import java.util.function.Supplier;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static utam.core.selenium.context.SeleniumContextProvider.DEFAULT_POLLING_INTERVAL;
import static utam.core.selenium.context.SeleniumContextProvider.DEFAULT_POLLING_TIMEOUT;
import static utam.core.selenium.element.LocatorUtilities.getAllNodes;
import static utam.core.selenium.element.LocatorUtilities.getElementLocator;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderTests {

  private static UtamLoader getDefaultLoader() {
    return new UtamLoaderImpl(new UtamLoaderConfigImpl(mock(WebDriver.class)));
  }

  @Test
  public void testRootPageObjectCreation() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.create(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testPageObjectLoad() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.load(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testCreateUtamFromContainer() {
    UtamLoader loader = getDefaultLoader();
    ContainerMock containerMock = new ContainerMock();
    TestLoaderConfigPageObject pageObjectMock =
        loader.create(containerMock, TestLoaderConfigPageObject.class, Web.byCss("root"));
    List<LocatorNode> chain = getAllNodes(getElementLocator(pageObjectMock.getRoot()));
    assertThat(chain.get(1).getSelector().getValue(), is(equalTo("root")));
  }

  @Test
  public void testDefaultConstructor() {
    UtamLoaderImpl loader = new UtamLoaderImpl(mock(WebDriver.class));
    SeleniumContext seleniumContext = loader.utamConfig.getSeleniumContext();
    assertThat(seleniumContext.getPollingTimeout(), is(equalTo(DEFAULT_POLLING_TIMEOUT)));
    assertThat(seleniumContext.getPollingInterval(), is(equalTo(DEFAULT_POLLING_INTERVAL)));
  }

  @Test
  public void testDefaultConstructorForTests() {
    UtamLoaderImpl loader = (UtamLoaderImpl) UtamLoaderImpl.getSimulatorLoader(mock(WebDriver.class));
    SeleniumContext seleniumContext = loader.utamConfig.getSeleniumContext();
    assertThat(seleniumContext.getPollingTimeout(), is(equalTo(Duration.ofSeconds(1))));
    assertThat(seleniumContext.getPollingInterval(), is(equalTo(DEFAULT_POLLING_INTERVAL)));
  }

  private static class ContainerMock implements Container {

    final Supplier<SearchContext> root = () -> mock(WebElement.class);

    @Override
    public Supplier<SearchContext> getScope() {
      return root;
    }
  }

  @PageMarker.Find(css = "root")
  // has to be public for reflections to work
  public static class TestLoaderConfigPageObjectOverride extends TestLoaderConfigPageObjectImpl {}
}
