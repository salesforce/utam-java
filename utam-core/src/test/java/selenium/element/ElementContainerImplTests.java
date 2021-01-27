package selenium.element;

import framework.base.BasePageObject;
import framework.base.PageObjectsFactory;
import framework.base.PageObjectsFactoryImpl;
import framework.consumer.Contained;
import framework.consumer.ContainerElement;
import framework.consumer.Utilities;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import selenium.context.SeleniumContext;
import selenium.context.SeleniumContextProvider;

import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static selenium.element.LocatorUtilities.getAllNodes;

/**
 * container element tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class ElementContainerImplTests {

  @Test
  public void testCreate() {
    LocatorNodeImpl locator = new LocatorNodeImpl.Css("css");
    SeleniumContext seleniumContext = new SeleniumContextProvider(mock(WebDriver.class));
    PageObjectsFactory factory =
            new PageObjectsFactoryImpl(Utilities.test(), seleniumContext);
    ContainerElement element = new ElementContainerImpl(new LocatorImpl(locator), factory, true);
    element.setScope(mock(Contained.class));
    TestLoad testLoad = element.load(TestLoad.class, "root-selector");
    List<LocatorNode> testLoadRoot = testLoad.getTestRootLocatorNodes();
    assertThat(testLoadRoot.size(), is(equalTo(2)));
    assertThat(testLoadRoot.get(0).getSelector().getValue(), is(equalTo("css")));
    assertThat(testLoadRoot.get(1).getSelector().getValue(), is(equalTo("root-selector")));
    assertThat(testLoadRoot.get(1).getScopeTransformer(), is(equalTo(ShadowBoundary.EXPAND_SHADOW_ROOT)));
  }

  static final class TestLoad extends BasePageObject {

    // should be public for java reflection to work
    public TestLoad(){}

    List<LocatorNode> getTestRootLocatorNodes() {
      return getAllNodes(getRootLocator());
    }
  }
}
