package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext.Type;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.LocatorBy;

/**
 * container element tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class ContainerElementTests {

  @Test
  public void testCreateWithElementNotFound() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ElementLocation elementLocation = new ElementLocationChain(elementMock);
    // nothing found, throw if not nullable
    expectThrows(NullPointerException.class,
        () -> new ContainerElementImpl(mock.getFactory(), elementLocation, Type.EXISTING)
            .load(TestLoad.class, LocatorBy.byCss("css")));
    expectThrows(NoSuchElementException.class,
        () -> new ContainerElementImpl(mock.getFactory(), elementLocation, Type.EXISTING_IN_SHADOW)
            .load(TestLoad.class, LocatorBy.byCss("css")));
    // nullable but can't find root
    expectThrows(NullPointerException.class,
        () -> new ContainerElementImpl(mock.getFactory(), elementLocation, Type.NULLABLE)
            .load(TestLoad.class, LocatorBy.byCss("css")));
    expectThrows(NullPointerException.class,
        () -> new ContainerElementImpl(mock.getFactory(), elementLocation, Type.NULLABLE_IN_SHADOW)
            .load(TestLoad.class, LocatorBy.byCss("css")));
  }

  @Test
  public void testCreateWithElementFound() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ElementLocation elementLocation = new ElementLocationChain(elementMock);
    ContainerElement element = new ContainerElementImpl(mock.getFactory(), elementLocation,
        Type.EXISTING);
    element.setScope(mock(Contained.class));
    when(mock.getWebElementMock().findElement(By.cssSelector("css"))).thenReturn(mock.getWebElementMock());
    TestLoad testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(testLoad.getRootLocator().getLocatorChainString(),
        is(equalTo("driver > element > By.cssSelector: css")));
    element = new ContainerElementImpl(mock.getFactory(), elementLocation, Type.NULLABLE);
    testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(testLoad.getRootLocator().getLocatorChainString(),
        is(equalTo("driver > element > By.cssSelector: css")));
  }

  @Test
  public void testCreateWithElementFoundInShadow() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ElementLocation elementLocation = new ElementLocationChain(elementMock);
    ContainerElement element = new ContainerElementImpl(mock.getFactory(), elementLocation,
        Type.EXISTING_IN_SHADOW);
    element.setScope(mock(Contained.class));
    mock.setShadowMock(mock.getWebElementMock(), "css");
    TestLoad testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(testLoad.getRootLocator().getLocatorChainString(),
        is(equalTo("driver > element >> By.cssSelector: css")));
  }

  @Test
  public void testCreateWithLocator() {
    MockUtilities mock = new MockUtilities();
    ContainerElement element = new ContainerElementImpl(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("scope"), Type.NULLABLE), Type.NULLABLE);
    element.setScope(mock(Contained.class));
    when(mock.getWebDriverMock().findElement(By.cssSelector("scope"))).thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().findElement(By.cssSelector("css"))).thenReturn(mock.getWebElementMock());
    TestLoad testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(testLoad.getRootLocator().getLocatorChainString(),
        is(equalTo("driver > By.cssSelector: scope > By.cssSelector: css")));
  }

  static final class TestLoad extends BasePageObject {

    // should be public for java reflection to work
    public TestLoad() {
    }
  }
}
