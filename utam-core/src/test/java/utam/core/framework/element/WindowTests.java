package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Window;
import utam.core.selenium.element.Rect;

/**
 * Test for the Window implementation
 *
 * @author william.sandy
 */
public class WindowTests {
  private MockUtilities mock;
  private WebDriver driver;

  @BeforeMethod
  void setup() {
    mock = new MockUtilities();
    driver = mock.getWebDriverMock();
  }

  @Test
  public void testGetRect() {
    Point p = new Point(64, 32);
    Dimension d = new Dimension(200, 800);

    when(driver.manage().window().getPosition()).thenReturn(p);
    when(driver.manage().window().getSize()).thenReturn(d);

    Rect rect = new WindowImpl(mock.getFactory()).getRect();
    assertThat(rect.getX(), is(equalTo(p.getX())));
    assertThat(rect.getY(), is(equalTo(p.getY())));
    assertThat(rect.getWidth(), is(equalTo(d.getWidth())));
    assertThat(rect.getHeight(), is(equalTo(d.getHeight())));
  }

  @Test
  public void testSetRect() {
    Point p = new Point(64, 32);
    Dimension d = new Dimension(200, 800);
    Rect r = new Rect(p, d);

    when(driver.manage().window().getPosition()).thenReturn(p);
    when(driver.manage().window().getSize()).thenReturn(d);

    WindowImpl window = new WindowImpl(mock.getFactory());
    window.setRect(r);

    assertThat(window.getRect().getX(), is(equalTo(r.getX())));
    assertThat(window.getRect().getY(), is(equalTo(r.getY())));
    assertThat(window.getRect().getWidth(), is(equalTo(r.getWidth())));
    assertThat(window.getRect().getHeight(), is(equalTo(r.getHeight())));
  }

  @Test
  public void testClose() {
    Window window = new WindowImpl(mock.getFactory());
    when(driver.getWindowHandle()).thenReturn("www.example.com");
    window.close();
    verify(driver, times(1)).close();
  }

  @Test
  public void testGetDocument() {
    Window window = new WindowImpl(mock.getFactory());
    assertThat(window.getDocument(), is(equalTo(mock.getFactory().getDocument())));
  }
}
