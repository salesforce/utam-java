package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.selenium.element.Rect;

/**
 * Test for the Window implementation
 *
 * @author william.sandy
 */
public class WindowTests {
    MockUtilities mock;
    WebDriver driver;

    @BeforeMethod
    void setup() {
        mock = new MockUtilities();
        driver = mock.getWebDriverMock();
    }

    @Test
    public void testGetRect() {
        //TODO mock manage or window to return getPosition and getSize

        Point p = new Point(64, 32);
        Dimension d = new Dimension(200, 800);

        when(driver.manage().window().getPosition()).thenReturn(p);
        when(driver.manage().window().getSize()).thenReturn(d);

        Rect rect = new WindowImpl(mock.getDriverAdapter()).getRect();
        assertThat(rect.getPoint(), is(equalTo(p)));
        assertThat(rect.getDimension(), is(equalTo(d)));
    }

    @Test
    public void testSetRect() {
        //TODO write test, set mock rectangle then confirm get gives it back
    }

    @Test
    public void testClose() {
        //TODO write test

    }

    @Test
    public void testGetDocument() {
        //TODO write test

    }
}
