package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Document;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class DocumentObjectTests {

  private static final String TEST_URL = "https://utam.dev/rocks/";


  @Test
  public void testGetUrl() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebDriverMock().getCurrentUrl()).thenReturn(TEST_URL);
    Document document = new DocumentObject(mock.getFactory());
    assertThat(document.getUrl(), is(equalTo(TEST_URL)));
  }

  @Test
  public void testWaitForDocumentReady() {
    MockUtilities mock = new MockUtilities();
    when(mock.getExecutorMock().executeScript(DocumentObject.DOM_READY_JAVASCRIPT)).thenReturn(true);
    Document document = new DocumentObject(mock.getFactory());
    document.waitForDocumentReady();
  }

  @Test
  public void testContainsElement() {
    MockUtilities mock = new MockUtilities();
    Document document = new DocumentObject(mock.getFactory());
    when(mock.getWebDriverMock().findElements(By.cssSelector("existing")))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(document.containsElement(LocatorBy.byCss("existing")), is(true));
    assertThat(document.containsElement(LocatorBy.byCss("non-existing")), is(false));
  }
}