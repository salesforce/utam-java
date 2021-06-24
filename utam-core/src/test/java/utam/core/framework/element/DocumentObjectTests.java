/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
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
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.base.RootPageObject;
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

  @Test
  public void testContainsObject() {
    MockUtilities mock = new MockUtilities();
    Document document = new DocumentObject(mock.getFactory());
    when(mock.getWebDriverMock().findElements(By.cssSelector("found")))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(document.containsObject(TestContains.class), is(true));
    assertThat(document.containsObject(TestNotContains.class), is(false));
  }

  @PageMarker.Find(css = "notfound")
  static class TestNotContains extends BasePageObject implements RootPageObject {
    // has to be public for reflection to create instance
    public TestNotContains() {}
  }

  @PageMarker.Find(css = "found")
  static class TestContains extends BasePageObject implements RootPageObject {
    // has to be public for reflection to create instance
    public TestContains() {}
  }
}