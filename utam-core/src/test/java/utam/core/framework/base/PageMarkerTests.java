package utam.core.framework.base;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.selenium.appium.LocatorUIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX;

import org.testng.annotations.Test;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class PageMarkerTests {

  @Test
  public void testGetRootLocator() {
    PageMarker.Find annotation = mock(PageMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(PageMarker.getRootLocator(annotation),
        is(equalTo(LocatorBy.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(PageMarker.getRootLocator(annotation), is(equalTo(LocatorBy.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn(UI_AUTOMATOR_SELECTOR_PREFIX + "enabled(true)");
    assertThat(PageMarker.getRootLocator(annotation),
        is(equalTo(LocatorBy.byUiAutomator("enabled(true)"))));
  }
}