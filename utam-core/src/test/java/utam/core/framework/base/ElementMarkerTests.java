package utam.core.framework.base;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.core.framework.base.ElementMarker.getLocator;
import static utam.core.selenium.appium.LocatorUIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX;

import java.lang.reflect.Field;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext.Type;
import utam.core.framework.base.PageObjectsFactoryImpl.FieldsBuilder;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementMarkerTests {

  @Test
  public void testGetFinderContext() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.expand()).thenReturn(true);
    when(annotation.nullable()).thenReturn(true);
    assertThat(ElementMarker.getFinderContext(annotation), is(equalTo(Type.NULLABLE_IN_SHADOW)));
  }

  @Test
  public void testGetLocator() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn(UI_AUTOMATOR_SELECTOR_PREFIX + "checkable()");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byUiAutomator("checkable()"))));
  }

  @Test
  public void testGetElementSelectorFromAnnotationNull() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn("");
    when(annotation.css()).thenReturn("css");
    assertThat(getLocator(annotation).getValue(), is(equalTo(By.cssSelector(annotation.css()))));
  }

  @Test
  public void testGetLocatorFromAnnotation() throws NoSuchFieldException {
    BasePageObject pageObject = new MockPageObject();
    FieldsBuilder builder = new FieldsBuilder(pageObject);
    Field field = pageObject.getClass().getDeclaredField("emptySelectorElement");
    assertThrows(() -> builder.getLocator(field)); // root is null
    pageObject.setBootstrap(mock(ElementLocation.class), mock(PageObjectsFactory.class));
    builder.getLocator(field);
  }

  static class MockPageObject extends BasePageObject {

    @ElementMarker.Find(css = ".fakeSelector")
    ElementLocation unscopedElement;

    @ElementMarker.Find(css = ".fakeScopedSelector", scope = "fakeScope")
    ElementLocation scopedElement;

    @ElementMarker.Find(css = ".fakeMissingScopeSelector", scope = "missingScope")
    ElementLocation illegalElement;

    @ElementMarker.Find(css = "")
    ElementLocation emptySelectorElement;
  }
}