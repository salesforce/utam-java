package utam.core.selenium.element;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.selenium.element.LocatorNodeImpl;

import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LocatorImplTestsUtilities {

  // !!! Changing this selector affects unit test results
  static final String GRANDCHILD_SELECTOR_STRING = ".fakeGrandChildSelector";
  // !!! Changing this selector affects unit test results
  static final String SELECTOR_STRING = ".fakeSelector";
  // !!! Changing this selector affects unit test results
  static final String CHILD_SELECTOR_STRING = ".fakeChildSelector";
  private static final String TEXT_PREFIX = "text found with ";

  static String getParentElementText() {
    return TEXT_PREFIX + SELECTOR_STRING;
  }

  static String getChildElementText() {
    return TEXT_PREFIX + CHILD_SELECTOR_STRING;
  }

  static String getGrandChildElementText() {
    return TEXT_PREFIX + GRANDCHILD_SELECTOR_STRING;
  }

  static WebDriver getMockDriverForCss() {
    WebElement mockGrandchildElement = mock(WebElement.class);
    when(mockGrandchildElement.getText()).thenReturn(TEXT_PREFIX + GRANDCHILD_SELECTOR_STRING);
    WebElement mockChildElement = mock(WebElement.class);
    when(mockChildElement.getText()).thenReturn(getChildElementText());
    when(mockChildElement.findElements(By.cssSelector(GRANDCHILD_SELECTOR_STRING)))
        .thenReturn(Collections.singletonList(mockGrandchildElement));
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getText()).thenReturn(getParentElementText());
    when(mockElement.findElements(By.cssSelector(CHILD_SELECTOR_STRING)))
        .thenReturn(Collections.singletonList(mockChildElement));
    WebDriver mockDriver = mock(WebDriver.class);
    when(mockDriver.findElements(By.cssSelector(SELECTOR_STRING)))
        .thenReturn(Collections.singletonList(mockElement));
    return mockDriver;
  }

  static class FakeLocator extends LocatorNodeImpl.Css {

    FakeLocator(String selector, Filter filter, Transformer transformer) {
      super(selector, selector, filter, transformer);
    }

    FakeLocator(Filter filter, Transformer transformer) {
      this(SELECTOR_STRING, filter, transformer);
    }

    @Override
    protected LocatorNodeImpl getCopy() {
      return new FakeLocator(getFilter(), getScopeTransformer());
    }
  }

  static class FakeChildLocator extends FakeLocator {

    FakeChildLocator(Filter filter, Transformer transformer) {
      super(CHILD_SELECTOR_STRING, filter, transformer);
    }

    @Override
    protected LocatorNodeImpl getCopy() {
      return new FakeChildLocator(getFilter(), getScopeTransformer());
    }
  }

  static class FakeGrandchildLocator extends FakeLocator {

    FakeGrandchildLocator(Filter filter, Transformer transformer) {
      super(GRANDCHILD_SELECTOR_STRING, filter, transformer);
    }

    @Override
    protected LocatorNodeImpl getCopy() {
      return new FakeGrandchildLocator(getFilter(), getScopeTransformer());
    }
  }
}
