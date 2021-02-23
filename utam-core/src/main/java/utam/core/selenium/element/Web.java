package utam.core.selenium.element;

import static utam.core.selenium.element.Selector.Type.ACCESSID;
import static utam.core.selenium.element.Selector.Type.CLASSCHAIN;
import static utam.core.selenium.element.Selector.Type.CSS;
import static utam.core.selenium.element.Selector.Type.UIAUTOMATOR;

import io.appium.java_client.MobileBy;
import java.util.Objects;
import org.openqa.selenium.By;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class Web {

    public static Selector byCss(String value) {
        return new SelectorImpl(Selector.Type.CSS, value);
    }

    protected static class SelectorImpl implements Selector {

      private final String stringValue;
      private final Type type;

      public SelectorImpl(Type type, String stringValue) {
        this.type = type;
        this.stringValue = stringValue;
      }

      @Override
      public String getValue() {
        return stringValue;
      }

      @Override
      public Type getType() {
        return type;
      }

      @Override
      public By by() {
        if (type == CSS) {
          return By.cssSelector(stringValue);
        }
        if (type == ACCESSID) {
          return MobileBy.AccessibilityId(stringValue);
        }
        if (type == CLASSCHAIN) {
          return MobileBy.iOSClassChain(stringValue);
        }
        if (type == UIAUTOMATOR) {
          return MobileBy.AndroidUIAutomator(stringValue);
        }
        throw new IllegalArgumentException("unsupported selector type " + type);
      }

      @Override
      public boolean equals(Object obj) {
        if (!(obj instanceof Selector)) {
          return false;
        }
        return ((Selector) obj).getType().equals(type)
            && ((Selector) obj).getValue().equals(getValue());
      }

      @Override
      public int hashCode() {
        return Objects.hash(stringValue);
      }
    }
}
