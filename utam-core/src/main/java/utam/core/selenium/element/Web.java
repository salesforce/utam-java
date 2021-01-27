package utam.core.selenium.element;

import java.util.Objects;

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
