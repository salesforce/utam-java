package utam.core.element;

/**
 * context of finding an element inside its parent, can be nullable (element does not exist) or
 * expand parent shadow
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface FindContext {

  boolean isExpandScopeShadowRoot();

  boolean isNullable();

  String getSeparator();

  enum Type implements FindContext {

    EXISTING,
    NULLABLE,
    EXISTING_IN_SHADOW,
    NULLABLE_IN_SHADOW;

    public static Type build(boolean isNullable, boolean isExpandsShadowRoot) {
      if (isNullable) {
        return isExpandsShadowRoot ? NULLABLE_IN_SHADOW : NULLABLE;
      }
      return isExpandsShadowRoot ? EXISTING_IN_SHADOW : EXISTING;
    }

    @Override
    public boolean isNullable() {
      return this == NULLABLE || this == NULLABLE_IN_SHADOW;
    }

    @Override
    public boolean isExpandScopeShadowRoot() {
      return this == EXISTING_IN_SHADOW || this == NULLABLE_IN_SHADOW;
    }

    @Override
    public String getSeparator() {
      return isExpandScopeShadowRoot() ? ">>" : ">";
    }
  }
}
