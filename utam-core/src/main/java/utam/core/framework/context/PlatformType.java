package utam.core.framework.context;

public enum PlatformType {
  NONE(""),
  WEB("web"),
  NATIVE("native");

  private static final String PLATFORM_PROFILE_KEY = "platform";
  public static final Profile PLATFORM_WEB = new StringValueProfile(PLATFORM_PROFILE_KEY, "web");
  public static final Profile PLATFORM_IOS = new StringValueProfile(PLATFORM_PROFILE_KEY, "ios");
  public static final Profile PLATFORM_ANDROID = new StringValueProfile(PLATFORM_PROFILE_KEY,
      "android");
  private final String name;

  PlatformType(String name) {
    this.name = name;
  }

  public static PlatformType fromString(String string) {
    if (string == null || string.isEmpty()) {
      return NONE;
    }
    for (PlatformType type : PlatformType.values()) {
      if (type.name().toLowerCase().equals(string)) {
        return type;
      }
    }
    throw new IllegalArgumentException(String.format("Unknown platform type '%s'", string));
  }

  public String getAnnotation() {
    return String.format("%s.%s", getClass().getSimpleName(), name());
  }

  public String getName() {
    return name;
  }
}
