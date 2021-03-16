package utam.core.framework.context;

import utam.core.framework.consumer.UtamError;

public class StringValueProfile implements Profile {
  
  private static final String ERR_NAME_REQUIRED = 
      "profile name must not be null or the empty string";
  private static final String ERR_VALUES_REQUIRED = 
      "profile value must not be null or the empty string";
  private static final String PROFILE_CONFIG_PATTERN = "%s_%s_config.properties";

  // default implementations for interfaces
  public static final Profile DEFAULT_IMPL = new StringValueProfile("default", "impl");

  private final String name;
  private final String value;

  public StringValueProfile(String name, String value) {
    if (name == null || name.isEmpty()) {
      throw new UtamError(ERR_NAME_REQUIRED);
    }
    
    if (value == null || value.isEmpty()) {
      throw new UtamError(ERR_VALUES_REQUIRED);
    }
    
    this.name = name;
    this.value = value;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public String asKey() {
    return String.format("%s = %s", getName(), getValue());
  }

  @Override
  public int hashCode() {
    return asKey().hashCode();
  }

  @Override //without this can't use Profile as a key in map inside Runner
  public boolean equals(Object obj) {
    if(obj instanceof StringValueProfile) {
      return ((StringValueProfile) obj).asKey().equals(this.asKey());
    }
    return false;
  }

  @Override
  public String getConfigFileName() {
    return String.format(PROFILE_CONFIG_PATTERN, getName(), getValue());
  }
}
