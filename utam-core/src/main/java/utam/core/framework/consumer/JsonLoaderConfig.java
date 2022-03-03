package utam.core.framework.consumer;

import static utam.core.driver.DriverConfig.DEFAULT_EXPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_IMPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_POLLING_INTERVAL;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.core.driver.DriverConfig;
import utam.core.framework.UtamCoreError;
import utam.core.framework.context.StringValueProfile;

/**
 * JSON mapping for config of a UTAM Loader
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonLoaderConfig {

  static final String ERR_READING_LOADER_CONFIG = "error while reading config '%s' for UTAM loader";
  static final String ERR_CANT_FIND_LOADER_CONFIG = "can't find loader config '%s'";
  static final String ERR_DUPLICATE_PROFILE = "Profile '%s = %s' is already configured";
  // if profile is set at the loader level, then it applies to all modules
  private final List<Profile> profiles = new ArrayList<>();
  final DriverConfig driverConfig;
  private final List<Module> modules = new ArrayList<>();

  /**
   * Initializes a new instance of the JsonLoaderConfig class
   *
   * @param bridgeAppTitleStr mobile only: bridge app title
   * @param timeoutsConfig the driver configuration
   * @param modules      the list of modules
   * @param profiles     the list of profiles
   */
  @JsonCreator
  JsonLoaderConfig(
      @JsonProperty(value = "bridgeAppTitle") String bridgeAppTitleStr,
      @JsonProperty(value = "timeouts") TimeoutsJsonMapping timeoutsConfig,
      @JsonProperty(value = "modules") List<Module> modules,
      @JsonProperty(value = "profiles") List<Profile> profiles) {
    String bridgeAppTitle = bridgeAppTitleStr == null? "" : bridgeAppTitleStr;
    this.driverConfig = timeoutsConfig == null ? new DriverConfig(bridgeAppTitle) : timeoutsConfig.getDriverConfig(bridgeAppTitle);
    if (modules != null) {
      this.modules.addAll(modules);
    }
    if (profiles != null) {
      this.profiles.addAll(profiles);
    }
  }

  /**
   * create empty loader config without JSON file
   */
  public JsonLoaderConfig() {
    this("", null, new ArrayList<>(), new ArrayList<>());
  }

  /**
   * loads config object from a File
   *
   * @param jsonConfigFile JSON file with config
   * @return config object
   */
  static JsonLoaderConfig loadConfig(File jsonConfigFile) {
    try {
      return new ObjectMapper().readValue(jsonConfigFile, JsonLoaderConfig.class);
    } catch (IOException e) {
      throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, jsonConfigFile.toString()),
          e);
    }
  }

  /**
   * loads config object from resources
   *
   * @param resourceName name of the JSON file with config in resources, for example
   *                     "utam.loader.json"
   * @return config object
   */
  static JsonLoaderConfig loadConfig(String resourceName) {
    URL url = JsonLoaderConfig.class.getClassLoader().getResource(resourceName);
    if (url == null) {
      throw new UtamCoreError(String.format(ERR_CANT_FIND_LOADER_CONFIG, resourceName));
    }
    try {
      return new ObjectMapper().readValue(url, JsonLoaderConfig.class);
    } catch (IOException e) {
      throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, url), e);
    }
  }

  /**
   * get list of configured modules
   *
   * @return list of modules from config
   */
  public List<Module> getModules() {
    return modules;
  }

  /**
   * get profiles configured for module. used in tests
   *
   * @param module module instance
   * @return list of configured profiles
   */
  List<utam.core.framework.context.Profile> getModuleProfiles(Module module) {
    return new ArrayList<>(getModuleToProfilesMapping()
        .get(module.getName())
        .values());
  }

  /**
   * set profile at config level. used in tests
   *
   * @param profile profile instance
   */
  void setProfile(Profile profile) {
    this.profiles.add(profile);
  }

  /**
   * for each configured module, set map of configured profiles
   *
   * @return profiles assigned to a module: key is module name, value is pairs of profileKey/profile
   */
  Map<String, Map<String, utam.core.framework.context.Profile>> getModuleToProfilesMapping() {
    Map<String, Map<String, utam.core.framework.context.Profile>> pageObjectModules = new HashMap<>();
    for (Module module : modules) {
      String moduleName = module.getName();
      Map<String, utam.core.framework.context.Profile> mappedProfiles = new HashMap<>();
      for (utam.core.framework.context.Profile profile : module.getModuleProfiles(this.profiles)) {
        mappedProfiles.put(profile.getKey(), profile);
      }
      pageObjectModules.put(moduleName, mappedProfiles);
    }
    return pageObjectModules;
  }

  /**
   * JSON mapper class for loader config to read and write. Should be kept separate from compiler
   * config
   *
   * @author elizaveta.ivanova
   * @since 234
   */
  public static class Module {

    // annotation is required for serializer to write into output
    @JsonProperty(value = "profiles")
    private final List<Profile> profiles = new ArrayList<>();

    // annotation is required for serializer to write into output
    @JsonProperty(value = "name")
    private final String name;

    /**
     * Initializes a new instance of the Module class
     * @param name     name of the module
     * @param profiles profiles defined for the module
     */
    @JsonCreator
    public Module(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "profiles") List<Profile> profiles) {
      this.name = name;
      if (profiles != null) {
        this.profiles.addAll(profiles);
      }
    }

    // prevents duplicate profile definitions inside a module
    private static void checkDuplicateProfile(
        List<utam.core.framework.context.Profile> list,
        utam.core.framework.context.Profile profile) {
      if (list.contains(profile)) {
        throw new UtamCoreError(
            String.format(ERR_DUPLICATE_PROFILE, profile.getName(), profile.getValue()));
      }
      list.add(profile);
    }

    /**
     * module profiles can be combined with shared profiles, but there should not be duplicates
     *
     * @param sharedProfiles profiles shared by all modules
     * @return list of both module and shared profiles
     */
    public Set<utam.core.framework.context.Profile> getModuleProfiles(List<Profile> sharedProfiles) {
      List<utam.core.framework.context.Profile> allProfiles = new ArrayList<>();
      this.profiles.stream()
          .flatMap(profile -> profile.getProfiles().stream())
          .forEach(profile -> checkDuplicateProfile(allProfiles, profile));
      sharedProfiles.stream()
          .flatMap(profile -> profile.getProfiles().stream())
          .forEach(profile -> checkDuplicateProfile(allProfiles, profile));
      return new HashSet<>(allProfiles);
    }

    /**
     * Gets the name of the module
     *
     * @return the name of the module
     */
    public String getName() {
      return name;
    }

    /**
     * set profile at module level. used in tests
     *
     * @param profile profile instance
     */
    void setProfile(Profile profile) {
      this.profiles.add(profile);
    }
  }

  /**
   * JSON mapper class for loader config to read and write. Should be kept separate from compiler
   * config
   *
   * @author elizaveta.ivanova
   * @since 234
   */
  public static class Profile {

    // annotation is required for serializer to write into output
    @JsonProperty(value = "name")
    final String name;
    // annotation is required for serializer to write into output
    @JsonProperty(value = "values")
    final String[] values;

    /**
     * Initializes a new instance of the Profile class
     * @param name   name of the profile
     * @param values array of the values defined in the profile
     */
    @JsonCreator
    public Profile(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "values", required = true) String[] values) {
      this.name = name;
      this.values = values;
    }

    /**
     * build and return list of context profiles from config
     *
     * @return list of profiles
     */
    List<utam.core.framework.context.Profile> getProfiles() {
      return Stream.of(values).map(value -> new StringValueProfile(name, value))
          .collect(Collectors.toList());
    }
  }

  /**
   * mapping for JSON with timeouts in loader config
   *
   * @since 236
   * @author elizaveta.ivanova
   */
  static class TimeoutsJsonMapping {

    private final Duration implicitTimeout;
    private final Duration explicitTimeout;
    private final Duration pollingInterval;

    @JsonCreator
    TimeoutsJsonMapping(
        @JsonProperty(value = "implicitTimeout") Integer implicitTimeoutMsec,
        @JsonProperty(value = "explicitTimeout") Integer explicitTimeoutMsec,
        @JsonProperty(value = "pollingInterval") Integer pollingIntervalMsec) {
      this.implicitTimeout = implicitTimeoutMsec == null ? DEFAULT_IMPLICIT_TIMEOUT
          : Duration.ofMillis(implicitTimeoutMsec);
      this.explicitTimeout = explicitTimeoutMsec == null ? DEFAULT_EXPLICIT_TIMEOUT
          : Duration.ofMillis(explicitTimeoutMsec);
      this.pollingInterval = pollingIntervalMsec == null ? DEFAULT_POLLING_INTERVAL
          : Duration.ofMillis(pollingIntervalMsec);
    }

    DriverConfig getDriverConfig(String bridgeAppTitle) {
      return new DriverConfig(implicitTimeout, explicitTimeout, pollingInterval, bridgeAppTitle);
    }
  }
}
