package utam.core.framework.consumer;

import static utam.core.framework.consumer.UtamLoaderConfigImpl.ERR_DUPLICATE_PROFILE;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
  private final List<Module> modules = new ArrayList<>();
  // if profile is set at the loader level, then it applies to all modules
  final List<Profile> profiles = new ArrayList<>();

  @JsonCreator
  public JsonLoaderConfig(
      @JsonProperty(value = "modules", required = true) List<Module> modules,
      @JsonProperty(value = "profiles") List<Profile> profiles) {
    this.modules.add(new Module(null, new ArrayList<>()));
    this.modules.addAll(modules);
    if (profiles != null) {
      this.profiles.addAll(profiles);
    }
  }

  /**
   * create empty loader config without JSON file
   */
  public JsonLoaderConfig() {
    this(new ArrayList<>(), new ArrayList<>());
  }

  /**
   * loads config object from a File
   *
   * @param jsonConfigFile JSON file with config
   * @return config object
   */
  public static JsonLoaderConfig loadConfig(File jsonConfigFile) {
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
  public static JsonLoaderConfig loadConfig(String resourceName) {
    URL url = JsonLoaderConfig.class.getClassLoader().getResource(resourceName);
    if (url == null) {
      throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, resourceName));
    }
    try {
      return new ObjectMapper().readValue(url, JsonLoaderConfig.class);
    } catch (IOException e) {
      throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, url.toString()),
          e);
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
   * JSON mapper class for loader config to read and write. Should be kept separate from compiler
   * config
   *
   * @author elizaveta.ivanova
   * @since 234
   */
  public static class Module {

    // annotation is required for serializer to write into output
    @JsonProperty(value = "profiles")
    final List<Profile> profiles = new ArrayList<>();

    // annotation is required for serializer to write into output
    @JsonProperty(value = "name")
    private final String name;

    @JsonCreator
    public Module(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "profiles") List<Profile> profiles) {
      this.name = name;
      if (profiles != null) {
        this.profiles.addAll(profiles);
      }
    }

    /**
     * used for testing in downstream modules to check how profiles were setup. marked to ignore by
     * serializer
     *
     * @return list of profiles
     */
    @JsonIgnore
    public List<Profile> getRawProfiles() {
      return profiles;
    }

    /**
     * module profiles can be combined with shared profiles, but there should not be duplicates
     *
     * @param sharedProfiles profiles shared by all modules
     * @return list of both module and shared profiles
     */
    List<utam.core.framework.context.Profile> getModuleProfiles(List<Profile> sharedProfiles) {
      List<utam.core.framework.context.Profile> allProfiles = new ArrayList<>();
      allProfiles.add(DEFAULT_PROFILE);
      this.profiles.stream()
          .flatMap(profile -> profile.getProfiles().stream())
          .forEach(profile -> checkDuplicateProfile(allProfiles, profile));
      sharedProfiles.stream()
          .flatMap(profile -> profile.getProfiles().stream())
          .forEach(profile -> checkDuplicateProfile(allProfiles, profile));
      return allProfiles;
    }

    // prevents duplicate profile definitions inside a module
    private static void checkDuplicateProfile(
        List<utam.core.framework.context.Profile> list,
        utam.core.framework.context.Profile profile) {
      if (list.contains(profile)) {
        throw new UtamCoreError(String.format(ERR_DUPLICATE_PROFILE, profile.getName(), profile.getValue()));
      }
      list.add(profile);
    }

    public String getName() {
      return name;
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
}
