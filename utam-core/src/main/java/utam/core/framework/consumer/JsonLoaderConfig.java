package utam.core.framework.consumer;

import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;
import utam.core.framework.UtamCoreError;

/**
 * JSON mapping for config of a UTAM Loader. This class has to be public as it's used in
 * RepositoryTransformer in distribution plugin.
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonLoaderConfig {

  // todo - injection filename should be configurable
  public static final String INJECTION_CONFIG_FILE_MASK = "%s.config.json";
  private final Set<String> injectionConfigs;

  /**
   * Create an instance of loader config
   *
   * @param injectionConfigs list of injection configs files names
   */
  @JsonCreator
  JsonLoaderConfig(@JsonProperty(value = "injectionConfigs") Set<String> injectionConfigs) {
    this.injectionConfigs = new HashSet<>();
    if (injectionConfigs != null) {
      this.injectionConfigs.addAll(injectionConfigs);
    }
  }

  /**
   * Create empty loader config without JSON file. This constructor is used by RepositoryTransformer
   * in distribution plugin.
   */
  protected JsonLoaderConfig() {
    this(null);
  }

  /**
   * Set injection config, used by RepositoryTransformer in distribution plugin
   *
   * @param moduleName name of the module, full file name will be set using
   *     INJECTION_CONFIG_FILE_MASK
   */
  protected final void setInjectionConfigFile(String moduleName) {
    if (moduleName != null) {
      injectionConfigs.add(String.format(INJECTION_CONFIG_FILE_MASK, moduleName));
    }
  }

  /**
   * Get all injection configurations file names, used by RepositoryTransformer in distribution
   * plugin
   *
   * @return set of strings
   */
  protected final Set<String> getInjectionConfigs() {
    return injectionConfigs;
  }

  /**
   * Load config from JSON
   *
   * @param deserializer helper to deserialize
   * @return loader configuration options
   */
  static JsonLoaderConfig loadConfig(Deserializer deserializer) {
    try {
      return deserializer.deserialize();
    } catch (IOException e) {
      throw new RuntimeException(deserializer.getErrorMessage(), e);
    }
  }

  /**
   * Helper class to deserialize loader config
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  abstract static class Deserializer {

    static final String ERR_READING_LOADER_CONFIG = "error while reading loader config ";

    abstract JsonLoaderConfig deserialize() throws IOException;

    abstract String getErrorMessage();

    final ObjectMapper getJsonMapper() {
      ObjectMapper mapper = new ObjectMapper();
      mapper.enable(FAIL_ON_UNKNOWN_PROPERTIES);
      return mapper;
    }
  }

  /**
   * Helper class to deserialize loader config from file
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  static class DeserializerFromFile extends Deserializer {

    private final File file;

    DeserializerFromFile(File file) {
      this.file = file;
    }

    @Override
    JsonLoaderConfig deserialize() throws IOException {
      return getJsonMapper().readValue(file, JsonLoaderConfig.class);
    }

    @Override
    String getErrorMessage() {
      return ERR_READING_LOADER_CONFIG + file.toString();
    }
  }

  /**
   * Helper class to deserialize loader config from URL
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  static class DeserializerFromUrl extends Deserializer {

    static final String ERR_CANT_FIND_LOADER_CONFIG = "can't find loader config ";
    private final URL url;
    private final String error;

    DeserializerFromUrl(String resourceName) {
      URL url = JsonLoaderConfig.class.getClassLoader().getResource(resourceName);
      if (url == null) {
        throw new UtamCoreError(ERR_CANT_FIND_LOADER_CONFIG + resourceName);
      }
      this.url = url;
      this.error = ERR_READING_LOADER_CONFIG + resourceName;
    }

    @Override
    JsonLoaderConfig deserialize() throws IOException {
      return getJsonMapper().readValue(url, JsonLoaderConfig.class);
    }

    @Override
    String getErrorMessage() {
      return error;
    }
  }
}
