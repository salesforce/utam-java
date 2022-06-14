package utam.core.framework.consumer;

import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES;
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
import java.util.HashSet;
import java.util.Set;
import utam.core.framework.UtamCoreError;

/**
 * JSON mapping for config of a UTAM Loader
 *
 * @author elizaveta.ivanova
 * @since 234
 */
class JsonLoaderConfig {

  final Set<String> modules;
  // driver timeouts
  final Duration implicitTimeout;
  final Duration explicitTimeout;
  final Duration pollingInterval;
  // mobile bridge app title
  final String bridgeAppTitle;

  /**
   * @param bridgeAppTitleStr   mobile only: bridge app title
   * @param implicitTimeoutMsec implicit timeout in msec
   * @param explicitTimeoutMsec explicit timeout in msec
   * @param pollingIntervalMsec polling interval in msec
   * @param modules             list of dependencies modules
   */
  @JsonCreator
  JsonLoaderConfig(
      @JsonProperty(value = "bridgeAppTitle") String bridgeAppTitleStr,
      @JsonProperty(value = "implicitTimeout") Integer implicitTimeoutMsec,
      @JsonProperty(value = "explicitTimeout") Integer explicitTimeoutMsec,
      @JsonProperty(value = "pollingInterval") Integer pollingIntervalMsec,
      @JsonProperty(value = "dependencies") Set<String> modules) {
    // default has to be empty string, not null
    this.bridgeAppTitle = bridgeAppTitleStr == null ? "" : bridgeAppTitleStr;
    this.implicitTimeout = implicitTimeoutMsec == null ? DEFAULT_IMPLICIT_TIMEOUT
        : Duration.ofMillis(implicitTimeoutMsec);
    this.explicitTimeout = explicitTimeoutMsec == null ? DEFAULT_EXPLICIT_TIMEOUT
        : Duration.ofMillis(explicitTimeoutMsec);
    this.pollingInterval = pollingIntervalMsec == null ? DEFAULT_POLLING_INTERVAL
        : Duration.ofMillis(pollingIntervalMsec);
    this.modules = modules == null ? new HashSet<>() : modules;
  }

  /**
   * create empty loader config without JSON file
   */
  JsonLoaderConfig() {
    this("", null, null, null, null);
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
      throw new UtamCoreError(deserializer.getErrorMessage(), e);
    }
  }

  /**
   * Helper class to deserialize loader config
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  private static abstract class Deserializer {

    static final String ERR_READING_LOADER_CONFIG = "error while reading loader config ";

    abstract JsonLoaderConfig deserialize() throws IOException;

    abstract String getErrorMessage();

    final ObjectMapper getJsonMapper() {
      ObjectMapper mapper = new ObjectMapper();
      mapper.disable(FAIL_ON_UNKNOWN_PROPERTIES);
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
