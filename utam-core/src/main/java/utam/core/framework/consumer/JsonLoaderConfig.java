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

  static final String ERR_READING_LOADER_CONFIG = "error while reading loader config ";
  static final String ERR_CANT_FIND_LOADER_CONFIG = "can't find loader config ";
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
   * loads config object from a File
   *
   * @param jsonConfigFile JSON file with config
   * @return config object
   */
  static JsonLoaderConfig loadConfig(File jsonConfigFile) {
    try {
      return new ObjectMapper().readValue(jsonConfigFile, JsonLoaderConfig.class);
    } catch (IOException e) {
      throw new UtamCoreError(ERR_READING_LOADER_CONFIG + jsonConfigFile.toString(), e);
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
      throw new UtamCoreError(ERR_CANT_FIND_LOADER_CONFIG + resourceName);
    }
    try {
      return new ObjectMapper().readValue(url, JsonLoaderConfig.class);
    } catch (IOException e) {
      throw new UtamCoreError(ERR_READING_LOADER_CONFIG + resourceName, e);
    }
  }
}
