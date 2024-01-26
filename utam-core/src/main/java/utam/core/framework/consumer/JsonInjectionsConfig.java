/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import utam.core.framework.UtamCoreError;
import utam.core.framework.UtamLogger;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.ProfileContext;
import utam.core.framework.context.StringValueProfile;

/**
 * Compiler generates output file with dependencies injections
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonInjectionsConfig {

  static final String ERR_WHILE_READING_CONFIG = "Error while reading JSON config '%s'";
  private static final String ERR_CANT_FIND_CONFIG = "Injections config file '%s' not found";

  /**
   * read JSON config with a given module name
   *
   * @param filename dependencies config file name
   * @return map: key is profile key (ex."platformmobile")
   */
  Map<String, ProfileContext> readDependenciesConfig(String filename) {
    Map<String, ProfileContext> map = new HashMap<>();
    try {
      // with dependencies from different modules, there could be modules with same name
      List<URL> resources = getDependenciesConfigResources(filename);
      if (!resources.isEmpty()) {
        for (URL resource : resources) {
          UtamLogger.info(String.format("Reading injections config file %s", filename));
          try (InputStream resourceStream = resource.openStream();
              InputStreamReader resourceStreamReader = new InputStreamReader(resourceStream);
              BufferedReader resourceReader = new BufferedReader(resourceStreamReader)) {
            String resourceValue =
                resourceReader.lines().collect(Collectors.joining(System.lineSeparator()));
            if (!resourceValue.isBlank()) {
              Mapping mapping = new ObjectMapper().readValue(resourceValue, Mapping.class);
              mapping.setInjectionsMapping(map);
            }
          }
        }
      } else {
        // we can't throw here because of distribution plugin:
        // transformer of distribution plugin combines modules and generates loader config assuming
        // that injection config exists
        UtamLogger.warning(String.format(ERR_CANT_FIND_CONFIG, filename));
      }
    } catch (IOException e) {
      throw new UtamCoreError(String.format(ERR_WHILE_READING_CONFIG, filename), e);
    }
    return map;
  }

  /**
   * overridden in unit test to verify possible resources with same name
   *
   * @param filename name of the resource
   * @return list of found URLs
   * @throws IOException if file error happened
   */
  private List<URL> getDependenciesConfigResources(String filename) throws IOException {
    return Collections.list(getClass().getClassLoader().getResources(filename));
  }

  /**
   * mapping for JSON with dependency injection config file
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  @JsonDeserialize(using = Deserializer.class)
  public static class Mapping {

    private final Map<String, ProfileImplementations> mapping = new HashMap<>();

    /**
     * read mapping and set values to provided map
     *
     * @param injections map for mapping
     */
    void setInjectionsMapping(Map<String, ProfileContext> injections) {
      for (String profileName : mapping.keySet()) {
        ProfileImplementations mappingProfile = mapping.get(profileName);
        for (String profileValue : mappingProfile.getProfileValues()) {
          String profileKey = new StringValueProfile(profileName, profileValue).getKey();
          Map<Object, Object> map =
              mappingProfile.getPairs(profileValue).stream()
                  .collect(
                      Collectors.toMap(
                          ImplementationPair::getInterface, ImplementationPair::getImplementation));
          ProfileContext profileContext = new DefaultProfileContext(map);
          // there could be more than one config for module with same name, then we need to merge
          if (injections.containsKey(profileKey)) {
            ProfileContext existingContext = injections.get(profileKey);
            for (Class beanType : profileContext.getConfiguredBeans()) {
              existingContext.setBean(beanType, profileContext.getBeanName(beanType));
            }
          } else {
            injections.put(profileKey, profileContext);
          }
        }
      }
    }

    /**
     * for output compiler config: profile names
     *
     * @return set of profile names
     */
    public Set<String> getProfileNames() {
      return mapping.keySet();
    }

    /**
     * for output compiler config: get injections map
     *
     * @param profileName name of the profile to get the map
     * @return map as an object
     */
    public ProfileImplementations getImplementationsMap(String profileName) {
      return mapping.containsKey(profileName)
          ? mapping.get(profileName)
          : new ProfileImplementations();
    }

    /**
     * for output compiler config: set injections for a given profile
     *
     * @param profileName name of the profile
     * @param mappingProfile injections map
     */
    public void setImplementationsMap(String profileName, ProfileImplementations mappingProfile) {
      mapping.put(profileName, mappingProfile);
    }

    public boolean isEmpty() {
      return mapping.isEmpty();
    }
  }

  /**
   * mapping between profile value and implementations pairs
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class ProfileImplementations {

    private final Map<String, List<ImplementationPair>> mapping;

    ProfileImplementations() {
      mapping = new HashMap<>();
    }

    /**
     * for output compiler config: get possible values
     *
     * @return set of profile values
     */
    public Set<String> getProfileValues() {
      return mapping.keySet();
    }

    /**
     * for output compiler config: get pairs for the given profile value
     *
     * @param profileValue profile value
     * @return list of configured pairs
     */
    public List<ImplementationPair> getPairs(String profileValue) {
      return mapping.get(profileValue);
    }

    /**
     * for output compiler config: for a given profile value, set pairs
     *
     * @param profileValue profile value
     * @param pairs implementation pairs
     */
    public void setPairs(String profileValue, List<ImplementationPair> pairs) {
      if (mapping.containsKey(profileValue)) {
        mapping.get(profileValue).addAll(pairs);
      } else {
        mapping.put(profileValue, pairs);
      }
    }

    /**
     * used in serializer - check if mapping is empty
     *
     * @return boolean
     */
    boolean isEmpty() {
      return mapping.isEmpty();
    }
  }

  /**
   * pairs of interface - implementation for dependency injections
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class ImplementationPair {

    private final String interfaceName;
    private final String implementationName;

    @JsonCreator
    public ImplementationPair(
        @JsonProperty(value = "interface", required = true) String interfaceName,
        @JsonProperty(value = "implementation", required = true) String implementationName) {
      this.interfaceName = interfaceName;
      this.implementationName = implementationName;
    }

    /**
     * getter has to be public for serializer to work
     *
     * @return interface name
     */
    public String getInterface() {
      return interfaceName;
    }

    /**
     * getter has to be public for serializer to work
     *
     * @return implementing class name
     */
    public String getImplementation() {
      return implementationName;
    }
  }

  /**
   * custom deserializer to read injections config from JSON
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  private static class Deserializer extends JsonDeserializer<Mapping> {

    /**
     * custom deserializer to read injections config
     *
     * @param jp parser
     * @param deserializationContext context
     * @return mapping object
     * @throws IOException if parsing fails
     */
    @Override
    public Mapping deserialize(JsonParser jp, DeserializationContext deserializationContext)
        throws IOException {
      JsonNode node = jp.getCodec().readTree(jp);
      Iterator<String> profiles = node.fieldNames();
      Mapping result = new Mapping();
      ObjectMapper objectMapper = new ObjectMapper();
      while (profiles.hasNext()) {
        String profileName = profiles.next();
        JsonNode valuesNode = node.get(profileName);
        Iterator<String> profileValues = valuesNode.fieldNames();
        ProfileImplementations mappingProfile = new ProfileImplementations();
        while (profileValues.hasNext()) {
          String profileValue = profileValues.next();
          JsonNode pairsNode = valuesNode.get(profileValue);
          List<ImplementationPair> pairsList = new ArrayList<>();
          for (JsonNode pairNode : pairsNode) {
            ImplementationPair pair = objectMapper.treeToValue(pairNode, ImplementationPair.class);
            pairsList.add(pair);
          }
          if (!pairsList.isEmpty()) {
            mappingProfile.setPairs(profileValue, pairsList);
          }
        }
        if (!mappingProfile.isEmpty()) {
          result.setImplementationsMap(profileName, mappingProfile);
        }
      }
      return result;
    }
  }
}
