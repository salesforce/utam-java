/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.core.framework.UtamLogger.info;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import utam.core.framework.consumer.JsonInjectionsConfig.ImplementationPair;
import utam.core.framework.consumer.JsonInjectionsConfig.Mapping;
import utam.core.framework.consumer.JsonInjectionsConfig.ProfileImplementations;
import utam.core.framework.context.Profile;

/**
 * Compiler generates output file with dependencies injections. This class creates JSON object of
 * same format as config and defines custom serialization
 *
 * @author elizaveta.ivanova
 * @since 238
 */
class JsonCompilerOutput {

  private final Mapping mapping;

  /**
   * construct compiler output JSON mapping object
   *
   * @param dependenciesMap map of dependencies passed from compiler/runner
   */
  JsonCompilerOutput(Map<Profile, Map<String, String>> dependenciesMap) {
    mapping = new Mapping();
    for (Profile profile : dependenciesMap.keySet()) {
      Map<String, String> configToWrite = dependenciesMap.get(profile);
      if (!configToWrite.isEmpty()) {
        String profileName = profile.getName();
        ProfileImplementations mappingProfile = mapping.getImplementationsMap(profileName);
        String profileValue = profile.getValue();
        List<ImplementationPair> mappingPairs =
            configToWrite.entrySet().stream()
                .map(entry -> new ImplementationPair(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList());
        mappingProfile.setPairs(profileValue, mappingPairs);
        mapping.setImplementationsMap(profileName, mappingProfile);
      }
    }
  }

  /**
   * write config to the output
   *
   * @param writer target file
   * @throws IOException if file operation fails
   */
  private void writeConfig(Writer writer) throws IOException {
    SimpleModule module = new SimpleModule();
    module.addSerializer(Mapping.class, new Serializer());
    ObjectMapper mapper = new ObjectMapper();
    DefaultPrettyPrinter formatter =
        new DefaultPrettyPrinter()
            .withObjectIndenter(new DefaultIndenter("  ", "\n"))
            .withArrayIndenter(new DefaultIndenter("  ", "\n"));
    mapper.registerModule(module).writer(formatter).writeValue(writer, mapping);
  }

  void writeConfigToFile(String configPath) {
    if (mapping.isEmpty()) {
      info("skip writing empty dependencies config " + configPath);
      return;
    }
    info("write dependencies config " + configPath);
    try {
      Writer writer = new FileWriter(configPath);
      writeConfig(writer);
    } catch (IOException e) {
      String err = String.format("error creating dependencies config %s", configPath);
      throw new UtamRunnerError(err, e);
    }
  }

  /**
   * used in tests - write config to string
   *
   * @return written config as a string
   */
  String writeConfigToString() {
    if (mapping.isEmpty()) {
      return "";
    }
    try {
      Writer writer = new StringWriter();
      writeConfig(writer);
      return writer.toString();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  /**
   * custom serializer to write JSON object as a file
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  private static class Serializer extends JsonSerializer<Mapping> {

    /**
     * serializer implementation
     *
     * @param value mapping object
     * @param output output writer
     * @param serializers provider
     * @throws IOException if write operation fail
     */
    @Override
    public void serialize(Mapping value, JsonGenerator output, SerializerProvider serializers)
        throws IOException {
      output.writeStartObject();
      for (String profileName : value.getProfileNames()) {
        ProfileImplementations mapping = value.getImplementationsMap(profileName);
        output.writeFieldName(profileName);
        output.writeStartObject();
        // writeMappingProfile - start
        for (String profileValue : mapping.getProfileValues()) {
          output.writeFieldName(profileValue);
          List<ImplementationPair> pairs = mapping.getPairs(profileValue);
          output.writeObject(pairs);
        }
        // write mapping profile - end
        output.writeEndObject();
      }
      output.writeEndObject();
      output.writeRaw("\n");
    }
  }
}
