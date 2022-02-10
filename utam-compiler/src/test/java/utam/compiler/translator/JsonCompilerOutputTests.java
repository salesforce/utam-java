/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;
import org.testng.annotations.Test;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * Tests for JSON file with compiler output
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonCompilerOutputTests {

  private static final Profile DUMMY_PROFILE = new StringValueProfile("name", "value");
  private static final String USER_ROOT = System.getProperty("user.dir");

  private static void runCompiler(String compilerConfig) throws IOException {
    JsonCompilerConfig jsonConfig = new JsonCompilerConfig(
        new File(USER_ROOT + "/src/test/resources/compiler/" + compilerConfig),
        new File(USER_ROOT)
    );
    TranslatorConfig config = jsonConfig.getTranslatorConfig(GuardrailsMode.WARNING);
    TranslatorRunner translator = new DefaultTranslatorRunner(config);
    translator.run();
    // do not write classes!
    translator.writeDependenciesConfigs();
  }

  private static String getCompilerOutputAsString(String compilerOutputFile) throws IOException {
    return new String(Files.readAllBytes(
        new File(USER_ROOT + "/src/test/resources/" + compilerOutputFile)
            .toPath()));
  }

  @Test
  public void testGeneratedConfigByRunner() throws IOException {
    runCompiler("test.compiler.json");
    String actualConfigStr = getCompilerOutputAsString("myModule.config.json");
    String expectedConfigStr = getCompilerOutputAsString("compiler/expected1.config.json");
    assertThat(actualConfigStr, is(equalTo(expectedConfigStr)));
  }

  @Test
  public void testEmptyMap() {
    String res = new JsonCompilerOutput(new HashMap<>()).writeConfigToString();
    assertThat(res, is(equalTo("{ }\n")));
  }

  @Test
  public void testEmptyPairs() {
    Map<Profile, Map<String, String>> dependenciesMap = new HashMap<>();
    dependenciesMap.put(DUMMY_PROFILE, new HashMap<>());
    String res = new JsonCompilerOutput(dependenciesMap).writeConfigToString();
    assertThat(res, is(equalTo("{ }\n")));
  }

  @Test
  public void testSameProfileDifferentPair() throws IOException {
    Map<Profile, Map<String, String>> dependenciesMap = new HashMap<>();
    Map<String, String> first = new HashMap<>();
    first.put("utam.my.PageObject1", "utam.my.PageObjectImpl1");
    first.put("utam.my.PageObject2", "utam.my.PageObjectImpl2");
    dependenciesMap.put(DUMMY_PROFILE, first);
    String actualConfigStr = new JsonCompilerOutput(dependenciesMap).writeConfigToString();
    String expectedConfigStr = getCompilerOutputAsString("compiler/expected2.config.json");
    assertThat(actualConfigStr, is(equalTo(expectedConfigStr)));
  }

  @Test
  public void testTwoProfileValuesDifferentPairs() throws IOException {
    Map<Profile, Map<String, String>> dependenciesMap = new HashMap<>();
    Map<String, String> first = new HashMap<>();
    first.put("utam.my.PageObject1", "utam.my.PageObjectImpl1");
    dependenciesMap.put(DUMMY_PROFILE, first);
    Map<String, String> second = new HashMap<>();
    second.put("utam.my.PageObject2", "utam.my.PageObjectImpl2");
    Profile profile = new StringValueProfile("name", "value2");
    dependenciesMap.put(profile, second);
    String actualConfigStr = new JsonCompilerOutput(dependenciesMap).writeConfigToString();
    String expectedConfigStr = getCompilerOutputAsString("compiler/expected3.config.json");
    assertThat(actualConfigStr, is(equalTo(expectedConfigStr)));
  }

  @Test
  public void testTwoProfileNamesDifferentPairs() throws IOException {
    Map<Profile, Map<String, String>> dependenciesMap = new HashMap<>();
    Map<String, String> first = new HashMap<>();
    first.put("utam.my.PageObject1", "utam.my.PageObjectImpl1");
    dependenciesMap.put(DUMMY_PROFILE, first);
    Map<String, String> second = new HashMap<>();
    second.put("utam.my.PageObject2", "utam.my.PageObjectImpl2");
    Profile profile = new StringValueProfile("name2", "value2");
    dependenciesMap.put(profile, second);
    String actualConfigStr = new JsonCompilerOutput(dependenciesMap).writeConfigToString();
    String expectedConfigStr = getCompilerOutputAsString("compiler/expected4.config.json");
    assertThat(actualConfigStr, is(equalTo(expectedConfigStr)));
  }
}
