/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anEmptyMap;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.JsonInjectionsConfig.ERR_WHILE_READING_CONFIG;

import com.fasterxml.jackson.core.JsonParseException;
import java.util.Map;
import org.testng.annotations.Test;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.framework.context.StringValueProfile;

/**
 * test reading dependency injections config
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonInjectionsConfigTests {

  @Test
  public void testMissingConfigReturnsEmptyMap() {
    assertThat(new JsonInjectionsConfig().readDependenciesConfig("notexisting"), is(anEmptyMap()));
  }

  @Test
  public void testWrongFormatThrows() {
    UtamCoreError e = expectThrows(UtamCoreError.class,
        () -> new JsonInjectionsConfig().readDependenciesConfig("config/wrongFormat"));
    assertThat(e.getCause(), instanceOf(JsonParseException.class));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_WHILE_READING_CONFIG, "config/wrongFormat.config.json")));
  }

  @Test
  public void testEmptyMapping() {
    Map<String, ProfileContext> map = new JsonInjectionsConfig()
        .readDependenciesConfig("config/emptyProfiles");
    assertThat(map, is(anEmptyMap()));
  }

  @Test
  public void testReloadingProfile() {
    UtamLoaderConfig loaderConfig = new UtamLoaderConfigImpl("config/module1", "config/module2",
        "nonexisting");
    // read from module1.config.json
    loaderConfig.setProfile(new StringValueProfile("name", "value1"));
    PageObjectContext pageObjectContext = loaderConfig.getPageContext();
    PageObject instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));

    // read from module1.config.json
    loaderConfig.setProfile(new StringValueProfile("name", "value2"));
    pageObjectContext = loaderConfig.getPageContext();
    instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));

    // default implementing class because there is nothing set for this profile
    loaderConfig.setProfile(new StringValueProfile("name", "value3"));
    pageObjectContext = loaderConfig.getPageContext();
    instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testReloadingProfileCombineWithProperties() {
    UtamLoaderConfig loaderConfig = new UtamLoaderConfigImpl("config/module1", "config/module3");
    // read from module1.config.json
    loaderConfig.setProfile(new StringValueProfile("name", "value1"));
    PageObjectContext pageObjectContext = loaderConfig.getPageContext();
    PageObject instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));

    // read from module3_name_value2_config.properties
    loaderConfig.setProfile(new StringValueProfile("name", "value2"));
    pageObjectContext = loaderConfig.getPageContext();
    instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testMergingTwoJsonConfigs() {
    UtamLoaderConfig loaderConfig = new UtamLoaderConfigImpl("config/module1", "config/module2");
    loaderConfig.setProfile(new StringValueProfile("name2", "value2"));
    PageObjectContext pageObjectContext = loaderConfig.getPageContext();
    // read from module1.config.json
    PageObject instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
    // read from module2.config.json
    instance = pageObjectContext.getBean(TestLoaderConfigDefault.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testMergingJsonAndPropertyConfigs() {
    UtamLoaderConfig loaderConfig = new UtamLoaderConfigImpl("config/module1", "config/module2");
    loaderConfig.setProfile(TestNonStringProfile.ONE);
    PageObjectContext pageObjectContext = loaderConfig.getPageContext();
    // read from module1.config.json
    PageObject instance = pageObjectContext.getBean(TestLoaderConfigDefault.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));

    // read from module2_test_one.config.json
    instance = pageObjectContext.getBean(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testSwitchProfile() {
    UtamLoaderConfig loaderConfig = new UtamLoaderConfigImpl();
    loaderConfig.setDependencyModule("config/module1");
    loaderConfig.setProfile(TestNonStringProfile.ONE);
    PageObject instance = loaderConfig.getPageContext().getBean(TestLoaderConfigDefault.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));

    loaderConfig.setProfile(TestNonStringProfile.TWO);
    instance = loaderConfig.getPageContext().getBean(TestLoaderConfigDefault.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  /**
   * enum profile can behave differently from string when used as a map key
   */
  enum TestNonStringProfile implements Profile {
    ONE, TWO;

    @Override
    public String getName() {
      return "test";
    }

    @Override
    public String getValue() {
      return name().toLowerCase();
    }
  }
}
