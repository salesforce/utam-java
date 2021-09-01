/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.profiles;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.context.MobilePlatformType.ANDROID;
import static utam.core.framework.context.MobilePlatformType.IOS;
import static utam.core.framework.context.MobilePlatformType.IOS_PHONE;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.function.BiFunction;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.translator.DefaultTranslatorRunner;
import utam.compiler.translator.JsonCompilerConfig;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.UtamLoaderConfig;
import utam.core.framework.consumer.UtamLoaderConfigImpl;
import utam.core.framework.consumer.UtamLoaderImpl;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.MobilePlatformType;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.profiles.pageobjects.DeviceInterface;
import utam.profiles.pageobjects.OnlyAndroidInterface;
import utam.profiles.pageobjects.OnlyDefault;
import utam.profiles.pageobjects.impl.DeviceDefaultImplementationImpl;
import utam.profiles.pageobjects.impl.DeviceIOSPhoneImpl;
import utam.profiles.pageobjects.impl.DeviceIOSandIOSTabletImpl;
import utam.profiles.pageobjects.impl.OnlyDefaultImplementationImpl;

/**
 * test POs with profiles including generated dependencies and loader
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class ProfilesGenerationTests {

  private Map<Profile, Properties> configs;

  private static TranslatorConfig getTranslatorConfigForProfiles() throws IOException {
    String USER_ROOT = System.getProperty("user.dir");
    JsonCompilerConfig jsonConfig = new JsonCompilerConfig(
        new File(USER_ROOT + "/src/test/resources/profiles/profiles.compiler.json"),
        new File(USER_ROOT)
    );
    return jsonConfig.getTranslatorConfig(GuardrailsMode.WARNING);
  }

  /**
   * use only if need to regenerate classes ! do not delete !
   *
   * @throws IOException IO error
   */
  @SuppressWarnings("unused")
  private static void generateClassesHelper() throws IOException {
    TranslatorConfig config = getTranslatorConfigForProfiles();
    TranslatorRunner translator = new DefaultTranslatorRunner(config);
    translator.run();
    translator.write();
  }

  /**
   * generate Page Objects and write dependencies configs
   *
   * @throws IOException IO error
   */
  @BeforeClass
  public void prepareBeans() throws IOException {
    TranslatorConfig config = getTranslatorConfigForProfiles();
    TestProfilesDefaultTranslatorRunner translator = new TestProfilesDefaultTranslatorRunner(
        config);
    translator.run();
    // do not write classes again!
    translator.writeDependenciesConfigs();
    this.configs = translator.configs;
  }

  /**
   * set active profile and get loader
   *
   * @param platform active platform
   * @return loader loader instance
   */
  private TestProfilesLoader getLoaderConfig(MobilePlatformType platform) {
    UtamLoaderConfig loaderConfig = new TestProfilesUtamLoaderConfig(
        configs);
    loaderConfig.setProfile(platform);
    return new TestProfilesLoader(loaderConfig);
  }

  @Test
  public void testPlatformProfilePicksRightImplementation() {
    TestProfilesLoader loader = getLoaderConfig(IOS);
    assertThat("impl with multiple profiles getting picked up if there is a match",
        loader.getBeanClass(DeviceInterface.class),
        is(equalTo(DeviceIOSandIOSTabletImpl.class)));
  }

  @Test
  public void testDefaultImplementation() {
    TestProfilesLoader loader = getLoaderConfig(IOS);
    assertThat("default impl should work regardless of active profile",
        loader.getBeanClass(OnlyDefault.class),
        is(equalTo(OnlyDefaultImplementationImpl.class)));
  }

  @Test
  public void testPlatformProfilePicksOneImplementation() {
    TestProfilesLoader loader = getLoaderConfig(IOS_PHONE);
    assertThat("impl with single profile getting picked up if there is a match",
        loader.getBeanClass(DeviceInterface.class),
        is(equalTo(DeviceIOSPhoneImpl.class)));
  }

  @Test
  public void testMatchingImplMissingNoDefaultThrows() {
    TestProfilesLoader loader = getLoaderConfig(IOS);
    // profile match is missing, default does not exist
    expectThrows(UtamCoreError.class, () -> loader.getBeanClass(OnlyAndroidInterface.class));
  }

  @Test
  public void testMatchingImplMissingDefaultIsPicked() {
    TestProfilesLoader loader = getLoaderConfig(ANDROID);
    assertThat("when match can't be found, prioritize default over random",
        loader.getBeanClass(DeviceInterface.class),
        is(equalTo(DeviceDefaultImplementationImpl.class)));
  }

  static class TestProfilesDefaultProfileContext extends DefaultProfileContext {

    TestProfilesDefaultProfileContext(String moduleName,
        Profile profile, Properties properties) {
      super(moduleName, profile, properties);
    }
  }

  static class TestProfilesUtamLoaderConfig extends UtamLoaderConfigImpl {

    TestProfilesUtamLoaderConfig(Map<Profile, Properties> configs) {
      super("profiles/profiles.loader.json", getProfileContextProvider(configs));
    }

    static BiFunction<String, Profile, ProfileContext> getProfileContextProvider(
        Map<Profile, Properties> configs) {
      return (module, profile) -> {
        Properties config = configs.get(profile) == null ? new Properties() : configs.get(profile);
        return new TestProfilesDefaultProfileContext(module, profile, config);
      };
    }
  }

  static class TestProfilesLoader extends UtamLoaderImpl {

    TestProfilesLoader(UtamLoaderConfig loaderConfig) {
      super(loaderConfig, null);
    }

    Class getBeanClass(Class type) {
      return super.getBean(type).getClass();
    }
  }

  static class TestProfilesDefaultTranslatorRunner extends DefaultTranslatorRunner {

    private final Map<Profile, Properties> configs = new HashMap<>();

    TestProfilesDefaultTranslatorRunner(TranslatorConfig translatorConfig) {
      super(translatorConfig);
    }

    @Override
    protected void writeDependenciesConfig(Profile profile, Properties configToWrite) {
      configs.put(profile, configToWrite);
    }
  }
}
