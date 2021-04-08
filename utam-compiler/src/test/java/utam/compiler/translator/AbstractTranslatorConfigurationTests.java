/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.declarative.translator.*;

import static utam.compiler.translator.AbstractTranslatorConfiguration.getHomeDirectory;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_JSON_SOURCES_NOT_CONFIGURED;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class AbstractTranslatorConfigurationTests {

  @Test
  public void testSourceNotConfigured() {
    TranslatorConfig translatorConfig = new Mock();
    TranslatorRunner translatorRunner = new DefaultTranslatorRunner(translatorConfig) {};
    UtamError e = expectThrows(UtamError.class, translatorRunner::run);
    assertThat(e.getMessage(), containsString(ERR_JSON_SOURCES_NOT_CONFIGURED));
  }

  @Test
  public void testGetHomeDir() {
    assertThrows(UtamError.class, () -> getHomeDirectory(this.getClass(), "error"));
  }

  static class Mock extends AbstractTranslatorConfiguration {

    Mock(TranslatorTargetConfig translatorTargetConfig) {
      super(translatorTargetConfig);
    }

    Mock(TranslatorTargetConfig translatorTargetConfig, TranslatorSourceConfig sourceConfig) {
      super(translatorTargetConfig);
      setSourceConfig(sourceConfig);
    }

    Mock(UnitTestRunner unitTestRunnerType,
         TranslatorTargetConfig translatorTargetConfig,
         TranslatorSourceConfig sourceConfig) {
      super(translatorTargetConfig);
      setSourceConfig(sourceConfig);
      setUnitTestRunner(unitTestRunnerType);
    }

    Mock() {
      super(new DefaultTargetConfigurationTests.Mock());
    }
  }
}
