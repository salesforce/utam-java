/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import java.util.ArrayList;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;

public abstract class TranslatorMockUtilities {

  static final String PAGE_OBJECT_URI = "utam-test/pageObjects/test/testPageObject";
  static final String INTERFACE_ONLY_URI = "utam-test/pageObjects/test/testAbstractObject";
  static final String IMPL_ONLY_URI = "utam-test/pageObjects/test/testImplObject";
  static final String TEST_URI = "utam-test/pageObjects/test/test";
  static final String PAGE_OBJECT_INTERFACE_CLASS_NAME =
      "utam.test.pageobjects.test.TestPageObject";
  static final String PAGE_OBJECT_IMPL_CLASS_NAME =
      "utam.test.pageobjects.test.impl.TestPageObjectImpl";
  static final String INTERFACE_ONLY_CLASS_NAME = "utam.test.pageobjects.test.TestAbstractObject";
  static final String IMPL_ONLY_CLASS_NAME = "utam.test.pageobjects.test.impl.TestImplObjectImpl";
  static final String TEST_URI_INTERFACE_NAME = "utam.test.pageobjects.test.Test";
  static final String TEST_URI_CLASS_NAME = "utam.test.pageobjects.test.impl.TestImpl";
  static final String PAGE_OBJECT_SOURCE = "{}";

  public static DefaultTranslatorConfiguration getDefaultConfig() {
    TranslatorTargetConfig targetConfig = new DefaultTargetConfigurationTests.Mock();
    TranslatorSourceConfig sourceConfig = new DefaultSourceConfigurationTests.Mock();
    return new DefaultTranslatorConfiguration("", GuardrailsMode.ERROR, sourceConfig, targetConfig, new ArrayList<>());
  }

  public static TranslatorConfig getDefaultConfig(TranslationTypesConfig translationTypesConfig) {
    TranslatorTargetConfig targetConfig = new DefaultTargetConfigurationTests.Mock();
    TranslatorSourceConfig sourceConfig = new DefaultSourceConfigurationTests.Mock();
    return new DefaultTranslatorConfiguration("", GuardrailsMode.ERROR, translationTypesConfig, sourceConfig, targetConfig, new ArrayList<>());
  }
}
