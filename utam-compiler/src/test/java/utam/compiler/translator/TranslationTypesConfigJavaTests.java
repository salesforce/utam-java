/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.helpers.TranslationContext;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.translator.TranslationTypesConfigJava.Mask.pageObjects;
import static utam.compiler.translator.TranslationTypesConfigJava.Mask.utils;
import static utam.compiler.translator.TranslationTypesConfigJava.getWrongTypeError;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the TranslatorTypes class
 *
 * @author james.evans
 */
public class TranslationTypesConfigJavaTests {

  private static final TranslationTypesConfig TYPES = new TranslationTypesConfigJava();

  /** The getInterfaceType method should return a valid interface type */
  @Test
  public void testGetInterfaceType() {
    assertThat(
        TYPES.getInterfaceType("utam-test/pageObjects/test/testObject").getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestObject")));
    assertThat(
        TYPES.getInterfaceType("utam-test/pageObjects/test/object/testObject").getFullName(),
        is(equalTo("utam.test.pageobjects.test.object.TestObject")));
  }
  /** The getInterfaceType method should return a valid interface type with an abbreviated prefix */
  @Test
  public void testGetInterfaceTypeWithAbbreviatedPrefix() {
    assertThat(
        TYPES.getInterfaceType("utam/pageObjects/test/testObject").getFullName(),
        is(equalTo("utam.pageobjects.test.TestObject")));
  }

  /**
   * The getInterfaceType method should throw the proper exception with an invalid interface type
   */
  @Test
  public void testGetInterfaceTypeWithInvalidTypeThrows() {
    final String shortPageObjectUri = "utam-test/pageObjects";
    UtamError e = expectThrows(UtamError.class, () -> TYPES.getInterfaceType(shortPageObjectUri));
    assertThat(e.getMessage(), containsString(getWrongTypeError(shortPageObjectUri, pageObjects)));
  }

  /**
   * The getInterfaceType method should throw the proper exception with an invalid interface type
   */
  @Test
  public void testGetInterfaceTypeWithInvalidPrefixThrows() {
    final String pageObjectURI = "matu/pageObjects/testObject";
    UtamError e = expectThrows(UtamError.class, () -> TYPES.getInterfaceType(pageObjectURI));
    assertThat(e.getMessage(), containsString(getWrongTypeError(pageObjectURI, pageObjects)));
  }

  /**
   * The getInterfaceType method should throw the proper exception with an invalid interface type
   */
  @Test
  public void testGetInterfaceTypeWithInvalidAbbreviatedPrefixThrows() {
    final String pageObjectURI = "matu/pageObjects/test/testObject";
    UtamError e = expectThrows(UtamError.class, () -> TYPES.getInterfaceType(pageObjectURI));
    assertThat(e.getMessage(), containsString(getWrongTypeError(pageObjectURI, pageObjects)));
  }

  /** The getInterfaceType method should throw the proper exception with an incorrect mask type */
  @Test
  public void testGetInterfaceTypeWithIncorrectMaskThrows() {
    final String URI = "utam-test/utils/test/testObject";
    UtamError e = expectThrows(UtamError.class, () -> TYPES.getInterfaceType(URI));
    assertThat(e.getMessage(), containsString(getWrongTypeError(URI, pageObjects)));
  }

  @Test
  public void testGetUtilityType() {
    final String URI = "utam-test/utils/test/testObject";
    assertThat(TYPES.getUtilityType(URI).getFullName(), is(equalTo("utam.test.utils.test.TestObject")));
  }

  @Test
  public void testGetUtilityTypeError() {
    final String URI = "utam-test/pageObjects/test/testObject";
    UtamError e = expectThrows(UtamError.class, () -> TYPES.getUtilityType(URI));
    assertThat(e.getMessage(), containsString(getWrongTypeError(URI, utils)));
  }

  @Test
  public void testTypeValidationFromContext() {
    TranslationContext context = new TranslationContext(TranslatorMockUtilities.TEST_URI, getDefaultConfig());
    final String INVALID_PAGE_OBJECT_TYPE = "InvalidType";
    UtamError e = expectThrows(UtamError.class, () -> context.getType(INVALID_PAGE_OBJECT_TYPE));
    assertThat(
        e.getMessage(), containsString(getWrongTypeError(INVALID_PAGE_OBJECT_TYPE, pageObjects)));
  }

  @Test
  public void testIsUtamType() {
    final String INVALID_PAGE_OBJECT_TYPE = "InvalidType";
    assertThat(TranslationTypesConfigJava.isPageObjectType(INVALID_PAGE_OBJECT_TYPE), is(false));
    assertThat(TranslationTypesConfigJava.isPageObjectType(TEST_URI), is(true));
  }

  /** The getClassType method should return a valid interface type */
  @Test
  public void testGetClassType() {
    assertThat(
            TYPES.getClassType("utam-test/pageObjects/test/testObject").getFullName(),
            is(equalTo("utam.test.pageobjects.test.impl.TestObjectImpl")));
  }
}
