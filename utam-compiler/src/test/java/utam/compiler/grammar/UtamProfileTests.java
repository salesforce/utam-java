/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerError;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.translator.StringValueProfileConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * tests for profile deserializer
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamProfileTests {

  @Test
  public void testGetPageObjectProfilesWithNullProfile() {
    List<Profile> profiles =
        new DeserializerUtilities()
            .getResultFromString("{}")
            .getPageObject()
            .getImplementation()
            .getProfiles();
    assertThat(profiles, is(empty()));
  }

  @Test
  public void testDeserializerWrongValueFormatThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("profile", "[ { \"name\" : {}} ]");
    test.addString("implements", "my/pageobjects/type");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 806: profile \"name\": values can either be string or non-empty string array"));
  }

  @Test
  public void testDeserializerWrongValueArrayFormatThrows() {
    JsonBuilderTestUtility jsonTest = new JsonBuilderTestUtility();
    jsonTest.addRawString("profile", "[ { \"name\" : [1]} ]");
    jsonTest.addString("implements", "my/pageobjects/type");
    Exception e = jsonTest.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 11: profile \"name\": array member should be a non-empty string, instead found"
                + " number"));
  }

  @Test
  public void testDeserializerDuplicateValueThrows() {
    JsonBuilderTestUtility jsonTest = new JsonBuilderTestUtility();
    jsonTest.addRawString("profile", "[ { \"name\" : [\"same\", \"same\"] } ]");
    jsonTest.addString("implements", "my/pageobjects/type");
    Exception e = jsonTest.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString("error 802: profile \"name\": duplicate profile value \"same\""));
  }

  @Test
  public void testDeserializerDuplicateNameThrows() {
    String json =
        "{ \"profile\" : [ { \"name\" : \"value\" }, { \"name\" : \"value\" } ] , \"implements\":"
            + " \"my/pageobjects/type\"}";
    Exception e = expectCompilerError(json);
    assertThat(
        e.getMessage(), containsString("error 801: \"profile\": duplicate profile name \"name\""));
  }

  @Test
  public void testDeserializerEmptyArrayThrows() {
    JsonBuilderTestUtility jsonTest = new JsonBuilderTestUtility();
    jsonTest.addRawString("profile", "[]");
    jsonTest.addString("implements", "my/pageobjects/type");
    Exception e = jsonTest.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 12: page object root: " + "property \"profile\" should be a non-empty array"));
  }

  @Test
  public void testDeserializerProfileValueAsString() {
    String json =
        "{ \"profile\" : [ { \"name\" : \"value\" } ] , \"implements\": \"my/pageobjects/type\"}";
    DeserializerUtilities utilities = new DeserializerUtilities();
    TranslatorConfig translatorConfig = utilities.getTranslatorConfig();
    translatorConfig.getConfiguredProfiles().add(new StringValueProfileConfig("name", "value"));
    List<Profile> profiles =
        utilities.getResultFromString(json).getPageObject().getImplementation().getProfiles();
    assertThat(profiles, hasSize(1));
    assertThat(profiles.get(0), is(equalTo(new StringValueProfile("name", "value"))));
  }

  @Test
  public void testDeserializerNotConfiguredProfileThrows() {
    String json =
        "{ \"profile\" : [ { \"name1\" : \"value1\" } ] , \"implements\": \"my/pageobjects/type\"}";
    Exception e = expectCompilerError(json);
    assertThat(
        e.getMessage(),
        containsString(
            "error 804: profile with name \"name1\" is not configured, make sure it's in compiler"
                + " config"));
  }

  @Test
  public void testNonConfiguredProfileValueThrows() {
    String json =
        "{ \"profile\" : [ { \"name\" : \"error\" } ] , \"implements\": \"my/pageobjects/type\"}";
    DeserializerUtilities utilities = new DeserializerUtilities();
    TranslatorConfig translatorConfig = utilities.getTranslatorConfig();
    translatorConfig.getConfiguredProfiles().add(new StringValueProfileConfig("name", "value"));
    Exception e =
        expectThrows(
            UtamError.class,
            () ->
                utilities
                    .getResultFromString(json)
                    .getPageObject()
                    .getImplementation()
                    .getProfiles());
    assertThat(
        e.getMessage(),
        containsString(
            "error 803: profile { \"name\": \"error\" } is not configured, make sure it's in"
                + " compiler config"));
  }

  @Test
  public void testMoreThanOneProfileThrows() {
    String json =
        "{ \"profile\" : [ { \"name1\" : \"value1\", \"name2\" : \"value2\" } ] , \"implements\":"
            + " \"my/pageobjects/type\"}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(), containsString("error 800: profile format is incorrect"));
  }

  @Test
  public void testEmptyProfileThrows() {
    String json = "{ \"profile\" : [ { } ] , \"implements\": \"my/pageobjects/type\"}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(), containsString("error 800: profile format is incorrect"));
  }
}
