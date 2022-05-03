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
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamRootDescription.VERSION_TAG;

import java.util.List;
import org.testng.annotations.Test;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.framework.consumer.UtamError;

/**
 * test for root comments of a page object
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class UtamRootDescriptionTests {

  private static List<String> getInterfaceDescription(String jsonFile) {
    return new DeserializerUtilities()
        .getResultFromFile(jsonFile).getPageObject().getInterface().getDescription();
  }

  private static List<String> getImplementationDescription(String jsonFile) {
    return new DeserializerUtilities()
        .getResultFromFile(jsonFile).getPageObject().getImplementation().getDescription();
  }

  @Test
  public void testEmptyRootComments() {
    String json = "{}";
    PageObjectDeclaration declaration = new DeserializerUtilities().getResultFromString(json).getPageObject();
    // for interface
    List<String> description = declaration.getInterface().getDescription();
    assertThat(description, is(hasSize(3)));
    assertThat(declaration.getInterface().getDescription(), is(hasSize(3)));
    assertThat(description.get(0), containsString("created from JSON"));
    assertThat(description.get(1), containsString("@author UTAM"));
    assertThat(description.get(2), containsString(VERSION_TAG));

    // for impl
    description = declaration.getImplementation().getDescription();
    assertThat(description, is(hasSize(3)));
    assertThat(declaration.getInterface().getDescription(), is(hasSize(3)));
    assertThat(description.get(0), containsString("created from JSON"));
    assertThat(description.get(1), containsString("@author UTAM"));
    assertThat(description.get(2), containsString(VERSION_TAG));
  }

  @Test
  public void testDescriptionString() {
    // for interface
    List<String> description = getInterfaceDescription("generated/comments/verboseString.utam");
    assertThat(description, hasSize(4));
    assertThat(description.get(0),
        containsString("Declarative programming is a high-level programming concept"));
    assertThat(description.get(1), containsString("created from JSON"));
    assertThat(description.get(2), containsString("@author UTAM"));
    assertThat(description.get(3), containsString(VERSION_TAG));

    // for impl
    description = getImplementationDescription("generated/comments/verboseString.utam");
    assertThat(description.get(0),
        containsString("Declarative programming is a high-level programming concept"));
    assertThat(description.get(1), containsString("created from JSON"));
    assertThat(description.get(2), containsString("@author UTAM"));
    assertThat(description.get(3), containsString(VERSION_TAG));
  }

  @Test
  public void testDescriptionObject() {
    // for interface
    List<String> description = getInterfaceDescription("generated/comments/verboseObject.utam");
    assertThat(description, hasSize(6));
    assertThat(description.get(0), containsString("one"));
    assertThat(description.get(1), containsString("&lt;two&gt; &amp; *&#47;"));
    assertThat(description.get(2), containsString("created from JSON"));
    assertThat(description.get(3), containsString("@author records_team"));
    assertThat(description.get(4), containsString(VERSION_TAG));
    assertThat(description.get(5), containsString("@deprecated this class is outdated"));

    // for impl
    description = getImplementationDescription("generated/comments/verboseObject.utam");
    assertThat(description, hasSize(6));
    assertThat(description.get(0), containsString("one"));
    assertThat(description.get(1), containsString("&lt;two&gt; &amp; *&#47;"));
    assertThat(description.get(2), containsString("created from JSON"));
    assertThat(description.get(3), containsString("@author records_team"));
    assertThat(description.get(4), containsString(VERSION_TAG));
    assertThat(description.get(5), containsString("@deprecated this class is outdated"));
  }

  @Test
  public void testDeprecatedAnnotation() {
    PageObjectDeclaration pageObject = new DeserializerUtilities()
        .getResultFromFile("generated/comments/verboseObject.utam").getPageObject();
    List<AnnotationProvider> annotations = pageObject.getImplementation().getClassAnnotations();
    assertThat(annotations, hasSize(1));
    assertThat(annotations.get(0).getAnnotationText(), is(equalTo("@Deprecated")));
    assertThat(pageObject.getInterface().isDeprecated(), is(true));
  }

  @Test
  public void testDeprecatedAnnotationForInterface() {
    PageObjectDeclaration pageObject = new DeserializerUtilities()
        .getResultFromFile("generated/comments/verboseInterface.utam").getPageObject();
    assertThat(pageObject.getInterface().isDeprecated(), is(true));
  }

  @Test
  public void testFormatIsNotStringOrObject() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString("{ \"description\" : true }"));
    assertThat(e.getMessage(), containsString("error 906: format of the root description is incorrect"));
  }

  @Test
  public void testIncorrectTextInsideDescriptionThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString("{ \"description\" : { \"text\" : {} } }"));
    assertThat(e.getMessage(), containsString("error 906: format of the root description is incorrect"));
  }

  @Test
  public void testDescriptionObjectForInterfaceWithoutAuthor() {
    List<String> description = getInterfaceDescription("generated/comments/verboseInterface.utam");
    assertThat(description, hasSize(5));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("created from JSON"));
    assertThat(description.get(2), containsString("@author UTAM"));
    assertThat(description.get(3), containsString(VERSION_TAG));
    assertThat(description.get(4), containsString("@deprecated this class is outdated"));
  }
}
