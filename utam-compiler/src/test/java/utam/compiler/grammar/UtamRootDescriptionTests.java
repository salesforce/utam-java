/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamRootDescription.ERR_FORMAT_ERROR;

import java.util.List;
import org.testng.annotations.Test;
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
    assertThat(description, is(hasSize(2)));
    assertThat(declaration.getInterface().getDescription(), is(hasSize(2)));
    assertThat(description.get(0), containsString("@author UTAM"));
    assertThat(description.get(1), containsString("@version"));

    // for impl
    description = declaration.getImplementation().getDescription();
    assertThat(description, is(hasSize(2)));
    assertThat(declaration.getInterface().getDescription(), is(hasSize(2)));
    assertThat(description.get(0), containsString("@author UTAM"));
    assertThat(description.get(1), containsString("@version"));
  }

  @Test
  public void testDescriptionString() {
    // for interface
    List<String> description = getInterfaceDescription("generated/comments/verboseString.utam");
    assertThat(description, hasSize(3));
    assertThat(description.get(0),
        containsString("Declarative programming is a high-level programming concept"));
    assertThat(description.get(1), containsString("@author UTAM"));
    assertThat(description.get(2), containsString("@version"));

    // for impl
    description = getImplementationDescription("generated/comments/verboseString.utam");
    assertThat(description, hasSize(3));
    assertThat(description.get(0),
        containsString("Declarative programming is a high-level programming concept"));
    assertThat(description.get(1), containsString("@author UTAM"));
    assertThat(description.get(2), containsString("@version"));
  }

  @Test
  public void testDescriptionObject() {
    // for interface
    List<String> description = getInterfaceDescription("generated/comments/verboseObject.utam");
    assertThat(description, hasSize(4));
    assertThat(description.get(0), containsString("one"));
    assertThat(description.get(1), containsString("two"));
    assertThat(description.get(2), containsString("@author records_team"));
    assertThat(description.get(3), containsString("@version"));

    // for impl
    description = getImplementationDescription("generated/comments/verboseObject.utam");
    assertThat(description, hasSize(4));
    assertThat(description.get(0), containsString("one"));
    assertThat(description.get(1), containsString("two"));
    assertThat(description.get(2), containsString("@author records_team"));
    assertThat(description.get(3), containsString("@version"));
  }

  @Test
  public void testFormatIsNotStringOrObject() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString("{ \"description\" : true }"));
    assertThat(e.getCause().getMessage(), containsString(ERR_FORMAT_ERROR));
  }

  @Test
  public void testIncorrectFormatOfDesscriptionObject() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString("{ \"description\" : { \"text\" : true } }"));
    assertThat(e.getCause().getMessage(), containsString(ERR_FORMAT_ERROR));
  }

  @Test
  public void testDescriptionObjectForInterfaceWithoutAuthor() {
    List<String> description = getInterfaceDescription("generated/comments/verboseInterface.utam");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@author UTAM"));
    assertThat(description.get(2), containsString("@version"));
  }
}
