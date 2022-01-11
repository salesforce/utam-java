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
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamElement.Type.CONTAINER;

import java.util.List;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * test validation of JSON files with container elements
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamElement_ContainerTests {

  private List<UtamElement> containers;

  @BeforeClass
  private void prepareData() {
    containers = DeserializerUtilities
        .getDeserializedObjects(UtamElement.class, "element/containerValidations");
  }

  @Test
  public void testNotAllowedFilter() {
    UtamElement element = containers.get(0);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(CONTAINER.getSupportedPropertiesErr("filterThrows"))));
  }

  @Test
  public void testNotAllowedShadow() {
    UtamElement element = containers.get(1);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("shadowThrows")));
  }

  @Test
  public void testNotAllowedNestedElements() {
    UtamElement element = containers.get(2);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("elementsThrows")));
  }

  @Test
  public void testNotAllowedLoad() {
    UtamElement element = containers.get(3);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("nullableThrows")));
  }
}
