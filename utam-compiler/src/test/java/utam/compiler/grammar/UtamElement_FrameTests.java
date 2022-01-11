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
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED;
import static utam.compiler.grammar.UtamElement.Type;

import java.util.List;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * test reads JSON file with declared frames
 *
 * @since 236
 */
public class UtamElement_FrameTests {

  private List<UtamElement> frames;

  @BeforeClass
  public void prepareData() {
    frames = DeserializerUtilities.getDeserializedObjects(UtamElement.class, "element/frameValidations");
  }

  @Test
  public void testFrameElementWithReturnAllSelectorThrows() {
    UtamElement utamElement = frames.get(0);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED, "returnAllThrows"))));
  }

  @Test
  public void testFrameElementWithNoSelectorThrows() {
    UtamElement utamElement = frames.get(1);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, "noSelectorThrows"))));
  }

  @Test
  public void testFrameElementWithNullableThrows() {
    UtamElement utamElement = frames.get(2);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("nullableThrows"))));
  }

  @Test
  public void testFrameElementWithElementsThrows() {
    UtamElement utamElement = frames.get(3);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("elementsThrows"))));
  }

  @Test
  public void testFrameElementWithShadowThrows() {
    UtamElement utamElement = frames.get(4);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("shadowThrows"))));
  }

  @Test
  public void testFrameElementWithFilterThrows() {
    UtamElement utamElement = frames.get(5);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("filterThrows"))));
  }
}
