/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.helpers.AnnotationUtils.EMPTY_ANNOTATION;

import org.testng.annotations.Test;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.core.declarative.representation.AnnotationProvider;

/**
 * Provides tests for the ElementField class
 *
 * @author james.evans
 */
public class ElementFieldTests {

  /** An ElementField object should be able to be created */
  @Test
  public void testElementFieldCreation() {
    FieldInfo info = new FieldInfo("fakeElementName");

    ElementField field = new ElementField("fakeElementName", EMPTY_ANNOTATION);
    info.validateField(field);
  }

  /** An ElementField object should be able to be created with annotations */
  @Test
  public void testElementFieldCreationWithAnnotations() {
    FieldInfo info = new FieldInfo("fakeElementName");
    info.addAnnotations("inShadow");
    AnnotationProvider annotation = mock(AnnotationProvider.class);
    when(annotation.getAnnotationText()).thenReturn("inShadow");

    ElementField field = new ElementField("fakeElementName", annotation);
    info.validateField(field);
  }
}
