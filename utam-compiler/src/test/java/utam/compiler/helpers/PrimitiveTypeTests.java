/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.testng.annotations.Test;

/**
 * Tests the default implementation and static members of the PrimitiveType enumerated type
 *
 * @author james.evans
 */
public class PrimitiveTypeTests {

  /** Test that STRING member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testStringEnumMember() {
    PrimitiveType provider = PrimitiveType.STRING;
    assertThat(provider.getSimpleName(), is(equalTo("String")));
    assertThat(provider.getFullName(), is(equalTo(String.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(String.class.getPackageName())));
  }

  /** Test that INTEGER member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testIntegerEnumMember() {
    PrimitiveType provider = PrimitiveType.NUMBER;
    assertThat(provider.getSimpleName(), is(equalTo(Integer.class.getSimpleName())));
    assertThat(provider.getFullName(), is(equalTo(Integer.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Integer.class.getPackageName())));
  }

  /** Test that BOOLEAN member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testBooleanEnumMember() {
    PrimitiveType provider = PrimitiveType.BOOLEAN;
    assertThat(provider.getSimpleName(), is(equalTo("Boolean")));
    assertThat(provider.getFullName(), is(equalTo(Boolean.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Boolean.class.getPackageName())));
  }

  /**
   * Test that the EMPTY_ARRAY member of the PrimitiveType enumerated type returns an empty array
   */
  @Test
  public void testEmptyArrayMember() {
    assertThat(PrimitiveType.EMPTY_ARRAY, is(equalTo(PrimitiveType.EMPTY_ARRAY)));
  }

  @Test
  public void testFromString() {
    assertThat(PrimitiveType.fromString("string"), is(equalTo(PrimitiveType.STRING)));
    assertThat(PrimitiveType.fromString("number"), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(PrimitiveType.fromString("boolean"), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(PrimitiveType.fromString(null), is(nullValue()));
    assertThat(PrimitiveType.fromString("invalid"), is(nullValue()));
  }

  @Test
  public void testIsPrimitive() {
    assertThat(PrimitiveType.isPrimitiveType("string"), is(true));
    assertThat(PrimitiveType.isPrimitiveType("boolean"), is(true));
    assertThat(PrimitiveType.isPrimitiveType("number"), is(true));
    assertThat(PrimitiveType.isPrimitiveType("selector"), is(false));
    assertThat(PrimitiveType.isPrimitiveType("invalid"), is(false));
  }
}
