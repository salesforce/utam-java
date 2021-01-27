package utam.compiler.helpers;

import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

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
    assertThat(provider.getFullName(), is(emptyString()));
    assertThat(provider.getPackageName(), is(emptyString()));
    assertThat(provider.getClassType(), is(equalTo(String.class)));
    assertThat(provider.equals(String.class), is(equalTo(true)));
  }

  /** Test that INTEGER member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testIntegerEnumMember() {
    PrimitiveType provider = PrimitiveType.NUMBER;
    assertThat(provider.getSimpleName(), is(equalTo("Integer")));
    assertThat(provider.getFullName(), is(emptyString()));
    assertThat(provider.getPackageName(), is(emptyString()));
    assertThat(provider.getClassType(), is(equalTo(Integer.class)));
    assertThat(provider.equals(Integer.class), is(equalTo(true)));   
    assertThat(provider.equals(int.class), is(equalTo(true)));
  }

  /** Test that BOOLEAN member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testBooleanEnumMember() {
    PrimitiveType provider = PrimitiveType.BOOLEAN;
    assertThat(provider.getSimpleName(), is(equalTo("Boolean")));
    assertThat(provider.getFullName(), is(emptyString()));
    assertThat(provider.getPackageName(), is(emptyString()));
    assertThat(provider.getClassType(), is(equalTo(Boolean.class)));
    assertThat(provider.equals(Boolean.class), is(equalTo(true)));
    assertThat(provider.equals(boolean.class), is(equalTo(true)));
  }

  /** Test that VOID member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testVoidEnumMember() {
    PrimitiveType provider = PrimitiveType.VOID;
    assertThat(provider.getSimpleName(), is(equalTo("void")));
    assertThat(provider.getFullName(), is(emptyString()));
    assertThat(provider.getPackageName(), is(emptyString()));
    assertThat(provider.getClassType(), is(equalTo(Void.class)));
    assertThat(provider.equals(Void.class), is(equalTo(true)));
  }

  /** Test that CLASS member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testClassEnumMember() {
    PrimitiveType provider = PrimitiveType.CLASS;
    assertThat(provider.getSimpleName(), is(equalTo("Class<T>")));
    assertThat(provider.getFullName(), is(emptyString()));
    assertThat(provider.getPackageName(), is(emptyString()));
    assertThat(provider.getClassType(), is(equalTo(Class.class)));
    assertThat(provider.equals(Class.class), is(equalTo(true)));
  }

  /**
   * Test that the EMPTY_ARRAY member of the PrimitiveType enumerated type returns an empty array
   */
  @Test
  public void testEmptyArrayMember() {
    assertThat(PrimitiveType.EMPTY_ARRAY, is(equalTo(PrimitiveType.EMPTY_ARRAY)));
  }
  
  /** Test that CLASS member of the PrimitiveType enumerated is not equal to a random class */
  @Test
  public void testInvalidClassTypeIsNotEqual() {
    PrimitiveType provider = PrimitiveType.CLASS;
    assertThat(provider.equals(Object.class), is(equalTo(false)));
  }
  
  @Test
  public void testFromString() {
    assertThat(PrimitiveType.fromString("string"), is(equalTo(PrimitiveType.STRING)));
    assertThat(PrimitiveType.fromString("number"), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(PrimitiveType.fromString("boolean"), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(PrimitiveType.fromString("void"), is(equalTo(PrimitiveType.VOID)));
    assertThat(PrimitiveType.fromString("class"), is(equalTo(PrimitiveType.CLASS)));
    assertThat(PrimitiveType.fromString("invalid"), is(nullValue()));
    assertThat(PrimitiveType.fromString("int") == null, is(equalTo(true)));
  }
}
