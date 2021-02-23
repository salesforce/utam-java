package utam.compiler.helpers;

import java.util.function.Supplier;
import org.testng.annotations.Test;
import utam.core.selenium.element.Selector;

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
    assertThat(provider.getFullName(), is(equalTo(String.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(String.class.getPackageName())));
    assertThat(provider.getClassType(), is(equalTo(String.class)));
  }

  /** Test that INTEGER member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testIntegerEnumMember() {
    PrimitiveType provider = PrimitiveType.NUMBER;
    assertThat(provider.getSimpleName(), is(equalTo(Integer.class.getSimpleName())));
    assertThat(provider.getFullName(), is(equalTo(Integer.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Integer.class.getPackageName())));
    assertThat(provider.getClassType(), is(equalTo(Integer.class)));
  }

  /** Test that BOOLEAN member of the PrimitiveType enumerated type returns the expected values */
  @Test
  public void testBooleanEnumMember() {
    PrimitiveType provider = PrimitiveType.BOOLEAN;
    assertThat(provider.getSimpleName(), is(equalTo("Boolean")));
    assertThat(provider.getFullName(), is(equalTo(Boolean.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Boolean.class.getPackageName())));
    assertThat(provider.getClassType(), is(equalTo(Boolean.class)));
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
    assertThat(PrimitiveType.fromString("predicate"), is(equalTo(PrimitiveType.PREDICATE)));
    assertThat(PrimitiveType.fromString("locator"), is(equalTo(PrimitiveType.LOCATOR)));
    assertThat(PrimitiveType.fromString(null), is(nullValue()));
    assertThat(PrimitiveType.fromString("invalid"), is(nullValue()));
  }

  @Test
  public void testIsPrimitive() {
    assertThat(PrimitiveType.isPrimitiveType("string"), is(true));
    assertThat(PrimitiveType.isPrimitiveType("number75"), is(false));
  }

  @Test
  public void testPredicateEnumMember() {
    PrimitiveType provider = PrimitiveType.PREDICATE;
    assertThat(provider.getSimpleName(), is(equalTo("Supplier<T>")));
    assertThat(provider.getFullName(), is(equalTo(Supplier.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Supplier.class.getPackageName())));
    assertThat(provider.getClassType(), is(equalTo(Supplier.class)));
  }

  @Test
  public void testSelectorEnumMember() {
    PrimitiveType provider = PrimitiveType.LOCATOR;
    assertThat(provider.getSimpleName(), is(equalTo("Selector")));
    assertThat(provider.getFullName(), is(equalTo(Selector.class.getName())));
    assertThat(provider.getPackageName(), is(equalTo(Selector.class.getPackageName())));
    assertThat(provider.getClassType(), is(equalTo(Selector.class)));
  }
}
