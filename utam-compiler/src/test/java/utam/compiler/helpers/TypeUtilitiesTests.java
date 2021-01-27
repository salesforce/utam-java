package utam.compiler.helpers;

import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;
import org.hamcrest.MatcherAssert;
import org.testng.annotations.Test;
import selenium.element.Actionable;
import selenium.element.Clickable;
import selenium.element.ElementMarker;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static utam.compiler.helpers.TypeUtilities.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Provides tests for the TypeUtilities class
 *
 * @author james.evans
 */
@SuppressWarnings("EqualsBetweenInconvertibleTypes")
public class TypeUtilitiesTests {

  private static MethodParameter getMockParameter(String name, TypeProvider type) {
    MethodParameter mockParam = mock(MethodParameter.class);
    when(mockParam.getValue()).thenReturn(name);
    when(mockParam.getType()).thenReturn(type);
    return mockParam;
  }

  /** Static isElementType method should return proper values */
  @Test
  public void testIsElementType() {
    MatcherAssert.assertThat(TypeUtilities.isElementType("actionable"), is(equalTo(true)));
    assertThat(TypeUtilities.isElementType(null), is(equalTo(true)));
    assertThat(TypeUtilities.isElementType("invalid"), is(equalTo(false)));
  }

  @Test
  public void testGetTypeMethod() {
    assertThat(
        TypeUtilities.Element.actionable.getType().getFullName(),
        is(equalTo("selenium.element.Actionable")));
  }

  /** FromString returns a valid TypeProvider */
  @Test
  public void testFromString() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    assertThat(type.getFullName(), is(equalTo("test.FakeType")));
    assertThat(type.getPackageName(), is(equalTo("test")));
    assertThat(type.getSimpleName(), is(equalTo("FakeType")));
  }

  /** FromString returns a valid TypeProvider when used with no package */
  @Test
  public void testPageObject() {
    TypeProvider type = new TypeUtilities.FromString("test.FakeType");
    assertThat(type.getFullName(), is(equalTo("test.FakeType")));
    assertThat(type.getPackageName(), is(equalTo("test")));
    assertThat(type.getSimpleName(), is(equalTo("FakeType")));
  }

  /** FromString returns a valid TypeProvider when used with no package specifying only full name */
  @Test
  public void testFromStringWithNoPackageUsingFullName() {
    TypeProvider type = new TypeUtilities.FromString("FakeType");
    assertThat(type.getFullName(), is(equalTo("FakeType")));
    assertThat(type.getPackageName(), is(equalTo("")));
    assertThat(type.getSimpleName(), is(equalTo("FakeType")));
  }

  /** FromString returns a valid TypeProvider when used with no package */
  @Test
  public void testFromStringWithNoPackage() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test");
    assertThat(type.getFullName(), is(equalTo("test")));
    assertThat(type.getPackageName(), is(equalTo("")));
    assertThat(type.getSimpleName(), is(equalTo("FakeType")));
  }

  /**
   * The FromString.equals method should return true with TypeProvider having same simple name and
   * package name
   */
  @Test
  public void testFromStringEquals() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("FakeType", "test.FakeType");

    assertThat(type.equals(otherType), is(equalTo(true)));
  }

  /**
   * The FromString.equals method should return false with TypeProvider having same simple name and
   * different package name
   */
  @Test
  public void testFromStringEqualsWithDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("FakeType", "testOther.FakeType");

    assertThat(type.equals(otherType), is(equalTo(false)));
  }

  /**
   * The FromString.equals method should return false with TypeProvider having same package name and
   * different simple name
   */
  @Test
  public void testFromStringEqualsWithDifferentSimpleNames() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("OtherFakeType", "test.FakeType");

    assertThat(type.equals(otherType), is(equalTo(false)));
  }

  /** The FromString.equals method should return false with an object that is not a TypeProvider */
  @Test
  public void testFromStringEqualsWithDifferentObjectTypes() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");

    assertThat(type.equals("InvalidString"), is(equalTo(false)));
  }

  /** ListOf should create a valid TypeProvider */
  @Test
  public void testListOf() {
    TypeProvider baseType = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider type = new TypeUtilities.ListOf(baseType);

    assertThat(type.getFullName(), is(equalTo("java.util.List")));
    assertThat(type.getPackageName(), is(equalTo("java.util")));
    assertThat(type.getSimpleName(), is(equalTo("List<FakeType>")));
  }

  /**
   * The ListOf.equals method should return true with a list of TypeProvider created from the same
   * type
   */
  @Test
  public void testListOfEquals() {
    TypeProvider baseType = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider type = new TypeUtilities.ListOf(baseType);
    assertThat(type, is(equalTo(new TypeUtilities.ListOf(baseType))));
  }

  /** The ListOf.equals method should return false with a TypeProvider that is not a list */
  @Test
  public void testListOfEqualsWithNonListType() {
    TypeProvider baseType = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider type = new TypeUtilities.ListOf(baseType);
    assertThat(type, is(not(equalTo(baseType))));
  }

  /**
   * The ListOf.equals method should return false with list of TypeProvider created from different
   * class
   */
  @Test
  public void testListOfEqualsWithDifferentListTypes() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider otherType = new TypeUtilities.FromClass(Clickable.class);
    TypeProvider listType = new TypeUtilities.ListOf(type);

    assertThat(listType, is(not(equalTo(new TypeUtilities.ListOf(otherType)))));
  }

  /**
   * The ListOf.equals method should return false with list of TypeProvider created from different
   * class having the same simple name but different package
   */
  @Test
  public void testListOfEqualsWithListsOfDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromClass(selenium.element.Editable.class);
    TypeProvider otherType = new TypeUtilities.FromString("Editable", "selenium.mismatch.Editable");
    TypeProvider listType = new TypeUtilities.ListOf(type);

    assertThat(listType, is(not(equalTo(new TypeUtilities.ListOf(otherType)))));
  }

  /** The ListOf.equals method should return false with an object that is not a TypeProvider */
  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testListOfEqualsWithDifferentObjectTypes() {
    TypeProvider type = new TypeUtilities.ListOf(new TypeUtilities.FromClass(Actionable.class));

    assertThat(type.equals("InvalidString"), is(equalTo(false)));
  }

  /** FromClass should create a valid TypeProvider */
  @Test
  public void testFromClass() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);

    assertThat(type.getFullName(), is(equalTo("selenium.element.Actionable")));
    assertThat(type.getPackageName(), is(equalTo("selenium.element")));
    assertThat(type.getSimpleName(), is(equalTo("Actionable")));
  }

  /** FromClass should create a valid TypeProvider with a nested class */
  @Test
  public void testFromClassWithNestedClass() {
    TypeProvider type = new TypeUtilities.FromClass(ElementMarker.Find.class);

    assertThat(type.getFullName(), is(equalTo(AnnotationUtilsTests.ELEMENT_MARKER_ANNOTATION_CLASS)));
    assertThat(type.getPackageName(), is(equalTo("selenium.element")));
    assertThat(type.getSimpleName(), is(equalTo("Find")));
  }

  /**
   * The FromClass.equals method should return true with TypeProvider having same simple name and
   * package name
   */
  @Test
  public void testFromClassEquals() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider otherType = new TypeUtilities.FromClass(Actionable.class);

    assertThat(type.equals(otherType), is(equalTo(true)));
  }

  /**
   * The FromClass.equals method should return false with TypeProvider created from different class
   */
  @Test
  public void testFromClassEqualsWithDifferentTypes() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider otherType = new TypeUtilities.FromClass(Clickable.class);

    assertThat(type.equals(otherType), is(equalTo(false)));
  }

  /**
   * The FromClass.equals method should return false with TypeProvider created from different class
   * having the same simple name but different package
   */
  @Test
  public void testFromClassEqualsWithDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromClass(selenium.element.Editable.class);
    TypeProvider otherType = new TypeUtilities.FromString("Editable", "selenium.mismatch.Editable");

    assertThat(type, is(not(equalTo(otherType))));
  }

  /** The FromClass.equals method should return false with an object that is not a TypeProvider */
  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testFromClassEqualsWithDifferentObjectTypes() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);

    assertThat(type.equals("InvalidString"), is(equalTo(false)));
  }

  /** The getElementType static method should return proper values */
  @Test
  public void testGetElementType() {
    assertThat(
        Objects.requireNonNull(getElementType("actionable", null)).getSimpleName(),
        is(equalTo("Actionable")));
    assertThat(
        Objects.requireNonNull(getElementType("clickable", null)).getSimpleName(),
        is(equalTo("Clickable")));
    assertThat(
        Objects.requireNonNull(getElementType("editable", null)).getSimpleName(),
        is(equalTo("Editable")));
    assertThat(TypeUtilities.getElementType("unknown", null), is(nullValue()));
    assertThat(TypeUtilities.getElementType(null, null), is(nullValue()));
    assertThat(
        Objects.requireNonNull(getElementType(null, TypeUtilities.Element.editable)).getSimpleName(),
        is(equalTo("Editable")));
  }

  /** The isTypesMatch static method should return true for matching lists of types */
  @Test
  public void testIsTypesMatch() {
    List<TypeProvider> types =
        Arrays.asList(
            new TypeUtilities.FromClass(Actionable.class),
            new TypeUtilities.FromString("String", "String"));

    List<MethodParameter> params =
        Arrays.asList(
            getMockParameter("element", new FromClass(Actionable.class)),
            getMockParameter("text", new FromString("String", "String")));

    assertThat(TypeUtilities.isParametersTypesMatch(types, params), is(equalTo(true)));
  }

  /** The isTypesMatch static method should return true for a null type list */
  @Test
  public void testIsTypesMatchWithNullTypeList() {
    List<MethodParameter> params =
        Arrays.asList(
            getMockParameter("element", new FromClass(Actionable.class)),
            getMockParameter("text", new FromString("String", "String")));

    assertThat(TypeUtilities.isParametersTypesMatch(null, params), is(equalTo(true)));
  }

  /**
   * The isTypesMatch static method should return false for lists of types containing different size
   * lists
   */
  @Test
  public void testIsTypesMatchWithDifferentSizeLists() {
    List<TypeProvider> types =
        Arrays.asList(
            new TypeUtilities.FromClass(Actionable.class),
            new TypeUtilities.FromString("String", "String"));

    List<MethodParameter> params =
        Collections.singletonList(getMockParameter("text", new FromString("String", "String")));

    assertThat(TypeUtilities.isParametersTypesMatch(types, params), is(equalTo(false)));
  }

  /**
   * The isTypesMatch static method should return false for lists of types containing different size
   * lists
   */
  @Test
  public void testIsTypesMatchWithDifferentListContent() {
    List<TypeProvider> types =
        Arrays.asList(
            new TypeUtilities.FromClass(Actionable.class),
            new TypeUtilities.FromString("String", "String"));

    List<MethodParameter> params =
        Arrays.asList(
            getMockParameter("text", new FromString("String", "String")),
            getMockParameter("element", new FromClass(Actionable.class)));

    assertThat(TypeUtilities.isParametersTypesMatch(types, params), is(equalTo(false)));
  }

  /** The getUnmatchedParametersErr static method should return false for different type lists */
  @Test
  public void testGetUnmatchedParametersErr() {
    List<TypeProvider> types =
        Arrays.asList(
            new TypeUtilities.FromClass(Actionable.class),
            new TypeUtilities.FromString("String", "String"));

    List<MethodParameter> params =
        Collections.singletonList(getMockParameter("text", new FromString("String", "String")));

    assertThat(
        TypeUtilities.getUnmatchedParametersErr(types, params),
        containsString("expected 2 parameters"));
    assertThat(
        TypeUtilities.getUnmatchedParametersErr(types, params),
        containsString("provided were {String}"));
  }

  /** The BASE_PAGE_OBJECT constant should return the proper value */
  @Test
  public void testBasePageObjectConstant() {
    assertThat(BASE_PAGE_OBJECT.getFullName(), is(equalTo("framework.base.BasePageObject")));
  }

  /** The ROOT_PAGE_OBJECT constant should return the proper value */
  @Test
  public void testRootPageObjectConstant() {
    assertThat(ROOT_PAGE_OBJECT.getFullName(), is(equalTo("framework.base.RootPageObject")));
  }

  /** The PAGE_OBJECT constant should return the proper value */
  @Test
  public void testPageObjectConstant() {
    assertThat(PAGE_OBJECT.getFullName(), is(equalTo("framework.base.PageObject")));
  }
}
