/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.function.Supplier;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import org.testng.annotations.Test;
import utam.core.element.Actionable;
import utam.core.element.Clickable;
import utam.core.framework.base.ElementMarker;
import utam.core.element.Editable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import utam.core.selenium.element.LocatorBy;

import static utam.compiler.types.BasicElementInterface.clickable;
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

  @Test
  public void testActionableGetTypeMethod() {
    TypeProvider type = BasicElementInterface.actionable;
    assertThat(type.getClassType(), is(equalTo(Actionable.class)));
    assertThat(type.getFullName(), is(equalTo(type.getClassType().getName())));
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

    assertThat(type.isSameType(otherType), is(equalTo(true)));
  }

  /**
   * The FromString.equals method should return false with TypeProvider having same simple name and
   * different package name
   */
  @Test
  public void testFromStringEqualsWithDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("FakeType", "testOther.FakeType");

    assertThat(type.isSameType(otherType), is(equalTo(false)));
  }

  /**
   * The FromString.equals method should return false with TypeProvider having same package name and
   * different simple name
   */
  @Test
  public void testFromStringEqualsWithDifferentSimpleNames() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("OtherFakeType", "test.FakeType");

    assertThat(type.isSameType(otherType), is(equalTo(false)));
  }

  /** The FromString.equals method should return false with an object that is not a TypeProvider */
  @Test
  public void testFromStringEqualsWithDifferentObjectTypes() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");

    assertThat(type.isSameType(new TypeUtilities.FromClass(Class.class)), is(equalTo(false)));
  }

  @Test
  public void testFromStringHashCode() {
    TypeProvider type = new TypeUtilities.FromString("FakeType", "test.FakeType");
    assertThat(type.hashCode(), is(equalTo(Objects.hash(type.getSimpleName(), type.getFullName()))));
  }

  @Test
  public void testListOf() {
    TypeProvider baseType = new TypeUtilities.FromString("FakeType", "test.FakeType");
    TypeProvider type = wrapAsList(baseType);

    assertThat(type.getFullName(), is(equalTo("java.util.List")));
    assertThat(type.getPackageName(), is(equalTo("java.util")));
    assertThat(type.getSimpleName(), is(equalTo("List<FakeType>")));
    assertThat(type.getClassType(), is(equalTo(List.class)));
    assertThat(type.isSameType(wrapAsList(baseType)), is(true));
    assertThat(type.isSameType(baseType), is(false));
    assertThat(type.isSameType(wrapAsList(clickable)), is(false));
    assertThat(type.hashCode(), is(equalTo(Objects.hash(baseType))));
  }

  /** FromClass should create a valid TypeProvider */
  @Test
  public void testFromClass() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    assertThat(type.getFullName(), is(equalTo(Actionable.class.getName())));
    assertThat(type.getPackageName(), is(equalTo(Actionable.class.getPackageName())));
    assertThat(type.getSimpleName(), is(equalTo(Actionable.class.getSimpleName())));
  }

  /** FromClass should create a valid TypeProvider with a nested class */
  @Test
  public void testFromClassWithNestedClass() {
    TypeProvider type = new TypeUtilities.FromClass(ElementMarker.Find.class);

    assertThat(type.getFullName(), is(equalTo(ElementMarker.class.getName())));
    assertThat(type.getPackageName(), is(equalTo(ElementMarker.class.getPackageName())));
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

    assertThat(type.isSameType(otherType), is(equalTo(true)));
  }

  /**
   * The FromClass.equals method should return false with TypeProvider created from different class
   */
  @Test
  public void testFromClassEqualsWithDifferentTypes() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider otherType = new TypeUtilities.FromClass(Clickable.class);

    assertThat(type.isSameType(otherType), is(equalTo(false)));
  }

  /**
   * The FromClass.equals method should return false with TypeProvider created from different class
   * having the same simple name but different package
   */
  @Test
  public void testFromClassEqualsWithDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromClass(Editable.class);
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

  @Test
  public void testFromClassHashCode() {
    TypeProvider type = new TypeUtilities.FromClass(Actionable.class);
    assertThat(type.hashCode(), is(equalTo(Objects.hash(type.getSimpleName(), type.getFullName()))));
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

  @Test
  public void testVoidType() {
    TypeProvider typeProvider = VOID;
    assertThat(typeProvider.getFullName(), is(emptyString()));
    assertThat(typeProvider.getPackageName(), is(emptyString()));
    assertThat(typeProvider.getClassType(), is(nullValue()));
    assertThat(typeProvider.getSimpleName(), is(equalTo("void")));
    assertThat(typeProvider.isSameType(VOID), is(true));
  }

  @Test
  public void testFunctionType() {
    TypeProvider selectorType = FUNCTION;
    assertThat(selectorType.getSimpleName(), is(equalTo("Supplier<T>")));
    assertThat(selectorType.getFullName(), is(equalTo(Supplier.class.getName())));
    assertThat(selectorType.getPackageName(), is(equalTo(Supplier.class.getPackageName())));
    assertThat(selectorType.getClassType(), is(equalTo(Supplier.class)));
  }

  @Test
  public void testSelectorType() {
    TypeProvider predicateType = SELECTOR;
    assertThat(predicateType.getSimpleName(), is(equalTo(LocatorBy.class.getSimpleName())));
    assertThat(predicateType.getFullName(), is(equalTo(LocatorBy.class.getName())));
    assertThat(predicateType.getClassType(), is(equalTo(LocatorBy.class)));
  }

  @Test
  public void testBoundedClassWithWildcard() {
    BoundedClass type = new BoundedClass(PrimitiveType.STRING, null);
    assertThat(type.isSameType(type), is(true));
    assertThat(type.isSameType(PrimitiveType.STRING), is(false));
    assertThat(type.getBoundTypes().size(), is(1));
    assertThat(type.getBoundTypes().get(0).isSameType(PrimitiveType.STRING), is(true));
    assertThat(type.getSimpleName(), is(equalTo("Class<? extends String>")));
  }

  @Test
  public void testBoundedClassWithBound() {
    BoundedClass type = new BoundedClass(PrimitiveType.STRING, "T");
    assertThat(type.isSameType(new BoundedClass(PrimitiveType.NUMBER, "T")), is(false));
    assertThat(type.getBoundTypes().size(), is(1));
    assertThat(type.getBoundTypes().get(0).isSameType(PrimitiveType.STRING), is(true));
    assertThat(type.getSimpleName(), is(equalTo("Class<T>")));
  }
}
