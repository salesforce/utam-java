/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.TBoundedPageObjectType;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.WBoundedPageObjectType;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.types.BasicElementInterface.clickable;

import org.testng.annotations.Test;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;
import utam.core.element.Clickable;
import utam.core.element.Editable;
import utam.core.framework.base.ElementMarker;
import utam.core.selenium.element.LocatorBy;

/**
 * Provides tests for the TypeUtilities class
 *
 * @author james.evans
 */
public class TypeUtilitiesTests {

  private static final TypeProvider FAKE_TYPE_FROM_STRING =
      new TypeUtilities.FromString("test.FakeType");

  @Test
  public void testActionableGetTypeMethod() {
    TypeProvider type = BasicElementInterface.actionable;
    assertThat(type.getFullName(), is(equalTo(Actionable.class.getName())));
  }

  /** FromString returns a valid TypeProvider */
  @Test
  public void testFromString() {
    TypeProvider type = FAKE_TYPE_FROM_STRING;
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

  /**
   * The FromString.equals method should return true with TypeProvider having same simple name and
   * package name
   */
  @Test
  public void testFromStringEquals() {
    TypeProvider otherType = new TypeUtilities.FromString("test.FakeType");
    assertThat(FAKE_TYPE_FROM_STRING.isSameType(otherType), is(equalTo(true)));
  }

  /**
   * The FromString.equals method should return false with TypeProvider having same simple name and
   * different package name
   */
  @Test
  public void testFromStringEqualsWithDifferentPackages() {
    TypeProvider type = new TypeUtilities.FromString("test.FakeType");
    TypeProvider otherType = new TypeUtilities.FromString("testOther.FakeType");
    assertThat(type.isSameType(otherType), is(equalTo(false)));
  }

  /** The FromString.equals method should return false with an object that is not a TypeProvider */
  @Test
  public void testFromStringEqualsWithDifferentObjectTypes() {
    assertThat(
        FAKE_TYPE_FROM_STRING.isSameType(new TypeUtilities.FromClass(Class.class)),
        is(equalTo(false)));
  }

  @Test
  public void testListOf() {
    TypeProvider baseType = FAKE_TYPE_FROM_STRING;
    TypeProvider type = wrapAsList(baseType);
    assertThat(type.getFullName(), is(equalTo("java.util.List")));
    assertThat(type.getPackageName(), is(equalTo("java.util")));
    assertThat(type.getSimpleName(), is(equalTo("List<FakeType>")));
    assertThat(type.isSameType(wrapAsList(baseType)), is(true));
    assertThat(type.isSameType(baseType), is(false));
    assertThat(type.isSameType(wrapAsList(clickable)), is(false));
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
    TypeProvider otherType = new TypeUtilities.FromString("selenium.mismatch.Editable");
    assertThat(type, is(not(equalTo(otherType))));
  }

  @Test
  public void testVoidType() {
    TypeProvider typeProvider = VOID;
    assertThat(typeProvider.getFullName(), is(emptyString()));
    assertThat(typeProvider.getPackageName(), is(emptyString()));
    assertThat(typeProvider.getSimpleName(), is(equalTo("void")));
    assertThat(typeProvider.isSameType(VOID), is(true));
  }

  @Test
  public void testSelectorType() {
    TypeProvider predicateType = SELECTOR;
    assertThat(predicateType.getSimpleName(), is(equalTo(LocatorBy.class.getSimpleName())));
    assertThat(predicateType.getFullName(), is(equalTo(LocatorBy.class.getName())));
  }

  @Test
  public void testBoundedClass() {
    TypeProvider boundType = new TypeUtilities.FromString("full.Name");
    WBoundedPageObjectType type = new WBoundedPageObjectType(boundType);
    assertThat(type.getBoundType().isSameType(boundType), is(true));
    assertThat(type.getSimpleName(), is(equalTo("Class<? extends Name>")));
  }

  @Test
  public void testTBoundedClass() {
    TypeProvider boundType = new TypeUtilities.FromString("full.Name");
    TBoundedPageObjectType type = new TBoundedPageObjectType(boundType);
    assertThat(type.getBoundType().isSameType(boundType), is(true));
    assertThat(type.getSimpleName(), is(equalTo("Class<T>")));
  }
}
