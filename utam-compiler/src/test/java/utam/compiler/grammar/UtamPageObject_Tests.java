/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.element.RootElement;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamPageObject.*;
import static utam.compiler.grammar.UtamProfile_Tests.PROFILE_KEY;
import static utam.compiler.grammar.UtamProfile_Tests.PROFILE_VALUE;
import static utam.compiler.grammar.UtamSelector_Tests.SELECTOR_STRING;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Page Object Root tests
 *
 * @author james.evans
 */
public class UtamPageObject_Tests {

  private static final UtamProfile[] MOCK_PROFILES =
      new UtamProfile[] {new UtamProfile(PROFILE_KEY, PROFILE_VALUE)};

  /** The getBaseType method should return the proper value with the root property set to true */
  @Test
  public void testGetBaseTypeWithRoot() {
    UtamPageObject pageObject = new UtamPageObject(true, new UtamSelector("css"));
    assertThat(pageObject.getBaseType(), is(equalTo(ROOT_PAGE_OBJECT)));
  }

  /** The getBaseType method should return the proper value with the root property set to false */
  @Test
  public void testGetBaseTypeWithNonRoot() {
    UtamPageObject pageObject = new UtamPageObject(false, null);
    assertThat(pageObject.getBaseType(), is(equalTo(PAGE_OBJECT)));
  }

  /** The getAnnotations method should return the proper value with a selector */
  @Test
  public void testGetAnnotationsWithSelector() {
    UtamPageObject pageObject = new UtamPageObject(true, UtamSelector_Tests.getUtamCssSelector());
    List<String> annotations =
        pageObject.getAnnotations().stream()
            .filter((annotation) -> !annotation.getAnnotationText().isEmpty())
            .map(AnnotationProvider::getAnnotationText)
            .collect(Collectors.toList());
    assertThat(
        annotations, contains(String.format("@PageMarker.Find(css = \"%s\")", SELECTOR_STRING)));
  }

  /** The getAnnotations method should return the proper value with a shadow property */
  @Test
  public void testGetAnnotationsWithShadow() {
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.shadow = new UtamShadowElement(new UtamElement[] {});
    List<String> annotations =
        pageObject.getAnnotations().stream()
            .filter((annotation) -> !annotation.getAnnotationText().isEmpty())
            .map(AnnotationProvider::getAnnotationText)
            .collect(Collectors.toList());
    assertThat(annotations, contains("@PageMarker.isShadowHost"));
  }

  /** The getAnnotations method should return the proper value with a platform property */
  @Test
  public void testGetAnnotationsWithPlatform() {
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.platform = "web";
    List<String> annotations =
        pageObject.getAnnotations().stream()
            .filter((annotation) -> !annotation.getAnnotationText().isEmpty())
            .map(AnnotationProvider::getAnnotationText)
            .collect(Collectors.toList());
    assertThat(annotations, contains("@PageMarker.Switch(PlatformType.WEB)"));
  }

  /** The getAnnotations method should return the proper value with a platform property */
  @Test
  public void testGetAnnotationsWithNoAnnotations() {
    UtamPageObject pageObject = new UtamPageObject();
    List<String> annotations =
        pageObject.getAnnotations().stream()
            .filter((annotation) -> !annotation.getAnnotationText().isEmpty())
            .map(AnnotationProvider::getAnnotationText)
            .collect(Collectors.toList());
    assertThat(annotations, is(empty()));
  }

  /**
   * The setPublicMethods method should set the proper methods on the TranslatorContext with no
   * methods specified
   */
  @Test
  public void testSetPublicMethodsWithNullMethodList() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.compile(context);
    assertThat(context.getMethods(), is(empty()));
  }

  /**
   * The setPublicMethods method should set the proper methods on the TranslatorContext with an
   * empty method list
   */
  @Test
  public void testSetPublicMethodsWithEmptyMethodList() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.methods = new UtamMethod[] {};
    pageObject.compile(context);
    assertThat(context.getMethods(), is(empty()));
  }

  /**
   * The setPublicMethods method should set the proper methods on the TranslatorContext with a valid
   * method list
   */
  @Test
  public void testSetPublicMethodsWithValidMethodList() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    UtamMethodChainLink[] chainLinks =
        new UtamMethodChainLink[] {
          new UtamMethodChainLink("first", false, TEST_URI),
          new UtamMethodChainLink("customName", false, "utam-test/pageObjects/test/SecondWrapper")
        };
    pageObject.methods = new UtamMethod[] {
        TestUtilities.UtamEntityCreator.createUtamMethod("testMethod", null, chainLinks)
    };
    pageObject.elements = new UtamElement[] {
        UtamEntityCreator.createUtamElement(
            "first", TEST_URI, new UtamSelector("css"))
    };
    pageObject.compile(context);
    assertThat(context.getMethods().size(), is(equalTo(2)));
    assertThat(
        context.getMethods().get(0).getDeclaration().getName(),
        is(equalTo(getElementPrivateMethod("first"))));
    assertThat(context.getMethods().get(1).getDeclaration().getName(), is(equalTo("testMethod")));
  }

  /**
   * The setPublicMethods method should set the proper methods on the TranslatorContext with the
   * abstract property true and an empty method list
   */
  @Test
  public void testSetPublicMethodsWithAbstractAndEmptyMethodList() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.methods = new UtamMethod[] {};
    pageObject.compile(context);
    assertThat(context.getMethods(), hasSize(0));
  }

  /**
   * The setPublicMethods method should set the proper methods on the TranslatorContext with the
   * abstract property true and a valid method list
   */
  @Test
  public void testSetPublicMethodsWithAbstractAndValidMethodList() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.isAbstract = true;
    pageObject.methods = new UtamMethod[] {
        TestUtilities.UtamEntityCreator.createUtamMethod("testMethod", (String)null, (UtamArgument[]) null)
    };
    MethodInfo methodInfo = new MethodInfo("testMethod", "void");

    pageObject.compile(context);
    PageObjectValidationTestHelper.validateMethods(
        "setPublicMethods",
        new ArrayList<>(context.getMethods()),
        Collections.singletonList(methodInfo));
  }

  /** The getProfiles method should return an empty profile array with null profile */
  @Test
  public void testGetProfilesWithNullProfile() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    assertThat(pageObject.getProfiles(context), is(arrayWithSize(0)));
  }

  @Test
  public void testAbstractPageObjectWithProfile() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.profiles = MOCK_PROFILES;
    utamPageObject.isAbstract = true;
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), is(equalTo(ERR_ROOT_ABSTRACT)));
  }

  @Test
  public void testGetProfilesWithNullImplementedTypeThrows() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.profiles = MOCK_PROFILES;
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), containsString(ERR_ROOT_PROFILE_HAS_NO_INTERFACE));
  }

  /** The getProfiles method should return a matched profile */
  @Test
  public void testGetProfilesFromPageObject() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.profiles = MOCK_PROFILES;
    utamPageObject.implementsType = TEST_URI;
    Profile[] profiles = utamPageObject.getProfiles(UtamProfile_Tests.getContextWithProfile());
    assertThat(profiles.length, is(equalTo(1)));
    assertThat(profiles[0].getName(), is(equalTo(PROFILE_KEY)));
    assertThat(profiles[0].getValue(), is(equalTo(PROFILE_VALUE)));
  }

  @Test
  public void testRootComments() {
    String json = "{}";
    JsonDeserializer deserializer = TestUtilities.getJsonStringDeserializer(json);
    PageObjectDeclaration declaration = deserializer.getObject();
    assertThat(declaration.getImplementation().getComments(), is(emptyString()));
    assertThat(declaration.getInterface().getComments(), is(emptyString()));
  }

  @Test
  public void testGetNextScopeForAbstract() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.isAbstract = true;
    pageObject.compile(context);
  }

  @Test
  public void testRootWithSelector() {
    TranslationContext context = getTestTranslationContext();
    UtamSelector rootSelector = UtamSelector_Tests.getUtamCssSelector();
    UtamPageObject pageObject = new UtamPageObject(true, rootSelector);
    pageObject.compile(context);
  }

  @Test
  public void testRootWithAccessIDSelector() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject(true, UtamSelector_Tests.getAccessIdSelector());
    pageObject.isRootPageObject = true;
    pageObject.platform = "native";
    pageObject.compile(context);
  }

  @Test
  public void testAbstractWithRootElementExposed() {
    MethodInfo info = new MethodInfo("getRoot", "RootElement");
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(RootElement.class.getName());
    info.setIsPublic(true);
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.isExposeRootElement = true;
    utamPageObject.rootElementType = new String[] { "clickable" };
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    PageObjectMethod rootElementMethod = context.getRootElement().getElementMethod();
    PageObjectValidationTestHelper.validateMethod(rootElementMethod, info);
  }

  @Test
  public void testAbstractWithNonNullShadowThrows() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.isAbstract = true;
    utamPageObject.shadow = new UtamShadowElement(new UtamElement[] {});
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), containsString(ERR_ROOT_ABSTRACT));
  }

  @Test
  public void testAbstractWithNonNullSelectorThrows() {
    UtamPageObject utamPageObject = new UtamPageObject(true, UtamSelector_Tests.getUtamCssSelector());
    utamPageObject.isAbstract = true;
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), containsString(ERR_ROOT_ABSTRACT));
  }

  @Test
  public void testAbstractWithNonNullElementsThrows() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.elements = new UtamElement[0];
    utamPageObject.isAbstract = true;
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), containsString(ERR_ROOT_ABSTRACT));
  }

  @Test
  public void testRootWithNullSelectorThrows() {
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.isRootPageObject = true;
    UtamError e = expectThrows(UtamError.class, utamPageObject::validate);
    assertThat(e.getMessage(), containsString(ERR_ROOT_MISSING_SELECTOR));
  }

  @Test
  public void testNonRootWithNonNullSelectorThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamPageObject(false, UtamSelector_Tests.getUtamCssSelector()));
    assertThat(e.getMessage(), containsString(ERR_ROOT_REDUNDANT_SELECTOR));
  }

  @Test
  public void testDefaultRootElementMethod() {
    UtamPageObject utamPageObject = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    assertThat(context.getRootElement().getElementMethod(), is(notNullValue()));
  }

  @Test
  public void testRootElementWithType() {
    MethodInfo info = new MethodInfo("getRoot", "RootElement");
    info.addCodeLine("this.getRootElement()");
    //info.addImportedTypes(Clickable.class.getName());
    info.setIsPublic(false);
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.rootElementType = new String[] { "clickable" };
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    PageObjectMethod rootElementMethod = context.getRootElement().getElementMethod();
    PageObjectValidationTestHelper.validateMethod(rootElementMethod, info);
  }

  @Test
  public void testRootElementWithActionableType() {
    MethodInfo info = new MethodInfo("getRoot", "RootElement");
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(RootElement.class.getName());
    info.setIsPublic(false);
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.rootElementType = new String[] { "actionable" };
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    PageObjectMethod rootElementMethod = context.getRootElement().getElementMethod();
    PageObjectValidationTestHelper.validateMethod(rootElementMethod, info);
  }

  @Test
  public void testPublicRootElementWithType() {
    MethodInfo info = new MethodInfo("getRoot", "RootElement");
    info.addCodeLine("this.getRootElement()");
    //info.addImportedTypes(Clickable.class.getName());
    UtamPageObject utamPageObject = new UtamPageObject();
    utamPageObject.rootElementType = new String[] { "clickable" };
    utamPageObject.isExposeRootElement = true;
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    PageObjectMethod rootElementMethod = context.getRootElement().getElementMethod();
    PageObjectValidationTestHelper.validateMethod(rootElementMethod, info);
    assertThat(rootElementMethod.isPublic(), is(true));
  }

  @Test
  public void testElementWithRootElementNameThrows() {
    UtamPageObject utamPageObject = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    utamPageObject.compile(context);
    ElementContext notRootWithRootName =
            new ElementContext.Basic(
                    "root", TypeUtilities.BasicElementInterface.clickable, getCssSelector("css"));
    expectThrows(UtamError.class, () -> context.setElement(notRootWithRootName));
  }
}
