/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.translator.TranslationTypesConfigJava.isCustomType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.BasicElement;
import utam.core.element.ElementLocation;
import utam.core.element.FrameElement;
import utam.core.element.RootElement;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.LocatorBy;

/**
 * implementation of type provider based on existing class
 *
 * @author elizaveta.ivanova
 * @since 226
 */
@SuppressWarnings("rawtypes")
public final class TypeUtilities {

  public static final TypeProvider COLLECTOR_IMPORT = new TypeUtilities.FromClass(Collectors.class);
  public static final TypeProvider LIST_IMPORT = new TypeUtilities.FromClass(List.class);
  public static final TypeProvider PAGE_OBJECT = new TypeUtilities.FromClass(PageObject.class);
  public static final TypeProvider BASE_PAGE_OBJECT =
      new TypeUtilities.FromClass(BasePageObject.class);
  public static final TypeProvider ROOT_PAGE_OBJECT =
      new TypeUtilities.FromClass(RootPageObject.class);
  public static final TypeProvider VOID = new UnimportableType("void");
  public static final TypeProvider PARAMETER_REFERENCE = new UnimportableType("argumentReference");
  public static final TypeProvider ROOT_ELEMENT_TYPE = new FromClass(RootElement.class);
  public static final TypeProvider CONTAINER_ELEMENT =
      new TypeUtilities.FromClass(ContainerElement.class);
  public static final TypeProvider FRAME_ELEMENT = new TypeUtilities.FromClass(FrameElement.class);
  public static final TypeProvider SELECTOR = new FromClass(LocatorBy.class);
  public static final TypeProvider FUNCTION = new FromClass(Supplier.class) {
    @Override
    public String getSimpleName() {
      return "Supplier<T>";
    }
  };
  public static final TypeProvider ELEMENT_FIELD = new FromClass(ElementLocation.class);
  public static final TypeProvider BASIC_ELEMENT = new FromClass(BasicElement.class);
  public static final String FRAME_ELEMENT_TYPE_NAME = "frame";
  public static final String CONTAINER_ELEMENT_TYPE_NAME = "container";
  public static final TypeProvider PAGE_OBJECT_PARAMETER = new PageObjectClass(null);
  public static final TypeProvider BOUNDED_PAGE_OBJECT_PARAMETER = new PageObjectClass("T");
  public static final TypeProvider ROOT_PAGE_OBJECT_PARAMETER = new RootPageObjectClass(null);
  static final TypeProvider JAVA_OBJECT_TYPE = new UnimportableType("Object");
  private static final String ERR_PARAMETERS_TYPES_MISMATCH =
      "expected %d parameters with type {%s}, provided were {%s}";

  static String getUnmatchedParametersErr(List<TypeProvider> expectedTypes,
      List<MethodParameter> providedParameters) {
    return String.format(
        ERR_PARAMETERS_TYPES_MISMATCH,
        expectedTypes.size(),
        expectedTypes.stream().map(TypeProvider::getSimpleName).collect(Collectors.joining(",")),
        providedParameters.stream()
            .map(parameter -> parameter.getType().getSimpleName())
            .collect(Collectors.joining(",")));
  }

  static boolean isParametersTypesMatch(List<TypeProvider> expectedTypes,
      List<MethodParameter> actualTypes) {
    if (expectedTypes == null) {
      return true;
    }
    if (expectedTypes.size() != actualTypes.size()) {
      return false;
    }
    for (int i = 0; i < expectedTypes.size(); i++) {
      if (!expectedTypes.get(i).isSameType(actualTypes.get(i).getType())) {
        return false;
      }
    }
    return true;
  }

  public static TypeProvider getElementType(TypeProvider type) {
    if (type instanceof ListOf) {
      return type.getBoundTypes().get(0);
    }
    return type;
  }

  public static TypeProvider wrapAsList(TypeProvider originalType) {
    return originalType instanceof ListOf ? originalType : new ListOf(originalType);
  }

  public static class FromClass implements TypeProvider {

    final Class clazz;

    public FromClass(Class type) {
      this.clazz = type;
    }

    private static String getClazzNameForImport(Class type) {
      return type.getPackage().getName() + "." + type.getSimpleName();
    }

    @Override
    public String getFullName() {
      if (clazz.getEnclosingClass() != null) {
        return getClazzNameForImport(clazz.getEnclosingClass());
      }
      return getClazzNameForImport(clazz);
    }

    @Override
    public String getPackageName() {
      return clazz.getPackage().getName();
    }

    @Override
    public String getSimpleName() {
      return clazz.getSimpleName();
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSimpleName(), getFullName());
    }

    @Override
    public Class getClassType() {
      return clazz;
    }
  }

  public static class FromString implements TypeProvider {

    private final String name;
    private final String fullName;
    private final String packageName;

    public FromString(String name, String fullName) {
      this.name = name;
      this.fullName = fullName;
      this.packageName = setPackageName(fullName);
    }

    public FromString(String fullName) {
      this.name = getShortName(fullName);
      this.fullName = fullName;
      this.packageName = setPackageName(fullName);
    }

    private static String getShortName(String fullName) {
      if (!fullName.contains(".")) {
        return fullName;
      }
      int index = fullName.lastIndexOf(".") + 1;
      return fullName.substring(index);
    }

    private static String setPackageName(String fullName) {
      if (!fullName.contains(".")) {
        return "";
      }
      int index = fullName.lastIndexOf(".") + 1;
      return fullName.substring(0, index - 1);
    }

    @Override
    public String getFullName() {
      return fullName;
    }

    @Override
    public String getSimpleName() {
      return name;
    }

    @Override
    public String getPackageName() {
      return packageName;
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSimpleName(), getFullName());
    }

    @Override
    public Class getClassType() {
      return null;
    }
  }

  public static final class ListOf extends FromClass {

    private final TypeProvider elementType;

    private ListOf(TypeProvider elementType) {
      super(List.class);
      this.elementType = elementType;
    }

    @Override
    public String getSimpleName() {
      return String.format("List<%s>", elementType.getSimpleName());
    }

    @Override
    public boolean isSameType(TypeProvider anotherType) {
      if (!(anotherType instanceof ListOf)) {
        return false;
      }
      return this.getSimpleName().equals(anotherType.getSimpleName());
    }

    @Override
    public int hashCode() {
      return Objects.hash(elementType);
    }

    @Override
    public List<TypeProvider> getBoundTypes() {
      return Collections.singletonList(elementType);
    }

    @Override
    public List<TypeProvider> getImportableTypes() {
      List<TypeProvider> typesToImport = new ArrayList<>(getBoundTypes());
      typesToImport.add(LIST_IMPORT);
      return typesToImport;
    }
  }

  public static class UnimportableType extends FromString {

    public UnimportableType(String name) {
      super(name, "");
    }
  }

  /**
   * types like Class&lt;T&gt; or Class&lt;? extends Page Object&gt;
   */
  static class BoundedClass extends FromClass {

    private final TypeProvider boundType;
    private final String typeNameStr;

    BoundedClass(TypeProvider boundType, String boundStr) {
      super(Class.class);
      this.boundType = boundType;
      this.typeNameStr = boundStr != null ? String.format("Class<%s>", boundStr)
          : String.format("Class<? extends %s>", boundType.getSimpleName());
    }

    @Override
    public String getSimpleName() {
      return typeNameStr;
    }

    @Override
    public List<TypeProvider> getBoundTypes() {
      return Collections.singletonList(boundType);
    }

    @Override
    public List<TypeProvider> getImportableTypes() {
      return new ArrayList<>(getBoundTypes());
    }

    @Override
    public boolean isSameType(TypeProvider anotherType) {
      if (anotherType instanceof BoundedClass) {
        return this.boundType.isSameType(((BoundedClass) anotherType).boundType);
      }
      return false;
    }
  }

  static class PageObjectClass extends BoundedClass {

    PageObjectClass(String boundStr) {
      super(PAGE_OBJECT, boundStr);
    }

    PageObjectClass(TypeProvider boundType, String boundStr) {
      super(boundType, boundStr);
    }

    @Override
    public boolean isSameType(TypeProvider anotherType) {
      if (isCustomType(anotherType)) {
        return true;
      }
      return super.isSameType(anotherType);
    }
  }

  static class RootPageObjectClass extends PageObjectClass {

    RootPageObjectClass(String boundStr) {
      super(ROOT_PAGE_OBJECT, boundStr);
    }
  }
}
