/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.*;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.base.UtamBase;
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

  public static final TypeProvider LIST_IMPORT = new TypeUtilities.FromClass(List.class);
  public static final TypeProvider COLLECTOR_IMPORT = new TypeUtilities.FromClass(Collectors.class);
  public static final TypeProvider PAGE_OBJECT = new TypeUtilities.FromClass(PageObject.class);
  public static final TypeProvider BASE_PAGE_OBJECT =
      new TypeUtilities.FromClass(BasePageObject.class);
  public static final TypeProvider ROOT_PAGE_OBJECT =
      new TypeUtilities.FromClass(RootPageObject.class);
  public static final TypeProvider VOID = new UnimportableType("void");
  public static final TypeProvider REFERENCE = new UnimportableType("reference");
  public static final TypeProvider BOUNDED_CLASS = new UnimportableType("Class<T>");
  static final TypeProvider GENERIC_TYPE = new UnimportableType("<T> T");
  static final TypeProvider CONTAINER_ELEMENT =
      new TypeUtilities.FromClass(ContainerElement.class);
  private static final String ERR_PARAMETERS_TYPES_MISMATCH =
      "expected %d parameters with type {%s}, provided were {%s}";
  public static final TypeProvider CONTAINER_RETURN_TYPE =
      new TypeUtilities.UnimportableType(String.format("<T extends %s> T", PAGE_OBJECT.getSimpleName()));
  public static final TypeProvider CONTAINER_LIST_RETURN_TYPE = new TypeUtilities.UnimportableType(
      String.format("<T extends %s> List<T>", PAGE_OBJECT.getSimpleName()));
  public static final TypeProvider SELECTOR = new FromClass(LocatorBy.class);
  public static final TypeProvider FUNCTION = new FromClass(Supplier.class) {
    @Override
    public String getSimpleName() {
      return "Supplier<T>";
    }
  };
  public static final TypeProvider ELEMENT_FIELD = new FromClass(ElementLocation.class);

  static String getUnmatchedParametersErr(
      List<TypeProvider> expectedTypes, List<MethodParameter> providedParameters) {
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

  public enum BasicElementInterface implements TypeProvider {
    actionable(Actionable.class),
    clickable(Clickable.class),
    editable(Editable.class),
    touchable(Touchable.class);

    private final Class type;

    BasicElementInterface(Class type) {
      this.type = type;
    }

    public static boolean isBasicType(String jsonString) {
      for (TypeUtilities.BasicElementInterface type : TypeUtilities.BasicElementInterface.values()) {
        if (type.name().equals(jsonString)) {
          return true;
        }
      }
      return false;
    }

    public static BasicElementInterface asBasicType(String jsonString) {
      if (jsonString == null) {
        return actionable;
      }
      for (TypeUtilities.BasicElementInterface type : TypeUtilities.BasicElementInterface.values()) {
        if (jsonString.equals(type.name())) {
          return type;
        }
      }
      return null;
    }

    public static boolean isBasicType(TypeProvider type) {
      if (type instanceof Element) {
        return true;
      }
      for (TypeUtilities.BasicElementInterface basicType : TypeUtilities.BasicElementInterface.values()) {
        if (basicType.isSameType(type)) {
          return true;
        }
      }
      return false;
    }

    public static BasicElementInterface[] getBasicElementTypes(TypeProvider type) {
      if (type instanceof Element) {
        return ((Element)type).basicInterfaces.toArray(BasicElementInterface[]::new);
      }

      if (type.isSameType(new TypeUtilities.FromClass(RootElement.class))) {
        return BasicElementInterface.values();
      }

      BasicElementInterface basicInterface = getBasicElementType(type);
      if (basicInterface != null) {
        return new BasicElementInterface[] { basicInterface };
      }

      return null;
    }

    public static TypeUtilities.BasicElementInterface getBasicElementType(TypeProvider type) {
      for (TypeUtilities.BasicElementInterface basicType : TypeUtilities.BasicElementInterface.values()) {
        if (basicType.isSameType(type)) {
          return basicType;
        }
      }
      return null;
    }

    public static String nameList() {
      return Arrays.stream(values())
          .map(basicInterface -> basicInterface.name()).collect(Collectors.joining(","));
    }

    @Override
    public String getFullName() {
      return type.getName();
    }

    @Override
    public String getSimpleName() {
      return type.getSimpleName();
    }

    @Override
    public String getPackageName() {
      return type.getPackageName();
    }

    @Override
    public Class getClassType() {
      return type;
    }
  }

  public static class Element implements TypeProvider {
    private final String name;
    private String containingType = "";
    private List<BasicElementInterface> basicInterfaces = new ArrayList<>();

    Element(String name, String[] interfaceTypes, String containingType) {
      this.name = name.substring(0, 1).toUpperCase() + name.substring(1) + "Element";
      this.containingType = containingType;
      for(String interfaceType : interfaceTypes) {
        if (BasicElementInterface.isBasicType(interfaceType)) {
          basicInterfaces.add(BasicElementInterface.asBasicType(interfaceType));
        }
      }
    }

    public static boolean isBasicType(String[] interfaceTypes) {
      if (interfaceTypes == null) {
        return true;
      }
      for (String interfaceType : interfaceTypes) {
        if (!BasicElementInterface.isBasicType(interfaceType)) {
          return false;
        }
      }
      return true;
    }

    public static Element asBasicType(String name, String[] interfaceTypes) {
      if (interfaceTypes == null || interfaceTypes.length == 0) {
        return new Element(name, new String[] { BasicElement.class.getSimpleName() }, "");
      }
      if (isBasicType(interfaceTypes)) {
        return new Element(name, interfaceTypes, "");
      }
      return null;
    }

    public Collection<TypeProvider> getBasicInterfaces() {
      if (basicInterfaces.size() == 0) {
        // If there are no basic interfaces declared, the only interface implemented by this
        // element is BasicElement.
        return new ArrayList<>(List.of(new TypeUtilities.FromClass(BasicElement.class)));
      }
      return new ArrayList<>(basicInterfaces);
    }

    @Override
    public String getFullName() {
      String separator = "".equals(containingType) ? "" : ".";
      return containingType + separator + name;
    }

    @Override
    public String getSimpleName() {
      return name;
    }

    @Override
    public String getPackageName() {
      return containingType;
    }

    @Override
    public Class getClassType() {
      return null;
    }
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

  public static final class ListOf implements TypeProvider {

    private final TypeProvider elementType;

    public ListOf(TypeProvider elementType) {
      this.elementType = elementType;
    }

    public TypeProvider getElementType() {
      return elementType;
    }

    @Override
    public String getFullName() {
      return List.class.getName();
    }

    @Override
    public String getSimpleName() {
      return String.format("List<%s>", elementType.getSimpleName());
    }

    @Override
    public String getPackageName() {
      return List.class.getPackage().getName();
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
    public Class getClassType() {
      return List.class;
    }
  }

  static class UnimportableType extends FromString {

    UnimportableType(String name) {
      super(name, "");
    }
  }
}
