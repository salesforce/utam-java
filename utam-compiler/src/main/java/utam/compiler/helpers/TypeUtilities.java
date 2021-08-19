/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.JsonNode;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;
import utam.core.element.BasicElement;
import utam.core.element.Clickable;
import utam.core.element.Editable;
import utam.core.element.ElementLocation;
import utam.core.element.RootElement;
import utam.core.element.Touchable;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
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
  public static final TypeProvider ROOT_ELEMENT_TYPE = new FromClass(RootElement.class);
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

  static final String CONTAINER_ELEMENT_TYPE = "container";
  public static final String ERR_TYPE_INVALID_VALUE_TYPE =
      "%s '%s': type must be %s, a Page Object type reference, or an array of basic element interfaces";
  public static final String ERR_TYPE_PROPERTY_INVALID_STRING_VALUE =
      "element '%s': invalid string value '%s'; type property string values must be either 'container' or a Page Object type reference.";
  public static final String ERR_RETURNS_PROPERTY_INVALID_STRING_VALUE =
      "method '%s': invalid string value '%s'; returns property string values must be either a primitive type or a Page Object type reference.";
  public static final String ERR_TYPE_INVALID_ARRAY_TYPES =
      "%s '%s': type array must contain only string values";
  public static final String ERR_TYPE_INVALID_ARRAY_VALUES =
      "%s '%s': type array contains invalid values; valid values are %s";

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

  public static String[] processTypeNode(
      String name, PropertyType propertyType, JsonNode typeNode) {
    if (typeNode == null || typeNode.isNull()) {
      return new String[]{};
    }
    if (typeNode.isTextual()) {
      String value = typeNode.textValue();
      if (propertyType == PropertyType.TYPE &&
          !CONTAINER_ELEMENT_TYPE.equals(value) &&
          !TranslationTypesConfigJava.isPageObjectType(value)) {
        throw new UtamError(String.format(ERR_TYPE_PROPERTY_INVALID_STRING_VALUE, name, value));
      }
      if (propertyType == PropertyType.RETURNS &&
          !PrimitiveType.isPrimitiveType(value) &&
          !TranslationTypesConfigJava.isPageObjectType(value)) {
        throw new UtamError(String.format(ERR_RETURNS_PROPERTY_INVALID_STRING_VALUE, name, value));
      }
      return new String[] {value};
    }
    String entityType = propertyType == PropertyType.TYPE ? "element" : "method";
    if (typeNode.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode valueNode : typeNode) {
        if (!valueNode.isTextual()) {
          throw new UtamError(String.format(ERR_TYPE_INVALID_ARRAY_TYPES, entityType, name));
        }
        values.add(valueNode.textValue());
      }
      String[] res = values.toArray(String[]::new);
      if (!TypeUtilities.Element.isBasicType(res)) {
        throw new UtamError(String.format(ERR_TYPE_INVALID_ARRAY_VALUES, entityType, name,
            TypeUtilities.BasicElementInterface.nameList()));
      }
      return res;
    }
    throw new UtamError(String.format(ERR_TYPE_INVALID_VALUE_TYPE,
        entityType,
        name,
        propertyType == PropertyType.TYPE ? "'" + CONTAINER_ELEMENT_TYPE + "'" : "a primitive data type"));
  }

  public enum PropertyType {
    TYPE,
    RETURNS
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

    static BasicElementInterface asBasicType(String jsonString) {
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

      if (type.isSameType(ROOT_ELEMENT_TYPE)) {
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
          .map(Enum::name).collect(Collectors.joining(","));
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
    private final List<BasicElementInterface> basicInterfaces = new ArrayList<>();

    Element(String name, String[] interfaceTypes, boolean requiresPrefix) {
      // If the basic element being defined is in an implementation-only Page Object,
      // that is, one that has an "implements" property defined, and the element is
      // marked as public, then there must be a method defined in the interface-only
      // Page Object that matches the name of the exposed element. Assuming the method
      // is named "getFoo," that method will have a generated return type of interface
      // "GetFooElement", and the implementation Page Object must have an implementation
      // for the element that will match that interface name. Note carefully that this
      // is distinct from the case where the element is normally exposed in a Page Object
      // using the "public" property in the JSON, in which case, the "Get" prefix is
      // omitted.
      final String prefix = requiresPrefix ? "Get" : "";
      this.name = prefix + name.substring(0, 1).toUpperCase() + name.substring(1) + "Element";
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

    public static Element asBasicType(
        String name, String[] interfaceTypes, boolean isPublicImplOnly) {
      if (interfaceTypes == null || interfaceTypes.length == 0) {
        return new Element(
            name, new String[] { BasicElement.class.getSimpleName() }, isPublicImplOnly);
      }
      if (isBasicType(interfaceTypes)) {
        return new Element(name, interfaceTypes, isPublicImplOnly);
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
      return name;
    }

    @Override
    public String getSimpleName() {
      return name;
    }

    @Override
    public String getPackageName() {
      return "";
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
