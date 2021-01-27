package utam.compiler.helpers;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.Actionable;
import utam.core.selenium.element.Clickable;
import utam.core.selenium.element.Editable;
import utam.core.selenium.element.Touchable;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
  public static final TypeProvider CONTAINER_ELEMENT =
      new TypeUtilities.FromClass(ContainerElement.class);
  public static final String ERR_PARAMETERS_TYPES_MISMATCH =
      "expected %d parameters with type {%s}, provided were {%s}";

  public static boolean isElementType(String typeStr) {
    if(typeStr == null) {
      return true;
    }
    for (TypeUtilities.Element type : TypeUtilities.Element.values()) {
      if (typeStr.equals(type.name())) {
        return true;
      }
    }
    return false;
  }

  public static String getUnmatchedParametersErr(
      List<TypeProvider> expectedTypes, List<MethodParameter> providedParameters) {
    return String.format(
        ERR_PARAMETERS_TYPES_MISMATCH,
        expectedTypes.size(),
        expectedTypes.stream().map(TypeProvider::getSimpleName).collect(Collectors.joining(",")),
        providedParameters.stream()
            .map(parameter -> parameter.getType().getSimpleName())
            .collect(Collectors.joining(",")));
  }

  public static boolean isParametersTypesMatch(List<TypeProvider> expectedTypes, List<MethodParameter> actualTypes) {
    if (expectedTypes == null) {
      return true;
    }
    if (expectedTypes.size() != actualTypes.size()) {
      return false;
    }
    for (int i = 0; i < expectedTypes.size(); i++) {
      if (!expectedTypes.get(i).equals(actualTypes.get(i).getType())) {
        return false;
      }
    }
    return true;
  }

  public static TypeProvider getElementType(String string, TypeUtilities.Element defaultType) {
    Element res = null;
    if (string == null) {
      return defaultType == null ? null : defaultType.getType();
    }
    for (TypeUtilities.Element type : TypeUtilities.Element.values()) {
      if (string.equals(type.name())) {
        res = type;
        break;
      }
    }
    if (res != null) {
      return res.getType();
    }
    return null;
  }

  public enum Element {
    actionable(Actionable.class),
    clickable(Clickable.class),
    editable(Editable.class),
    touchable(Touchable.class);

    private final Class type;

    Element(Class type) {
      this.type = type;
    }

    public TypeProvider getType() {
      return new FromClass(type);
    }
  }

  public static final class FromClass implements TypeProvider {

    private final Class clazz;

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
    public boolean equals(Object type) {
      if (!(type instanceof TypeProvider)) {
        return false;
      }
      return getSimpleName().equals(((TypeProvider) type).getSimpleName())
          && getFullName().equals(((TypeProvider) type).getFullName());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSimpleName(), getFullName());
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
    public boolean equals(Object type) {
      if (!(type instanceof TypeProvider)) {
        return false;
      }
      return getSimpleName().equals(((TypeProvider) type).getSimpleName())
          && getFullName().equals(((TypeProvider) type).getFullName());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSimpleName(), getFullName());
    }
  }

  public static final class ListOf implements TypeProvider {

    private final TypeProvider elementType;

    public ListOf(TypeProvider elementType) {
      this.elementType = elementType;
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
    public boolean equals(Object type) {
      if (!(type instanceof TypeProvider)) {
        return false;
      }

      if (!(type instanceof ListOf)) {
        return false;
      }

      return elementType.equals(((ListOf) type).elementType);
    }

    @Override
    public int hashCode() {
      return Objects.hash(elementType);
    }
  }
}
