/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.BasicElement;
import utam.core.element.FrameElement;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.BaseRootPageObject;
import utam.core.framework.base.ElementLocation;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.BasePageElement;
import utam.core.selenium.element.LocatorBy;

/**
 * implementation of type provider based on existing class
 *
 * @author elizaveta.ivanova
 * @since 226
 */
@SuppressWarnings("rawtypes")
public final class TypeUtilities {

  /** Used to import the java.util.stream.Collectors class for compose statements */
  public static final TypeProvider COLLECTOR_IMPORT = new TypeUtilities.FromClass(Collectors.class);

  /** A type provider for the PageObject class */
  public static final TypeProvider PAGE_OBJECT =
      new TypeUtilities.FromClass(PageObject.class) {
        @Override
        public boolean isSameType(TypeProvider anotherType) {
          // for literal page object it's still same type
          return isCustomType(anotherType) || super.isSameType(anotherType);
        }
      };

  /** A type provider for the BasePageObject class */
  public static final TypeProvider BASE_PAGE_OBJECT_CLASS =
      new TypeUtilities.FromClass(BasePageObject.class);

  /** A type provider for the RootPageObject class */
  public static final TypeProvider ROOT_PAGE_OBJECT =
      new TypeUtilities.FromClass(RootPageObject.class);

  /** A type provider for the BaseRootPageObject class */
  public static final TypeProvider BASE_ROOT_PAGE_OBJECT_CLASS =
      new TypeUtilities.FromClass(BaseRootPageObject.class);

  /** A type provider for void */
  public static final TypeProvider VOID = new UnimportableType("void");

  /** A type provider an argument reference */
  public static final TypeProvider PARAMETER_REFERENCE = new UnimportableType("argumentReference");

  /** A type provider for the ContainerElement class */
  public static final TypeProvider CONTAINER_ELEMENT =
      new TypeUtilities.FromClass(ContainerElement.class);

  /** A type provider for the LocatorBy class */
  public static final TypeProvider SELECTOR = new FromClass(LocatorBy.class);

  /** A type provider for the ElementLocation class */
  public static final TypeProvider ELEMENT_FIELD = new FromClass(ElementLocation.class);

  /** A type provider for the BasicElement class */
  public static final TypeProvider BASIC_ELEMENT = new FromClass(BasicElement.class);

  /** The type name for a frame element */
  public static final String FRAME_ELEMENT_TYPE_NAME = "frame";

  /** A type name for a container element */
  public static final String CONTAINER_ELEMENT_TYPE_NAME = "container";

  /** A type name for a page object - can be parameter type or return type */
  public static final String PAGE_OBJECT_TYPE_NAME = "pageObject";

  /** A type name for a root page object - can be parameter type or return type */
  public static final String ROOT_PAGE_OBJECT_TYPE_NAME = "rootPageObject";

  /** PageObject type for parameter. Used for non-literal arg with "type" : "pageObject" */
  public static final TypeProvider T_PAGE_OBJECT_TYPE_PARAMETER =
      new TBoundedPageObjectType(PAGE_OBJECT);

  /**
   * PageObject type for parameter. Used for non-literal arg with "type" : "pageObject" if it's not
   * returned from method
   */
  public static final TypeProvider W_PAGE_OBJECT_TYPE_PARAMETER =
      new WBoundedPageObjectType(PAGE_OBJECT);

  /**
   * RootPageObject type for parameter. Used for non-literal arg with "type" : "rootPageObject" if
   * it's not returned from method
   */
  public static final TypeProvider W_ROOT_PAGE_OBJECT_TYPE_PARAMETER =
      new WBoundedPageObjectType(ROOT_PAGE_OBJECT);

  /** Bounded Page Object. Used as return type in container method or returned from compose. */
  public static final TypeProvider PAGE_OBJECT_RETURN =
      new TBoundedPageObject(PAGE_OBJECT) {
        @Override
        public boolean isSameType(TypeProvider anotherType) {
          // for literal page object it's still same type
          return isCustomType(anotherType) || super.isSameType(anotherType);
        }
      };

  /** List of bounded Root Page Objects. Used as return type in container method. */
  public static final TypeProvider PAGE_OBJECT_RETURN_LIST = new TBoundedList(PAGE_OBJECT);

  /** A type provider for the BasicPageElement class */
  public static final TypeProvider BASIC_ELEMENT_IMPL_CLASS = new FromClass(BasePageElement.class);

  /** A type provider representing a frame element */
  public static final TypeProvider FRAME_ELEMENT = new FromClass(FrameElement.class);

  /** Bounded Root Page Object. Used as return type in container method or returned from compose. */
  static final TypeProvider ROOT_PAGE_OBJECT_RETURN = new TBoundedPageObject(ROOT_PAGE_OBJECT);

  /** List of bounded Root Page Objects. Used as return type in container method. */
  static final TypeProvider ROOT_PAGE_OBJECT_RETURN_LIST = new TBoundedList(ROOT_PAGE_OBJECT);

  /** Expected type for notNull matcher */
  public static final TypeProvider JAVA_OBJECT_TYPE = new UnimportableType("Object");

  /** RootPageObject type for parameter. Used for non-literal arg with "type" : "rootPageObject" */
  private static final TypeProvider T_ROOT_PAGE_OBJECT_TYPE_PARAMETER =
      new TBoundedPageObjectType(ROOT_PAGE_OBJECT);

  /** Type for Lists */
  public static final TypeProvider LIST_TYPE = new TypeUtilities.FromClass(List.class);

  static Class getClassFromFullName(TypeProvider type) {
    String fullName = type.getFullName();
    try {
      return Class.forName(fullName);
    } catch (ClassNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Wraps a type provider as a list
   *
   * @param originalType a type provider
   * @return a type provider describing a list of the original type
   */
  public static TypeProvider wrapAsList(TypeProvider originalType) {
    if (isListType(originalType)) {
      return originalType;
    }
    return new BoundedList(originalType);
  }

  /**
   * check if type is a list
   *
   * @param type type to check
   * @return boolean, true if list
   */
  public static boolean isListType(TypeProvider type) {
    return type instanceof BoundedList;
  }

  /**
   * check if type is a page object or list of page objects
   *
   * @param type type to check
   * @return true if type is a custom (page object) type
   */
  public static boolean isCustomType(TypeProvider type) {
    if (type == null) {
      return false;
    }
    if (type instanceof PageObjectType) {
      return true;
    }
    TypeProvider boundType = type.getBoundType();
    if (boundType != null) {
      return isCustomType(boundType);
    }
    return false;
  }

  /**
   * Parameter type depends on method return type: if return type is not page object itself, use
   * Class with wildcards, otherwise Class with T
   *
   * @param returnType method or statement return type
   * @return type to use
   */
  public static TypeProvider getPageObjectTypeParameter(ReturnType returnType) {
    if (returnType.isPageObject()) {
      return T_PAGE_OBJECT_TYPE_PARAMETER; // with T
    }
    return W_PAGE_OBJECT_TYPE_PARAMETER; // with wildcard
  }

  /**
   * Parameter type depends on method return type: if return type is not page object itself, use
   * Class with wildcards, otherwise Class with T
   *
   * @param returnType method or statement return type
   * @return type to use
   */
  public static TypeProvider getRootPageObjectTypeParameter(ReturnType returnType) {
    if (returnType.isPageObject()) {
      return T_ROOT_PAGE_OBJECT_TYPE_PARAMETER; // with T
    }
    return W_ROOT_PAGE_OBJECT_TYPE_PARAMETER; // with wildcard
  }

  /** Creates a type provider from a Class object */
  public static class FromClass implements TypeProvider {

    final Class clazz;

    /**
     * Initializes a new instance of the FromClass class
     *
     * @param type the Class from which to create the type provider
     */
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
  }

  /** Creates a type provider from a string */
  public static class FromString implements TypeProvider {

    private final String name;
    private final String fullName;
    private final String packageName;

    /**
     * Initializes a new instance of the FromString class
     *
     * @param fullName the full name of the type
     */
    public FromString(String fullName) {
      this.fullName = fullName;
      this.name =
          fullName.contains(".") ? fullName.substring(fullName.lastIndexOf(".") + 1) : fullName;
      this.packageName =
          fullName.contains(".") ? fullName.substring(0, fullName.lastIndexOf(".")) : "";
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
  }

  /** Creates a type provider for a virtual type that does not require an import statement */
  public static class UnimportableType extends FromString {

    /**
     * Initializes a new instance of the UmimportableType class
     *
     * @param name the name of the type
     */
    public UnimportableType(String name) {
      super(name);
    }

    @Override
    public String getFullName() {
      return ""; // returning empty string means nothing will be imported
    }
  }

  /** Type for a list of objects, accepts object type as a parameter */
  private static class BoundedList extends BoundedType {

    BoundedList(TypeProvider boundType) {
      super(List.class, boundType);
    }

    @Override
    public List<TypeProvider> getImportableTypes() {
      List<TypeProvider> typesToImport = new ArrayList<>(boundType.getImportableTypes());
      typesToImport.add(LIST_TYPE);
      return typesToImport;
    }
  }

  /** Type for List of page objects whose types were passed as parameters */
  static final class TBoundedList extends BoundedList {

    TBoundedList(TypeProvider boundType) {
      super(boundType);
    }

    @Override
    public String getReturnString() {
      return String.format("<T extends %s> List<T>", boundType.getSimpleName());
    }

    @Override
    public String getSimpleName() {
      return "List<T>";
    }
  }

  /** type to support "List of entities" or "Class of entities" */
  private abstract static class BoundedType extends FromString {

    final TypeProvider boundType;
    private final Class wrapperClass;

    /**
     * create instance of bounded type
     *
     * @param wrapperClass can be Class.class or List.class
     * @param boundType bounded type, can be page object type, custom type or primitive
     */
    BoundedType(Class wrapperClass, TypeProvider boundType) {
      super(wrapperClass.getName());
      this.wrapperClass = wrapperClass;
      this.boundType = boundType;
    }

    @Override
    public String getSimpleName() {
      return String.format("%s<%s>", wrapperClass.getSimpleName(), boundType.getSimpleName());
    }

    @Override
    public boolean isSameType(TypeProvider anotherType) {
      if (anotherType instanceof BoundedType) {
        boolean isBoundSame = this.boundType.isSameType(((BoundedType) anotherType).boundType);
        boolean isWrapperSame = this.wrapperClass.equals(((BoundedType) anotherType).wrapperClass);
        return isBoundSame && isWrapperSame;
      }
      return false;
    }

    @Override
    public TypeProvider getBoundType() {
      return boundType;
    }

    @Override
    public List<TypeProvider> getImportableTypes() {
      return boundType.getImportableTypes();
    }
  }

  /**
   * Wildcard bounded type like Class&lt;T&gt; or Class&lt;? extends Page Object&gt;. Bound type can
   * be PageObject or RootPageObject
   */
  static class WBoundedPageObjectType extends BoundedType {

    WBoundedPageObjectType(TypeProvider boundType) {
      super(Class.class, boundType);
    }

    @Override
    public String getSimpleName() {
      return String.format("Class<? extends %s>", boundType.getSimpleName());
    }
  }

  /**
   * Parameter T bounded type like Class&lt;T&gt;. At the moment bound type can be either PageObject
   * or RootPageObject
   */
  static class TBoundedPageObjectType extends BoundedType {

    TBoundedPageObjectType(TypeProvider boundType) {
      super(Class.class, boundType);
    }

    @Override
    public String getSimpleName() {
      return "Class<T>";
    }
  }

  /** type used as a return type from container or compose method */
  static class TBoundedPageObject extends BoundedType {

    TBoundedPageObject(TypeProvider boundType) {
      super(Class.class, boundType);
    }

    @Override
    public String getReturnString() {
      return String.format("<T extends %s> T", boundType.getSimpleName());
    }

    @Override
    public String getSimpleName() {
      return "T";
    }
  }

  /**
   * This class is separated from FromString so that method "isCustomType" could distinguish page
   * object from other custom types like utilities
   */
  public static class PageObjectType extends FromString {

    public PageObjectType(String fullName) {
      super(fullName);
    }
  }

  /**
   * Supports custom types name collisions: compiler will use full class name and not import it, for
   * example: my.page.Object
   */
  static class PageObjectWithNamesCollisionType extends PageObjectType {

    private final TypeProvider originalType;

    PageObjectWithNamesCollisionType(TypeProvider originalType) {
      super(originalType.getFullName());
      this.originalType = originalType;
    }

    @Override
    public String getFullName() {
      return originalType.getFullName();
    }

    @Override
    public String getSimpleName() {
      return originalType.getFullName();
    }

    @Override
    public String getPackageName() {
      return originalType.getPackageName();
    }

    @Override
    public boolean isSameType(TypeProvider anotherType) {
      return anotherType.getFullName().equals(this.getFullName());
    }

    @Override
    public String getFalsyValue() {
      return originalType.getFalsyValue();
    }

    @Override
    public List<TypeProvider> getImportableTypes() {
      return new ArrayList<>();
    }
  }
}
