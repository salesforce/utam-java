package utam.compiler.helpers;

import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.RootElementHelper.AccessSettings.DEFAULT_TYPE_PROTECTED;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TypeUtilities.FromClass;
import utam.compiler.representation.RootElementMethod.PrivateCustomType;
import utam.compiler.representation.RootElementMethod.ProtectedDefaultType;
import utam.compiler.representation.RootElementMethod.PublicCustomType;
import utam.compiler.representation.RootElementMethod.PublicDefaultType;
import utam.compiler.representation.UnionTypeImpl;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.BasicElement;
import utam.core.element.Locator;
import utam.core.framework.element.BasePageElement;

/**
 * helper class for root element declaration
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public final class RootElementHelper {

  private static final TypeProvider DEFAULT_ROOT_ELEMENT_TYPE = new FromClass(BasePageElement.class);
  private static final TypeProvider PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE = new FromClass(BasicElement.class);

  static final String ERR_UNSUPPORTED_ELEMENT_TYPE = "element '%s': type %s is not supported, "
      + "valid values are: " + BasicElementInterface.nameList();

  private final AccessSettings accessSettings;
  private final String[] rootElementType;

  public RootElementHelper(
      JsonNode typeNode,
      boolean isExposeRootElement) {
    this.rootElementType = processBasicTypeNode(typeNode, ROOT_ELEMENT_NAME);
    this.accessSettings = getAccessSettings(rootElementType, isExposeRootElement);
  }

  private static AccessSettings getAccessSettings(String[] types, boolean isExposeRootElement) {
    if (types.length == 0) {
      return isExposeRootElement ? AccessSettings.DEFAULT_TYPE_PUBLIC : DEFAULT_TYPE_PROTECTED;
    }
    return isExposeRootElement ? AccessSettings.CUSTOM_TYPE_PUBLIC
        : AccessSettings.CUSTOM_TYPE_PRIVATE;
  }

  public static String[] processBasicTypeNode(JsonNode typeNode, String elementName) {
    if (typeNode == null || typeNode.isNull()) {
      return new String[]{};
    }
    final String typeNodeValueError = String
        .format(ERR_UNSUPPORTED_ELEMENT_TYPE, elementName, typeNode.toPrettyString());
    if (typeNode.isTextual() && BasicElementInterface.isBasicType(typeNode.textValue())) {
      return new String[]{typeNode.textValue()};
    }
    if (typeNode.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode valueNode : typeNode) {
        if (!valueNode.isTextual()) {
          throw new UtamCompilationError(typeNodeValueError);
        }
        String valueStr = valueNode.textValue();
        if (!BasicElementInterface.isBasicType(valueStr)) {
          throw new UtamCompilationError(typeNodeValueError);
        }
        values.add(valueStr);
      }
      return values.toArray(String[]::new);
    }
    throw new UtamCompilationError(typeNodeValueError);
  }

  public ElementContext setRootElementMethod(TranslationContext context, Locator rootLocator) {
    TypeProvider interfaceType = context.getSelfType();
    final ElementContext rootElement;
    final PageObjectMethod rootElementMethod;
    if (accessSettings == AccessSettings.DEFAULT_TYPE_PUBLIC) {
      // if type is not set and element should be public - add public getter that returns UtamBase
      rootElement = new ElementContext.Root(interfaceType, rootLocator,
          PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE);
      rootElementMethod = new PublicDefaultType(PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE);
      context.setMethod(rootElementMethod);
    } else if (accessSettings == AccessSettings.CUSTOM_TYPE_PRIVATE) {
      // if type is set, but element is private - use protected method BasePageObject.getRootElement
      BasicElementUnionType elementType = BasicElementUnionType
          .asBasicType(ROOT_ELEMENT_NAME, rootElementType, false);
      rootElement = new ElementContext.Root(interfaceType, rootLocator, elementType);
      UnionType unionType = new UnionTypeImpl(false, elementType, elementType.getBasicInterfaces());
      rootElementMethod = new PrivateCustomType(unionType);
      context.setMethod(rootElementMethod);
      context.setUnionType(unionType, false);
    } else if (accessSettings == AccessSettings.CUSTOM_TYPE_PUBLIC) {
      // if type is set and element should be public - add new type and public getter
      BasicElementUnionType elementType =
          BasicElementUnionType.asBasicType(ROOT_ELEMENT_NAME,
              rootElementType, false);
      rootElement = new ElementContext.Root(interfaceType, rootLocator, elementType);
      UnionType unionType = new UnionTypeImpl(false, elementType, elementType.getBasicInterfaces());
      rootElementMethod = new PublicCustomType(unionType);
      context.setMethod(rootElementMethod);
      context.setUnionType(unionType, true);
    } else {
      // if type is not set and root element is not exposed - use protected method BasePageObject.getRootElement
      rootElement = new ElementContext.Root(interfaceType, rootLocator, DEFAULT_ROOT_ELEMENT_TYPE);
      rootElementMethod = new ProtectedDefaultType(DEFAULT_ROOT_ELEMENT_TYPE);
    }
    context.setElement(rootElement);
    rootElement.setElementMethod(rootElementMethod);
    return rootElement;
  }

  enum AccessSettings {
    DEFAULT_TYPE_PUBLIC,
    DEFAULT_TYPE_PROTECTED,
    CUSTOM_TYPE_PUBLIC,
    CUSTOM_TYPE_PRIVATE
  }
}
