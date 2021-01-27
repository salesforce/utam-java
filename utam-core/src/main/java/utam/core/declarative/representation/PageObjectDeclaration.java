package utam.core.declarative.representation;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public interface PageObjectDeclaration {

  boolean isInterfaceOnly();

  boolean isClassWithInterface();

  boolean isClassWithProfiles();

  PageObjectClass getImplementation();

  PageObjectInterface getInterface();
}
