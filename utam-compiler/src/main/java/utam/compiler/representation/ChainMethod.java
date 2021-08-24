/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.ElementContext;
import utam.core.framework.consumer.UtamError;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

/**
 * representation of chained method
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class ChainMethod implements PageObjectMethod {

  static final String ERR_CHAIN_LINK_ARGS_NOT_SUPPORTED =
      "Element '%s' cannot be used in chain - selector arguments are not supported";
  private final String codeLine;
  private final List<TypeProvider> addedClassImports = new ArrayList<>();
  private final String name;
  private final TypeProvider returns;

  public ChainMethod(String methodName, List<Link> chain) {
    this.name = methodName;
    StringBuilder builder = new StringBuilder();
    Cardinality returnCardinality = null;
    for (Link link : chain) {
      returnCardinality = link.setCodeString(builder, returnCardinality);
    }
    if (returnCardinality == Cardinality.COLLECTED_LIST) {
      builder.append(".collect(Collectors.toList())");
    }
    this.codeLine = builder.toString();
    this.returns = chain.get(chain.size() - 1).getReturn(returnCardinality);
    if (returnCardinality == Cardinality.COLLECTED_LIST) {
      addedClassImports.add(COLLECTOR_IMPORT);
    }
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, EMPTY_PARAMETERS, returns);
  }

  @Override
  public final List<String> getCodeLines() {
    return Stream.of(codeLine).collect(Collectors.toList());
  }

  @Override
  public List<TypeProvider> getClassImports() {
    List<TypeProvider> imports = getDeclaration().getImports();
    imports.addAll(addedClassImports); //collector import
    return imports;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

  public enum ApplyLink {
    get,
    map
  }

  enum Cardinality {
    ONE,
    LIST,
    COLLECTED_LIST;

    boolean isList() {
      return this != ONE;
    }
  }

  public static class Link {

    private final TypeProvider returns;
    private final String methodCallString;
    private final ApplyLink apply;

    public Link(ElementContext firstElement) {
      this.returns = firstElement.getType();
      String elementName = firstElement.getName();
      if (!firstElement.getParameters().isEmpty()) {
        throw new UtamError(String.format(ERR_CHAIN_LINK_ARGS_NOT_SUPPORTED, elementName));
      }
      this.methodCallString = String.format("this.%s()", firstElement.getElementGetterName());
      this.apply = firstElement.isList() ? ApplyLink.map : ApplyLink.get;
    }

    public Link(TypeProvider returns, String elementName, boolean isList) {
      this.returns = returns;
      this.methodCallString = getElementGetterMethodName(elementName, true) + "()";
      this.apply = isList ? ApplyLink.map : ApplyLink.get;
    }

    TypeProvider getReturn(Cardinality cardinality) {
      if (cardinality.isList()) {
        return new ListOf(returns);
      }
      return returns;
    }

    /**
     * adds code
     *
     * @param builder string collector
     * @param previous previous link type
     * @return type the link
     */
    Cardinality setCodeString(StringBuilder builder, Cardinality previous) {
      if (previous == null) {
        builder.append(methodCallString);
        return apply == ApplyLink.get ? Cardinality.ONE : Cardinality.LIST;
      }
      if (previous.isList()) {
        if (apply == ApplyLink.get) {
          builder.append(String.format(".stream().map(element -> element.%s)", methodCallString));
        } else {
          builder.append(
              String.format(".stream().flatMap(element -> element.%s.stream())", methodCallString));
        }
        return Cardinality.COLLECTED_LIST;
      }
      builder.append(String.format(".%s", methodCallString));
      return apply == ApplyLink.get ? Cardinality.ONE : Cardinality.LIST;
    }
  }
}
