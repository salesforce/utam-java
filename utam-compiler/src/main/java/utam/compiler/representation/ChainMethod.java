/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.framework.consumer.UtamError;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;
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
  private final List<TypeProvider> imports;
  private final List<TypeProvider> classImports;
  private final String name;
  private final TypeProvider returns;
  private final String comments;

  public ChainMethod(String methodName, List<Link> chain, String comments) {
    this.name = methodName;
    this.comments = comments;
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
    imports = new ArrayList<>();
    imports.add(chain.get(chain.size() - 1).returns);
    if (returnCardinality.isList()) {
      imports.add(LIST_IMPORT);
    }
    classImports = new ArrayList<>(imports);
    if (returnCardinality == Cardinality.COLLECTED_LIST) {
      classImports.add(COLLECTOR_IMPORT);
    }
  }

  // used in tests
  ChainMethod(String methodName, List<Link> chain) {
    this(methodName, chain, EMPTY_COMMENTS);
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, EMPTY_PARAMETERS, returns, imports, comments);
  }

  @Override
  public final List<String> getCodeLines() {
    return Stream.of(codeLine).collect(Collectors.toList());
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
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
      this.methodCallString = "this." + firstElement.getElementMethod().getDeclaration().getName() + "()";
      this.apply = firstElement.isList() ? ApplyLink.map : ApplyLink.get;
    }

    public Link(TypeProvider returns, String elementName, boolean isList) {
      this.returns = returns;
      this.methodCallString = getElementGetterMethodName(elementName, true) + "()";
      this.apply = isList ? ApplyLink.map : ApplyLink.get;
    }

    TypeProvider getReturn(Cardinality cardinality) {
      if (cardinality.isList()) {
        return new TypeUtilities.ListOf(returns);
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
