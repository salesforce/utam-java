/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import utam.compiler.helpers.ParametersContext.AbstractParametersContext;
import utam.compiler.helpers.ParametersContext.MethodParametersContext;
import utam.core.declarative.representation.TypeProvider;

/**
 * Context of the method, keeps track of elements and return type. Class is stateful because of
 * elements usage tracker.
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  private final String methodName;
  // to keep track of element usages, each predicate has its own tracker
  private final List<ElementsUsageTracker> elementsUsageTrackers = new ArrayList<>();
  private final ReturnType methodReturnType;
  private final ParametersContext parametersContext;
  // index of the current elements usage tracker, starting from 0 and incrementing as we get into a
  // predicate
  private int elementsUsageContextIndex = 0;

  /**
   * Initializes a new instance of the MethodContext class
   *
   * @param methodName the name of the method
   * @param declaredReturnType the declared return type of the method
   * @param context translation context
   * @param isAbstract true for interface methods
   * @param hasMethodLevelArgs indicates if method has args declared by user
   */
  public MethodContext(
      String methodName,
      ReturnType declaredReturnType,
      TranslationContext context,
      boolean isAbstract,
      boolean hasMethodLevelArgs) {
    this.methodName = methodName;
    this.methodReturnType = declaredReturnType;
    this.elementsUsageTrackers.add(new ElementsUsageTracker());
    String parserContext = String.format("method \"%s\"", methodName);
    this.parametersContext =
        isAbstract
            ? new AbstractParametersContext(parserContext, context, hasMethodLevelArgs)
            : new MethodParametersContext(parserContext, context, hasMethodLevelArgs);
  }

  /**
   * check if method returns void
   *
   * @param returnType return type
   * @return true if method returns void
   */
  public static boolean isNullOrVoid(TypeProvider returnType) {
    return returnType == null || returnType.isSameType(VOID);
  }

  /**
   * get instance of parameters context
   *
   * @return object
   */
  public ParametersContext getParametersContext() {
    return parametersContext;
  }

  /**
   * get declared return type of the method
   *
   * @return declared return type
   */
  public ReturnType getDeclaredReturnType() {
    return methodReturnType;
  }

  /**
   * get method name
   *
   * @return string with method name
   */
  public String getName() {
    return methodName;
  }

  /**
   * get current elements usage tracker
   *
   * @return instance of the tracker
   */
  public final ElementsUsageTracker getElementUsageTracker() {
    return elementsUsageTrackers.get(elementsUsageContextIndex);
  }

  /** enters predicate context to track used elements separately */
  public final void enterPredicateContext() {
    this.elementsUsageContextIndex = elementsUsageTrackers.size();
    this.elementsUsageTrackers.add(new ElementsUsageTracker());
  }

  /** exits predicate context to track used elements separately */
  public final void exitPredicateContext() {
    this.elementsUsageContextIndex--;
  }

  /**
   * Tracker for elements used in compose methods. When same element is reused we want to store it
   * in a variable instead calling the same getter several times.
   *
   * @author elizaveta.ivanova
   * @since 236
   */
  public static class ElementsUsageTracker {

    private final Map<String, Entry<String, ElementContext>> elementVariables = new HashMap<>();

    /**
     * check if element has been used in previous statements
     *
     * @param name name of the element from compose statement
     * @return true if element was already used
     */
    public boolean isReusedElement(String name) {
      return elementVariables.containsKey(name);
    }

    /**
     * set element variable usage to track reusing of a same element
     *
     * @param elementVariable variable name for the element
     * @param context element
     */
    public void setElementUsage(String elementVariable, ElementContext context) {
      elementVariables.put(context.getName(), new SimpleEntry<>(elementVariable, context));
    }

    /**
     * for an element that has been used in previous statements, return variable name
     *
     * @param name name of the element from compose statement
     * @return variable name string
     */
    public String getReusedElementVariable(String name) {
      return elementVariables.get(name).getKey();
    }
  }
}
