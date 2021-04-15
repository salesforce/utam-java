/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Imperative extension compose statement mapping.
 * This class creates an entity from a JSON object that represents a utility in a compose statement.
 * It's a 1 to 1 mapping so every utility declared in a compose method will create one UtamUtilityMethodAction object.
 *
 *
 * @author olivier.martin
 * @since 234
 */
public class UtamUtilityMethodAction {
    final String externalClassPath;
    final String methodName;
    UtamArgument[] args;

    /**
     * Creates a utility method action object by deserializing JSON utility compose statements.
     * This object holds the information necessary to generate the executable compose statement once compiled.
     *
     * @param externalClassPath class path of the imperative extension
     * @param methodName name of the static method declared in the class that needs to be run
     * @param args arguments that needs to be passed to the method specified in methodName
     */
    @JsonCreator
    UtamUtilityMethodAction(
            @JsonProperty(value = "type", required = true) String externalClassPath,
            @JsonProperty(value = "invoke", required = true) String methodName,
            @JsonProperty(value = "args") UtamArgument[] args
    ) {
        this.externalClassPath = externalClassPath;
        this.methodName = methodName;
        this.args = args;
    }

    /**
     * @return the utility static method name to run
     */
    public String getMethodName() {
        return methodName;
    }

    /**
     * @return the utitility class path
     */
    public String getExternalClassPath() {
        return externalClassPath;
    }
}
