package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * imperative extension compose statement mapping
 *
 * @author olivier.martin
 * @since 234
 */
public class UtamUtilityMethodAction {
    final String externalClassPath;
    final String methodName;
    UtamArgument[] args;

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

    public String getMethodName() {
        return methodName;
    }

    public String getExternalClassPath() {
        return externalClassPath;
    }
}
