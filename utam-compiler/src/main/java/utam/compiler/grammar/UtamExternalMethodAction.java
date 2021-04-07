package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class UtamExternalMethodAction {
    final String externalClassPath;
    final String methodName;
    UtamArgument[] args;

    @JsonCreator
    UtamExternalMethodAction(
            @JsonProperty(value = "type", required = true) String externalClassPath,
            @JsonProperty(value = "invoke", required = true) String methodName,
            @JsonProperty(value = "args") UtamArgument[] args
    ) {
        this.externalClassPath = externalClassPath;
        this.methodName = methodName;
        this.args = args;
    }
}
