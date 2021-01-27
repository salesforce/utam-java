package utam.compiler.representation;

import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.TypeUtilities;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.ContainerElementPageObject;
import utam.core.selenium.element.Selector;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ElementContext.ROOT_SCOPE;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

/**
 * method generated for container element
 * @author elizaveta.ivanova
 * @since 230
 */
public class ContainerMethod implements PageObjectMethod {

    static final String PAGE_OBJECT_TYPE_PARAMETER_NAME = "pageObjectType";
    static final TypeProvider PAGE_OBJECT_TYPE_PARAMETER_TYPE = new TypeUtilities.FromString("Class<T>");
    static final String INJECTED_SELECTOR_PARAMETER_NAME = "injectedSelector";
    static final TypeProvider INJECTED_SELECTOR_PARAMETER_TYPE = new TypeUtilities.FromClass(Selector.class);
    static final String RETURN_TYPE_STRING = String.format("<T extends %s> T", PageObject.class.getSimpleName());
    static final TypeProvider CONTAINER_ELEMENT_PAGE_OBJECT_TYPE = new TypeUtilities.FromClass(ContainerElementPageObject.class);
    private static final TypeProvider RETURN_TYPE = new TypeUtilities.FromString(RETURN_TYPE_STRING);
    private static final List<TypeProvider> INTERFACE_IMPORTS = Stream.of(
            new TypeUtilities.FromClass(PageObject.class),
            new TypeUtilities.FromClass(Selector.class)
    ).collect(Collectors.toList());
    private static final List<TypeProvider> CLASS_IMPORTS = Stream.of(
            new TypeUtilities.FromClass(ContainerElementPageObject.class),
            new TypeUtilities.FromClass(PageObject.class),
            new TypeUtilities.FromClass(Selector.class)
    ).collect(Collectors.toList());

    private final List<MethodParameter> parameters;
    private final String methodName;
    private final List<String> implCodeLines = new ArrayList<>();
    // this will be used after refactoring
    private final ElementContext scopeElement;

    public ContainerMethod(ElementContext scopeElement, ElementContext element, boolean isPublic) {
        this.scopeElement = scopeElement;
        this.methodName = getElementGetterMethodName(element.getName(), isPublic);
        implCodeLines.add(String.format(
            "if (%s.equals(%s.class)) {",
            PAGE_OBJECT_TYPE_PARAMETER_NAME,
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName()));
        implCodeLines.add(String.format(
            "return (T)(new %s(this.%s))",
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName(),
            element.getName()));
        implCodeLines.add("}");
        List<MethodParameter> scopeParameters = element.getParameters();
        implCodeLines.add(getElementCode(element.getName(), scopeParameters) + String.format(".load(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, INJECTED_SELECTOR_PARAMETER_NAME));
        parameters = new ArrayList<>();
        parameters.add(new ParameterUtils.Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME, PAGE_OBJECT_TYPE_PARAMETER_TYPE));
        parameters.add(new ParameterUtils.Regular(INJECTED_SELECTOR_PARAMETER_NAME, INJECTED_SELECTOR_PARAMETER_TYPE));
        parameters.addAll(scopeParameters);
    }

    private static String getElementCode(String elementName, List<MethodParameter> scopeParameters) {
        if (!scopeParameters.isEmpty()) {
            return String.format(
                    "setParameters(%s, %s)", String.format("this.%s", elementName), getParametersValuesString(scopeParameters));
        } else {
            return String.format("this.%s", elementName);
        }
    }

    // for tests
    ContainerMethod(ElementContext element) {
        this(ROOT_SCOPE, element, true);
    }

    @Override
    public MethodDeclaration getDeclaration() {
        return new MethodDeclarationImpl(methodName, parameters, RETURN_TYPE, INTERFACE_IMPORTS);
    }

    @Override
    public List<String> getCodeLines() {
        return implCodeLines;
    }

    @Override
    public List<TypeProvider> getClassImports() {
        return CLASS_IMPORTS;
    }

    @Override
    public boolean isPublic() {
        return true;
    }
}
