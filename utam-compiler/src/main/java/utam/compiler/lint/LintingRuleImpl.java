package utam.compiler.lint;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.lint.JsonLintRulesConfig.getConfiguredRule;
import static utam.compiler.lint.PageObjectLintingImpl.isCustomElement;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import utam.compiler.lint.JsonLintRulesConfig.LintRuleOverride;
import utam.compiler.lint.JsonLintRulesConfig.LintingRuleObject;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.LintingError.ViolationLevel;
import utam.core.declarative.lint.LintingRule;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.declarative.lint.PageObjectLinting.ElementLinting;
import utam.core.declarative.lint.PageObjectLinting.FileSearchContext;
import utam.core.declarative.lint.PageObjectLinting.MetadataLinting;
import utam.core.declarative.lint.PageObjectLinting.MethodLinting;

/**
 * Base class for linting rules
 *
 * @author elizaveta.ivanova
 * @since 242
 */
abstract class LintingRuleImpl implements LintingRule {

  private static final String ROOT_LINE = "root";
  private final ViolationLevel violationLevel;
  private final String ruleId;
  private final Integer errorCode;
  private final Set<String> exceptions;
  private final String name;
  private final String description;
  final String help;

  private LintingRuleImpl(String ruleId, LintRuleOverride override) {
    LintingRuleObject configuredRule = getConfiguredRule(ruleId);
    this.violationLevel = override == null ? configuredRule.violation : override.violationLevel;
    this.errorCode = configuredRule.errorCode;
    this.exceptions = override == null ? new HashSet<>() : override.exceptions;
    this.name = configuredRule.name;
    this.description = configuredRule.description;
    this.ruleId = ruleId;
    this.help = configuredRule.help;
  }

  @Override
  public String toString() {
    String output = String.format("Rule %s: %s", ruleId, violationLevel.name());
    String exceptions =
        this.exceptions.isEmpty() ? "" : ", exceptions: " + String.join(", ", this.exceptions);
    return output + exceptions;
  }

  final boolean isEnabled(PageObjectLinting context) {
    boolean isReport = exceptions.stream().noneMatch(str -> str.endsWith(context.getName()));
    return violationLevel != LintingError.ViolationLevel.disabled && isReport;
  }

  /**
   * Create linting error for a rule violation
   *
   * @param context page object under linting
   * @param lineNumber number of the line with violation
   * @param fixSuggestion string with suggestion how to fix an error
   * @param args parameters for error message
   */
  final LintingError getError(
      PageObjectLinting context, int lineNumber, String fixSuggestion, String... args) {
    String errMessage = VALIDATION.getLintingMessage(violationLevel, errorCode, args);
    return new LintingErrorImpl(
        ruleId, violationLevel, fixSuggestion, context, lineNumber, errorCode, errMessage);
  }

  @Override
  public void validate(List<LintingError> errors, PageObjectLinting pageObject) {}

  @Override
  public void validate(PageObjectLinting first, PageObjectLinting second, LintingContext context) {}

  @Override
  public String getId() {
    return ruleId;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String getDescription() {
    return description;
  }

  /** Implementation for search context */
  private static class FileSearchContextImpl implements FileSearchContext {

    private final String[] context;

    FileSearchContextImpl(String[] context) {
      this.context = context;
    }

    @Override
    public int find(File file, String string) {
      if (file == null) {
        return -1;
      }
      if (ROOT_LINE.equals(string)) {
        return 1;
      }
      int lineNum = 0;
      int contextIndex = 0;
      boolean foundContext = context.length == 0;
      try {
        Scanner scanner = new Scanner(file);
        while (scanner.hasNextLine()) {
          String line = scanner.nextLine();
          if (foundContext && line.contains(string)) {
            return lineNum + 1;
          }
          if (!foundContext && line.contains(context[contextIndex])) {
            foundContext = context.length == ++contextIndex;
          }
          lineNum++;
        }
      } catch (IOException ignored) {
      }
      return -1;
    }
  }

  /**
   * Check for unique selectors inside same file. Note: list element can have same selector
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class UniqueSelectorInsidePageObject extends LintingRuleImpl {

    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[] {"elements"});

    UniqueSelectorInsidePageObject(LintRuleOverride override) {
      super("ULR01", override);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        List<ElementLinting> elements = pageObject.getElements();
        for (int i = 0; i < elements.size(); i++) {
          ElementLinting first = elements.get(i);
          for (int j = i + 1; j < elements.size(); j++) {
            ElementLinting second = elements.get(j);
            if (first.isSameScope(second) && first.isSameLocator(second)) {
              errors.add(
                  getError(
                      pageObject,
                      pageObject.findCodeLine(SEARCH_CONTEXT, second.getName()),
                      String.format(this.help, first.getName(), second.getName()),
                      first.getLocator(),
                      second.getName(),
                      first.getName()));
            }
          }
        }
      }
    }
  }

  /**
   * Check description at the root level
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RequiredRootDescription extends LintingRuleImpl {
    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[0]);

    RequiredRootDescription(LintRuleOverride override) {
      super("ULR02", override);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject) && !pageObject.getRootContext().hasDescription()) {
        errors.add(
            getError(pageObject, pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE), this.help));
      }
    }
  }

  /**
   * Check description at the root level has an author
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RequiredAuthor extends LintingRuleImpl {
    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[0]);

    RequiredAuthor(LintRuleOverride override) {
      super("ULR03", override);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)
          && pageObject.getRootContext().hasDescription()
          && !pageObject.getRootContext().hasAuthor()) {
        errors.add(
            getError(
                pageObject, pageObject.findCodeLine(SEARCH_CONTEXT, "description"), this.help));
      }
    }
  }

  /**
   * Check metadata object for presence, and for presence and proper values of properties.
   *
   * @author james.evans
   * @since 248
   */
  static class RequiredMetadata extends LintingRuleImpl {
    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[0]);
    private static final String MISSING_METADATA_PROPERTY_ERROR =
        "add a property named \"%s\" to the metadata object in the page object";
    private static final String INVALID_METADATA_PROPERTY_VALUE =
        "set the \"%s\" metadata property to one of the following values: %s";
    private static final String EMPTY_METADATA_PROPERTY_VALUE =
        "set the \"%s\" metadata property a non-empty value";
    private final List<Object> requiredProperties;

    RequiredMetadata(LintRuleOverride override) {
      super("ULR09", override);
      this.requiredProperties =
          override == null || override.additionalConfig == null
              ? null
              : (List<Object>)
                  override.additionalConfig.getAdditionalConfigValue("requiredProperties");
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        if (!pageObject.getRootContext().hasMetadata()) {
          errors.add(
              getError(pageObject, pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE), this.help));
        } else {
          if (requiredProperties != null) {
            MetadataLinting metadataLinting = pageObject.getRootContext().getMetadata();
            for (Object propertyObject : requiredProperties) {
              Map<String, Object> requiredProperty = (Map<String, Object>) propertyObject;
              String propertyName = requiredProperty.get("name").toString();
              if (!metadataLinting.hasMetadataProperty(propertyName)) {
                errors.add(
                    getError(
                        pageObject,
                        pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE),
                        String.format(MISSING_METADATA_PROPERTY_ERROR, propertyName)));
              } else {
                String propertyValue =
                    metadataLinting.getMetadataPropertyValue(propertyName).toString();
                if (requiredProperty.containsKey("values")) {
                  List<String> validValues = (List<String>) requiredProperty.get("values");
                  if (!validValues.contains(propertyValue)) {
                    errors.add(
                        getError(
                            pageObject,
                            pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE),
                            String.format(
                                INVALID_METADATA_PROPERTY_VALUE,
                                propertyName,
                                String.join(", ", validValues))));
                  }
                } else {
                  if (propertyValue.isBlank() || propertyValue.isEmpty()) {
                    errors.add(
                        getError(
                            pageObject,
                            pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE),
                            String.format(EMPTY_METADATA_PROPERTY_VALUE, propertyName)));
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  /**
   * Check every method has description
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RequiredMethodDescription extends LintingRuleImpl {
    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[] {"methods"});

    RequiredMethodDescription(LintRuleOverride override) {
      super("ULR04", override);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        for (MethodLinting method : pageObject.getMethods()) {
          if (!method.hasDescription()) {
            errors.add(
                getError(
                    pageObject,
                    pageObject.findCodeLine(SEARCH_CONTEXT, method.getName()),
                    String.format(this.help, method.getName()),
                    method.getName()));
          }
        }
      }
    }
  }

  /**
   * Root selector should be unique across all POs
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class UniqueRootSelector extends LintingRuleImpl {

    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[0]);

    UniqueRootSelector(LintRuleOverride override) {
      super("ULR06", override);
    }

    private boolean isSameRoot(PageObjectLinting first, PageObjectLinting second) {
      if (first.getRootContext().isRoot() && second.getRootContext().isRoot()) {
        return first
            .getRootContext()
            .getRootElement()
            .isSameLocator(second.getRootContext().getRootElement());
      }
      return false;
    }

    @Override
    public void validate(
        PageObjectLinting first, PageObjectLinting second, LintingContext context) {
      if (isEnabled(first) && isEnabled(second) && isSameRoot(first, second)) {
        String locator = first.getRootContext().getRootElement().getLocator();
        context
            .getErrors()
            .add(
                getError(
                    first,
                    first.findCodeLine(SEARCH_CONTEXT, "selector"),
                    String.format(this.help, first.getName(), second.getName()),
                    locator,
                    second.getName()));
      }
    }
  }

  /**
   * Root selector should match only custom elements of the same type as root selector's PO
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RootSelectorExistsForElement extends LintingRuleImpl {
    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[] {"elements"});

    RootSelectorExistsForElement(LintRuleOverride override) {
      super("ULR07", override);
    }

    private static boolean isRootDuplicateLocator(PageObjectLinting first, ElementLinting element) {
      if (first.getTypeFullName().equals(element.getTypeFullName())) {
        // if element is same type as page object
        return false;
      }
      return first.getRootContext().getRootElement().isSameLocator(element);
    }

    @Override
    public void validate(
        PageObjectLinting first, PageObjectLinting second, LintingContext context) {
      if (isEnabled(first) && isEnabled(second) && first.getRootContext().isRoot()) {
        String existingPageObjectType = first.getTypeFullName();
        for (ElementLinting element : second.getElements()) {
          if (isRootDuplicateLocator(first, element)) {
            String violatedElementName = element.getName();
            context
                .getErrors()
                .add(
                    getError(
                        second,
                        second.findCodeLine(SEARCH_CONTEXT, violatedElementName),
                        String.format(this.help, violatedElementName, existingPageObjectType),
                        violatedElementName,
                        existingPageObjectType));
          }
        }
      }
    }
  }

  /*
  PREVIOUS HARDCODED EXCLUSIONS
  static {
    // COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/groupedCombobox", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/timepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/combobox", "labelText");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/input", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/textarea", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/quill", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/select", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/datetimepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/datepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-aura/pageObjects/modal", "modalFooter");
  }
  */

  /**
   * Element with custom tag should have same type across all POs
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class ElementsWithDifferentTypes extends LintingRuleImpl {

    private static final FileSearchContext SEARCH_CONTEXT =
        new FileSearchContextImpl(new String[] {"elements"});

    ElementsWithDifferentTypes(LintRuleOverride override) {
      super("ULR08", override);
    }

    private static boolean isSameCustomLocatorButDifferentTypes(
        ElementLinting first, ElementLinting second) {
      // if element is same type, locator is irrelevant, could be duplicate or not
      if (!isCustomLocator(first)
          || !isCustomLocator(second)
          || first.getTypeFullName().equals(second.getTypeFullName())) {
        return false;
      }
      return first.isSameLocator(second)
          && (isCustomElement(first) || isCustomElement(second))
          && !first.getTypeFullName().equals(second.getTypeFullName());
    }

    private static boolean isCustomLocator(ElementLinting element) {
      return element.getLocator().contains("-");
    }

    @Override
    public void validate(
        PageObjectLinting first, PageObjectLinting second, LintingContext context) {
      if (isEnabled(first) && isEnabled(second)) {
        for (ElementLinting firstElement : first.getElements()) {
          if (isCustomLocator(firstElement)) { // for custom tags only
            String firstElementName = firstElement.getName();
            String firstElementLocator = firstElement.getLocator();
            for (ElementLinting secondElement : second.getElements()) {
              if (isSameCustomLocatorButDifferentTypes(firstElement, secondElement)) {
                String secondElementName = secondElement.getName();
                context
                    .getErrors()
                    .add(
                        getError(
                            second,
                            second.findCodeLine(SEARCH_CONTEXT, secondElementName),
                            String.format(
                                this.help, secondElementName, firstElementName, second.getName()),
                            firstElementLocator,
                            secondElementName,
                            firstElementName,
                            second.getName(),
                            secondElement.getTypeFullName()));
              }
            }
          }
        }
      }
    }
  }
}
