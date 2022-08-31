package utam.compiler.lint;

import static utam.compiler.lint.PageObjectLintingImpl.isCustomElement;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.declarative.lint.PageObjectLinting.ElementLinting;
import utam.core.declarative.lint.PageObjectLinting.MethodLinting;

/**
 * Base class for linting rules
 *
 * @author elizaveta.ivanova
 * @since 242
 */
abstract class LintingRule {

  private static final ViolationType DEFAULT_RULE_TYPE = ViolationType.error;
  private static final Set<String> RULE_NO_EXCEPTION = new HashSet<>();

  private final ViolationType violationType;
  private final Integer errorCode;
  private final Set<String> exceptions;

  private LintingRule(ViolationType ruleType, Integer errorCode, Set<String> exceptions) {
    this.violationType = Objects.requireNonNullElse(ruleType, DEFAULT_RULE_TYPE);
    this.errorCode = errorCode;
    this.exceptions = Objects.requireNonNullElse(exceptions, RULE_NO_EXCEPTION);
  }

  // is Rule enabled?
  final boolean isEnabled() {
    return violationType != ViolationType.disabled;
  }

  /**
   * Transform violation type and add to list of errors
   *
   * @param context page object under linting
   * @param args    parameters for error message
   */
  final LintingError getError(PageObjectLinting context, String... args) {
    boolean isReport = exceptions.stream().noneMatch(str -> str.endsWith(context.getName()));
    return new LintingErrorImpl(violationType, isReport, errorCode, args);
  }

  void validate(List<LintingError> errors, PageObjectLinting pageObjectContext) {
  }

  void validate(List<LintingError> errors, LintingContext context) {
  }

  /**
   * Type of the linting rule
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  enum ViolationType {
    // lower case for deserialization
    error,
    warning,
    disabled
  }

  /**
   * Check for unique selectors inside same file. By default warning because list element can have
   * same selector
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class UniqueSelectorInsidePageObject extends LintingRule {

    static final UniqueSelectorInsidePageObject DEFAULT = new UniqueSelectorInsidePageObject(
        DEFAULT_RULE_TYPE, new HashSet<>());

    @JsonCreator
    UniqueSelectorInsidePageObject(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 2001, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, PageObjectLinting pageObjectContext) {
      for (String locator : pageObjectContext.getAllLocators()) {
        List<ElementLinting> elements = pageObjectContext.getElementsByLocator(locator);
        for (int i = 1; i < elements.size(); i++) {
          ElementLinting first = elements.get(i - 1);
          ElementLinting second = elements.get(i);
          if (first.getParentScope().equals(second.getParentScope())
              // lists can have duplicates
              && !first.isList() && !second.isList()) {
            errors.add(getError(pageObjectContext,
                pageObjectContext.getName(),
                locator,
                second.getName(),
                first.getName()));
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
  static class RequiredRootDescription extends LintingRule {

    static final RequiredRootDescription DEFAULT = new RequiredRootDescription(DEFAULT_RULE_TYPE,
        new HashSet<>());

    @JsonCreator
    RequiredRootDescription(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 2002, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, PageObjectLinting pageObjectContext) {
      if (!pageObjectContext.getRootContext().hasDescription()) {
        errors.add(getError(pageObjectContext, pageObjectContext.getName()));
      }
    }
  }

  /**
   * Check every method has description
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RequiredMethodDescription extends LintingRule {

    static final RequiredMethodDescription DEFAULT = new RequiredMethodDescription(
        DEFAULT_RULE_TYPE,
        new HashSet<>());

    @JsonCreator
    RequiredMethodDescription(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 2003, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, PageObjectLinting pageObjectContext) {
      for (MethodLinting method : pageObjectContext.getMethods()) {
        if (!method.hasDescription()) {
          errors.add(getError(pageObjectContext, pageObjectContext.getName(), method.getName()));
        }
      }
    }
  }

  /**
   * Check for single shadow boundary at the root level
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class SingleShadowBoundaryAllowed extends LintingRule {

    static final SingleShadowBoundaryAllowed DEFAULT = new SingleShadowBoundaryAllowed(
        DEFAULT_RULE_TYPE,
        new HashSet<>());

    @JsonCreator
    SingleShadowBoundaryAllowed(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 2004, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, PageObjectLinting pageObjectContext) {
      for (String elementName : pageObjectContext.getShadowBoundaries()) {
        errors.add(getError(pageObjectContext, pageObjectContext.getName(), elementName));
      }
    }
  }

  /**
   * Root selector should be unique across all POs
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class UniqueRootSelector extends LintingRule {

    static final UniqueRootSelector DEFAULT = new UniqueRootSelector(DEFAULT_RULE_TYPE,
        new HashSet<>());

    @JsonCreator
    UniqueRootSelector(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 3001, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, LintingContext context) {
      Set<String> processedLocators = new HashSet<>();
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        String locatorKey = pageObjectContext.getRootContext().getLocator();
        if (locatorKey != null && !processedLocators.contains(locatorKey)) {
          List<PageObjectLinting> pageObjects = context.getByRootLocator(locatorKey);
          if (pageObjects != null && pageObjects.size() > 1) {
            for (int i = 0; i < pageObjects.size() - 1; i++) {
              PageObjectLinting first = pageObjects.get(i);
              PageObjectLinting second = pageObjects.get(i + 1);
              errors.add(getError(first, first.getName(), locatorKey, second.getName()));
            }
          }
          // remove to avoid reporting same error
          processedLocators.add(locatorKey);
        }
      }
    }
  }

  /**
   * Root selector should match only custom elements of the same type as root selector's PO
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class RootSelectorExistsForElement extends LintingRule {

    static final RootSelectorExistsForElement DEFAULT = new RootSelectorExistsForElement(
        DEFAULT_RULE_TYPE,
        new HashSet<>());

    @JsonCreator
    RootSelectorExistsForElement(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 3002, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, LintingContext context) {
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        for (String elementSelector : pageObjectContext.getAllLocators()) {
          List<PageObjectLinting> existingPageObjects = context.getByRootLocator(elementSelector);
          if (existingPageObjects != null) {
            // report only first error that is not for same PO, others mean there are violation of rule 3001
            PageObjectLinting existingPageObject = existingPageObjects
                .stream()
                .filter(po -> !po.getName().equals(pageObjectContext.getName()))
                .findAny()
                .orElse(null); // null returned if it's same PO
            if (existingPageObject != null) {
              String existingPageObjectType = existingPageObject.getTypeFullName();
              for (ElementLinting element : pageObjectContext
                  .getElementsByLocator(elementSelector)) {
                if (!element.getFullTypeName().equals(existingPageObjectType)) {
                  errors.add(getError(pageObjectContext, pageObjectContext.getName(),
                      element.getName(),
                      existingPageObjectType));
                }
              }
            }
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
  static class ElementsWithDifferentTypes extends LintingRule {

    static final ElementsWithDifferentTypes DEFAULT = new ElementsWithDifferentTypes(
        DEFAULT_RULE_TYPE, new HashSet<>());

    @JsonCreator
    ElementsWithDifferentTypes(
        @JsonProperty(value = "violation") ViolationType ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(ruleType, 3003, exceptions);
    }

    @Override
    void validate(List<LintingError> errors, LintingContext context) {
      final Set<String> processedLocators = new HashSet<>();
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        for (String locator : pageObjectContext.getAllLocators()) {
          if (locator.contains("-") && !processedLocators.contains(locator)) {
            String pageObjectUnderTest = pageObjectContext.getName();
            for (PageObjectLinting existing : context.getAllPageObjects()) {
              if (existing.getName().equals(pageObjectUnderTest) || !existing.getAllLocators()
                  .contains(locator)) {
                // do not test self
                continue;
              }
              List<ElementLinting> elementsUnderTest = pageObjectContext
                  .getElementsByLocator(locator);
              List<ElementLinting> existingElements = existing.getElementsByLocator(locator);
              for (ElementLinting existingElement : existingElements) {
                for (ElementLinting element : elementsUnderTest) {
                  if (isCustomElement(existingElement) || isCustomElement(element)) {
                    errors.add(getError(pageObjectContext,
                        pageObjectUnderTest,
                        locator,
                        element.getName(),
                        existingElement.getName(),
                        existing.getName()));
                  }
                }
                // to avoid reporting duplicates
                processedLocators.add(locator);
              }
            }
          }
        }
      }
    }
  }
}
