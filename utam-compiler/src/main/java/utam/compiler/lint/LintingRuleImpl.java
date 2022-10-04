package utam.compiler.lint;

import static utam.compiler.lint.PageObjectLintingImpl.isCustomElement;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;
import java.util.Set;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.LintingError.ViolationLevel;
import utam.core.declarative.lint.LintingRule;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.declarative.lint.PageObjectLinting.ElementLinting;
import utam.core.declarative.lint.PageObjectLinting.FileSearchContext;
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

  private LintingRuleImpl(String ruleId, String name, String description,
      ViolationLevel violationLevel, Integer errorCode, Set<String> exceptions) {
    this.violationLevel = Objects
        .requireNonNullElse(violationLevel, LintingError.ViolationLevel.error);
    this.errorCode = errorCode;
    this.exceptions = Objects.requireNonNullElse(exceptions, new HashSet<>());
    this.name = name;
    this.description = description;
    this.ruleId = ruleId;
  }

  final boolean isEnabled(PageObjectLinting context) {
    boolean isReport = exceptions.stream().noneMatch(str -> str.endsWith(context.getName()));
    return violationLevel != LintingError.ViolationLevel.disabled && isReport;
  }

  /**
   * Create linting error for a rule violation
   *
   * @param context       page object under linting
   * @param lineNumber    number of the line with violation
   * @param fixSuggestion string with suggestion how to fix an error
   * @param args          parameters for error message
   */
  final LintingError getError(PageObjectLinting context, int lineNumber,
      String fixSuggestion, String... args) {
    return new LintingErrorImpl(ruleId, violationLevel, fixSuggestion, context, lineNumber,
        errorCode,
        args);
  }

  @Override
  public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
  }

  @Override
  public void validate(List<LintingError> errors, LintingContext context) {
  }

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

  /**
   * Implementation for search context
   */
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
   * Check for unique selectors inside same file. By default warning because list element can have
   * same selector
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  static class UniqueSelectorInsidePageObject extends LintingRuleImpl {

    static final UniqueSelectorInsidePageObject DEFAULT = new UniqueSelectorInsidePageObject(
        LintingError.ViolationLevel.error, new HashSet<>());
    static final String RULE_ID = "ULR01";
    private static final String NAME = "Unique local selectors";
    private static final String DESCRIPTION = "Check for unique selectors inside same file. "
        + "By default warning because list element can have same selector";
    private static final String FIX = "remove duplicate elements: \"%s\" or \"%s\"";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[]{"elements"});

    @JsonCreator
    UniqueSelectorInsidePageObject(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 2001, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        for (String locator : pageObject.getAllLocators()) {
          List<ElementLinting> elements = pageObject.getElementsByLocator(locator);
          for (int i = 1; i < elements.size(); i++) {
            ElementLinting first = elements.get(i - 1);
            ElementLinting second = elements.get(i);
            if (first.getParentScope().equals(second.getParentScope())
                // lists can have duplicates
                && !first.isList() && !second.isList()) {
              errors.add(getError(pageObject,
                  pageObject.findCodeLine(SEARCH_CONTEXT, second.getName()),
                  String.format(FIX, first.getName(), second.getName()),
                  locator,
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

    static final RequiredRootDescription DEFAULT = new RequiredRootDescription(
        // warning because it's optional property
        LintingError.ViolationLevel.warning,
        new HashSet<>());
    static final String RULE_ID = "ULR02";
    private static final String NAME = "Required root description";
    private static final String DESCRIPTION = "Check description at the root level";
    private static final String FIX = "add \"description\" property at the root";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[0]);


    @JsonCreator
    RequiredRootDescription(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 2002, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject) && !pageObject.getRootContext().hasDescription()) {
        errors.add(getError(pageObject, pageObject.findCodeLine(SEARCH_CONTEXT, ROOT_LINE), FIX));
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

    static final RequiredAuthor DEFAULT = new RequiredAuthor(
        // warning because it's optional property
        LintingError.ViolationLevel.warning,
        new HashSet<>());
    static final String RULE_ID = "ULR03";
    static final String NAME = "Required author";
    static final String DESCRIPTION = "Check description at the root level has an author";
    static final String FIX = "add \"author\" property to the root description";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[0]);

    @JsonCreator
    RequiredAuthor(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 2005, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject) && pageObject.getRootContext().hasDescription()
          && !pageObject.getRootContext()
          .hasAuthor()) {
        errors.add(getError(pageObject,
            pageObject.findCodeLine(SEARCH_CONTEXT, "description"), FIX));
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

    static final RequiredMethodDescription DEFAULT = new RequiredMethodDescription(
        // warning because it's optional property
        LintingError.ViolationLevel.warning,
        new HashSet<>());
    static final String RULE_ID = "ULR04";
    private static final String NAME = "Required method description";
    private static final String DESCRIPTION = "Check every compose method has description";
    private static final String FIX = "add \"description\" property to the method \"%s\"";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[]{"methods"});

    @JsonCreator
    RequiredMethodDescription(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 2003, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        for (MethodLinting method : pageObject.getMethods()) {
          if (!method.hasDescription()) {
            errors.add(getError(pageObject,
                pageObject.findCodeLine(SEARCH_CONTEXT, method.getName()),
                String.format(FIX, method.getName()),
                method.getName()));
          }
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
  static class SingleShadowBoundaryAllowed extends LintingRuleImpl {

    static final SingleShadowBoundaryAllowed DEFAULT = new SingleShadowBoundaryAllowed(
        // warning because it depends on the team
        LintingError.ViolationLevel.warning,
        new HashSet<>());
    static final String RULE_ID = "ULR05";
    private static final String NAME = "Single shadowRoot";
    private static final String DESCRIPTION = "Check only one shadowRoot present at the component root";
    private static final String FIX = "remove \"shadow\" under element \"%s\" and create separate page object for its content";

    @JsonCreator
    SingleShadowBoundaryAllowed(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 2004, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, PageObjectLinting pageObject) {
      if (isEnabled(pageObject)) {
        for (String elementName : pageObject.getShadowBoundaries()) {
          FileSearchContext context = new FileSearchContextImpl(
              new String[]{"elements", elementName});
          errors.add(getError(pageObject,
              pageObject.findCodeLine(context, "shadow"),
              String.format(FIX, elementName), elementName));
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

    static final UniqueRootSelector DEFAULT = new UniqueRootSelector(
        LintingError.ViolationLevel.error,
        new HashSet<>());
    static final String RULE_ID = "ULR06";
    private static final String NAME = "Unique root selector";
    private static final String DESCRIPTION = "Check root selector is unique across all page objects";
    private static final String FIX = "remove one of the page objects with same root selector: \"%s\" or \"%s\"";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[0]);

    @JsonCreator
    UniqueRootSelector(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 3001, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, LintingContext context) {
      Set<String> processedLocators = new HashSet<>();
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        if (isEnabled(pageObjectContext)) {
          String locatorKey = pageObjectContext.getRootContext().getLocator();
          if (locatorKey != null && !processedLocators.contains(locatorKey)) {
            List<PageObjectLinting> pageObjects = context.getByRootLocator(locatorKey);
            if (pageObjects != null && pageObjects.size() > 1) {
              for (int i = 0; i < pageObjects.size() - 1; i++) {
                PageObjectLinting first = pageObjects.get(i);
                if (isEnabled(first)) {
                  PageObjectLinting second = pageObjects.get(i + 1);
                  errors.add(getError(first,
                      first.findCodeLine(SEARCH_CONTEXT, "selector"),
                      String.format(FIX, first.getName(), second.getName()),
                      locatorKey, second.getName()));
                }
              }
            }
            processedLocators.add(locatorKey);
          }
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
  static class RootSelectorExistsForElement extends LintingRuleImpl {

    static final RootSelectorExistsForElement DEFAULT = new RootSelectorExistsForElement(
        LintingError.ViolationLevel.error,
        new HashSet<>());
    static final String RULE_ID = "ULR07";
    private static final String NAME = "Root selector usage";
    private static final String DESCRIPTION = "Root selector should match only custom elements of the same type";
    private static final String FIX = "change the element \"%s\" type to the type of the page object \"%s\"";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[]{"elements"});

    @JsonCreator
    RootSelectorExistsForElement(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 3002, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, LintingContext context) {
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        if (isEnabled(pageObjectContext)) {
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
                    errors.add(getError(pageObjectContext,
                        pageObjectContext.findCodeLine(SEARCH_CONTEXT, element.getName()),
                        String.format(FIX, element.getName(), existingPageObjectType),
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

    static final ElementsWithDifferentTypes DEFAULT = new ElementsWithDifferentTypes(
        LintingError.ViolationLevel.error, new HashSet<>());
    static final String RULE_ID = "ULR08";
    private static final String NAME = "Custom selector";
    private static final String DESCRIPTION = "Element with custom tag should have same type across all page objects";
    private static final String FIX = "change the element \"%s\" type to the same type as the element \"%s\" in page object \"%s\"";
    private static final FileSearchContext SEARCH_CONTEXT = new FileSearchContextImpl(
        new String[]{"elements"});

    @JsonCreator
    ElementsWithDifferentTypes(
        @JsonProperty(value = "violation") ViolationLevel ruleType,
        @JsonProperty(value = "exclude") Set<String> exceptions) {
      super(RULE_ID, NAME, DESCRIPTION, ruleType, 3003, exceptions);
    }

    @Override
    public void validate(List<LintingError> errors, LintingContext context) {
      final Set<String> processedLocators = new HashSet<>();
      for (PageObjectLinting pageObjectContext : context.getAllPageObjects()) {
        if (isEnabled(pageObjectContext)) {
          for (String locator : pageObjectContext.getAllLocators()) {
            if (locator.contains("-") && !processedLocators.contains(locator)) {
              String pageObjectUnderTest = pageObjectContext.getName();
              for (PageObjectLinting existing : context.getAllPageObjects()) {
                if (!isEnabled(existing) || existing.getName().equals(pageObjectUnderTest)
                    || !existing.getAllLocators()
                    .contains(locator)) {
                  continue;
                }
                List<ElementLinting> elementsUnderTest = pageObjectContext
                    .getElementsByLocator(locator);
                List<ElementLinting> existingElements = existing.getElementsByLocator(locator);
                for (ElementLinting existingElement : existingElements) {
                  for (ElementLinting element : elementsUnderTest) {
                    if (isCustomElement(existingElement) || isCustomElement(element)) {
                      errors.add(getError(pageObjectContext,
                          pageObjectContext.findCodeLine(SEARCH_CONTEXT, element.getName()),
                          String.format(FIX, element.getName(), existingElement.getName(),
                              existing.getName()),
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
}
