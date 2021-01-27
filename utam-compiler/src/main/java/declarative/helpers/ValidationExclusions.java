package declarative.helpers;

import java.util.*;

import static declarative.helpers.Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES;
import static declarative.helpers.Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;

/**
 * static guardrails exclusions
 *
 * @author elizaveta.ivanova
 * @since 230
 */
class ValidationExclusions {

  private static final Map<String, String> DIFFERENT_COMPONENT_TYPES = Collections.synchronizedMap(new HashMap<>());
  private static final Map<String, String> ELEMENT_AND_COMPONENT = Collections.synchronizedMap(new HashMap<>());

  static {
    // COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES
    DIFFERENT_COMPONENT_TYPES.put("utam-lists/pageObjects/lst/relatedPreviewCard", "rowLevelActions");

    // COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/groupedCombobox", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/timepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/combobox", "labelText");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/input", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/textarea", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/quill", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/select", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/datetimepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/datepicker", "label");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/lightning/primitiveCellFactory", "listViewRowLevelAction");
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/ui/virtualDataTable", "primitiveCellCheckbox");
    ELEMENT_AND_COMPONENT.put("utam-force/pageObjects/force/modalContainer", "modalFooter");
  }

  static synchronized boolean isExceptionAllowed(String pageObject, String elementName, Validation.ErrorType error) {
    if(error == COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR) {
      return ELEMENT_AND_COMPONENT.containsKey(pageObject)
              && ELEMENT_AND_COMPONENT.get(pageObject).equals(elementName);
    }
    if(error == COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES) {
      return DIFFERENT_COMPONENT_TYPES.containsKey(pageObject)
              && DIFFERENT_COMPONENT_TYPES.get(pageObject).equals(elementName);
    }
    return false;
  }
}
