package utam.compiler.helpers;

import java.util.*;

import static utam.compiler.helpers.Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES;
import static utam.compiler.helpers.Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;

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
    DIFFERENT_COMPONENT_TYPES.put("utam-lists/pageObjects/relatedPreviewCard", "rowLevelActions");

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
    ELEMENT_AND_COMPONENT.put("utam-lightning/pageObjects/primitiveCellFactory", "listViewRowLevelAction");
    ELEMENT_AND_COMPONENT.put("utam-aura/pageObjects/virtualDataTable", "primitiveCellCheckbox");
    ELEMENT_AND_COMPONENT.put("utam-force/pageObjects/modalContainer", "modalFooter");
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
