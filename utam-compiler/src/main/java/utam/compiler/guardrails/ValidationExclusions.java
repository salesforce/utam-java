/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import java.util.*;

import static utam.compiler.guardrails.ValidationError.*;

/**
 * static guardrails exclusions.
 * This file is LOCKED FOR CHANGES
 *
 * @author elizaveta.ivanova
 * @since 230
 */
class ValidationExclusions {

  private static final Map<String, String> ELEMENT_AND_COMPONENT = Collections.synchronizedMap(new HashMap<>());

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

  static synchronized boolean isViolationAllowed(String pageObject, String elementName, ValidationError error) {
    if(error == COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR) {
      return ELEMENT_AND_COMPONENT.containsKey(pageObject)
              && ELEMENT_AND_COMPONENT.get(pageObject).equals(elementName);
    }
    return false;
  }
}
