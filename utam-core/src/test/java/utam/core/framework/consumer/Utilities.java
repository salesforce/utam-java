/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.util.Collections;

/**
 * utilities to make package private methods of PageObjectContext
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class Utilities {

    public static PageObjectContext test() {
        return new PageObjectContextImpl(Collections.emptyMap());
    }
}
