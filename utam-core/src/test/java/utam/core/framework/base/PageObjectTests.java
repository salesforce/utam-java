/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import org.testng.annotations.Test;
import utam.core.framework.base.PageObject;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

/**
 * Tests the default implementation members of the PageObject interface
 *
 * @author james.evans
 */
public class PageObjectTests {

    @Test
    public void testDefaultMethods() {
        final PageObject MOCK = new PageObject() {};
        MOCK.load();
        assertThat(MOCK.isPresent(), is(equalTo(false)));
    }
}
