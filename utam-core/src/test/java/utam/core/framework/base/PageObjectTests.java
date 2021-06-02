/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.framework.base.PageObject;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.ElementLocationChain;

import java.util.function.Supplier;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
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

    @Test
    public void testWaitFor() {
        MockUtilities mock = new MockUtilities();
        TestPageImpl testPage = new TestPageImpl();
        testPage.setBootstrap(new ElementLocationChain(mock.getElementAdapter()), mock.getFactory());

        Supplier<Object> apply =
                () -> {
                    mock.getElementAdapter().setText("text");
                    return mock.getElementAdapter();
                };

        BasePageElement element = mock.getUtamElement();
        assertThat(testPage.waitFor(apply), CoreMatchers.is(notNullValue()));

        // successfully waits for visibility
        when(mock.getElementAdapter().isDisplayed()).thenReturn(true);
        assertThat(testPage.waitFor(() -> element.isVisible()), CoreMatchers.is(true));

        // throws exception when visibility times out
        when(mock.getElementAdapter().isDisplayed()).thenReturn(false);
        assertThrows(() -> testPage.waitFor(() -> mock.getUtamElement().isVisible()));
    }

    interface TestPage extends PageObject {}

    static class TestPageImpl extends BasePageObject implements TestPage {}
}
