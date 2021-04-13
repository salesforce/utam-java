/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import utam.core.framework.context.Driver;
import org.testng.annotations.Test;
import utam.core.selenium.factory.WebDriverFactory;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.factory.WebDriverFactory.ERR_UNKNOWN_DRIVER_TYPE;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class WebDriverFactoryTests {

    @Test
    void testChromeOptions() {
        WebDriverFactory.defaultChromeOptions(false);
        WebDriverFactory.defaultChromeOptions(true);
        assertThrows(() -> WebDriverFactory.getWebDriver(mock(Driver.class)));
    }

    @Test
    void testGetDriverError() {
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> WebDriverFactory.getWebDriver(null));
        assertThat(e.getMessage(), is(equalTo(String.format(ERR_UNKNOWN_DRIVER_TYPE, "null"))));
    }
}
