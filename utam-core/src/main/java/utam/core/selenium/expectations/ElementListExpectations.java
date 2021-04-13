/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.selenium.expectations;

import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.List;
import java.util.function.Function;

/**
 * element expectations that checks for true/false <br>
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface ElementListExpectations<T> {

  T returnIfNothingFound();

  String getLogMessage();

  Function<List<WebElement>, T> apply(WebDriverUtilities utilities);
}
