/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.context.DefaultProfileContext.ERR_GET_CLASS_BY_NAME;
import static utam.core.framework.context.DefaultProfileContext.getClassFromName;

import org.testng.annotations.Test;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.framework.consumer.UtamError;

public class ProfileContextTests {

  @Test
  public void testProfileContext() {
    DefaultProfileContext profile = new DefaultProfileContext();
    assertThat(profile.getBeanName(null), is(nullValue()));
    profile.setBean(TestLoaderConfigPageObject.class, "class");
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class), is(equalTo("class")));
  }

  @Test
  public void testNonExistingClassThrows() {
    UtamError error = expectThrows(UtamError.class, () -> getClassFromName("utam.test.Error"));
    assertThat(
        error.getMessage(), is(equalTo(String.format(ERR_GET_CLASS_BY_NAME, "utam.test.Error"))));
  }
}
