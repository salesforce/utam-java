/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static utam.core.driver.DriverContext.DEFAULT;
import static utam.core.driver.DriverContext.TEST;

import java.time.Duration;
import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverContextTests {

  @Test
  public void testGetTimeouts() {
    assertThat(DEFAULT.getTimeouts(), is(equalTo(DriverTimeouts.DEFAULT)));
    assertThat(TEST.getTimeouts(), is(equalTo(DriverTimeouts.TEST)));
  }

  @Test
  public void testGetBridgeAppTitle() {
    assertThat(DEFAULT.getBridgeAppTitle(), is(emptyString()));
    assertThat(TEST.getBridgeAppTitle(), is(emptyString()));
  }

  @Test
  public void testCustomContext() {
    DriverTimeouts myTimeouts = new DriverTimeouts(Duration.ofSeconds(1), Duration.ofSeconds(2), Duration.ofSeconds(3));
    DriverContext myContext = new DriverContext(myTimeouts, "testAppTitle");
    assertThat(myContext.getTimeouts(), is(equalTo(myTimeouts)));
    assertThat(myContext.getBridgeAppTitle(), is(equalTo("testAppTitle")));
  }
}