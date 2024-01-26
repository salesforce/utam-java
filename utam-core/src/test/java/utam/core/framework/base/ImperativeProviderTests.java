/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.testng.Assert.assertThrows;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class ImperativeProviderTests {

  @Test
  public void testInitUtils() {
    TestPage testPage = new TestPageImpl();
    assertThat(testPage.getUtils().getInstance(), is(sameInstance(testPage)));
  }

  @Test
  public void testInitUtilsErr() {
    assertThrows(UtamError.class, () -> ImperativeProvider.build(ErrorUtils.class));
  }

  interface TestPage extends PageObject {

    Utils getUtils();
  }

  static class ErrorUtils extends ImperativeProvider<TestPage> {

    ErrorUtils(String str) {}
  }

  static class Utils extends ImperativeProvider<TestPage> {

    Utils() {}
  }

  static class TestPageImpl extends BasePageObject implements TestPage {

    @Override
    public Utils getUtils() {
      return getUtility(Utils.class);
    }
  }
}
