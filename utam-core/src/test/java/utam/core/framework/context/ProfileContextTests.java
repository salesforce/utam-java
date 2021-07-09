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
import static utam.core.framework.consumer.PageObjectContextImpl.ERR_GET_CLASS_BY_NAME;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import org.testng.annotations.Test;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.framework.consumer.TestLoaderConfigPageObjectOverride;
import utam.core.framework.consumer.TestLoaderConfigPageObjectProfile;
import utam.core.framework.consumer.UtamError;

public class ProfileContextTests {

  @Test
  public void testEmptyProfileContextSetupOverride() {
    DefaultProfileContext profile = new DefaultProfileContext(null, DEFAULT_PROFILE);
    assertThat(profile.getBeanName(null), is(nullValue()));
    profile.setBean(TestLoaderConfigPageObject.class, "class");
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class),
        is(equalTo("class")));
  }

  @Test
  public void testNotExistingConfig() {
    DefaultProfileContext profile = new DefaultProfileContext(null, new StringValueProfile("foo", "bar"));
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
  }

  @Test
  public void testNotExistingConfigWithModule() {
    DefaultProfileContext profile = new DefaultProfileContext("module", new StringValueProfile("foo", "bar"));
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
  }

  @Test
  public void testExistingConfig() {
    // read beans from config test_profiles_config
    ProfileContext profileContext =
        new DefaultProfileContext(null, new StringValueProfile("test", "profiles"));
    assertThat(profileContext.getBeanName(TestLoaderConfigPageObject.class),
        is(equalTo(TestLoaderConfigPageObjectOverride.class.getName())));
  }

  @Test
  public void testExistingConfigWithModule() {
    // read beans from config module1_test_profiles_config
    ProfileContext profileContext =
        new DefaultProfileContext("module1", new StringValueProfile("test", "profiles"));
    assertThat(profileContext.getBeanName(TestLoaderConfigPageObject.class),
        is(equalTo(TestLoaderConfigPageObjectProfile.class.getName())));
  }

  @Test
  public void testNonExistingClassThrows() {
    // utam.test.Error=utam.test.ErrorImpl
    UtamError error = expectThrows(UtamError.class,
        () -> new DefaultProfileContext(null, new StringValueProfile("test", "error")));
    assertThat(error.getMessage(), is(equalTo(String.format(ERR_GET_CLASS_BY_NAME, "utam.test.Error"))));
  }
}
