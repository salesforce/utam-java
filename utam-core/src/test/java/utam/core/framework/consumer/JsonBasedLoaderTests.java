/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.JsonBasedLoader.ERR_READING_LOADER_CONFIG;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.util.List;
import org.testng.annotations.Test;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonBasedLoaderTests {

  @Test
  public void testExistingJson() {
    JsonBasedLoader json = new JsonBasedLoader("module.loader");
    assertThat(json.getModules(), is(containsInAnyOrder(null, "module1", "module2")));
    List<Profile> profiles = json.getConfiguredProfiles();
    assertThat(profiles.size(), is(3));
    assertThat(profiles.get(0),
        is(equalTo(DEFAULT_PROFILE)));
    assertThat(profiles.get(1),
        is(equalTo(new StringValueProfile("platform", "ios"))));
    assertThat(profiles.get(2),
        is(equalTo(new StringValueProfile("platform", "android"))));
  }

  @Test
  public void testWrongFormatThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new JsonBasedLoader("incorrect.loader"));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_READING_LOADER_CONFIG, "incorrect.loader.json"))));
  }
}
