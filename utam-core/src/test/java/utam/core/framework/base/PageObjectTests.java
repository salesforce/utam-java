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
