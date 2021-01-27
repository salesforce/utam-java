package declarative.representation;

import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

/**
 * Tests the default implementation and static members of the TypeProvider interface
 *
 * @author james.evans
 */
public class TypeProviderTests {

  /**
   * Test that the EMPTY_LIST member of the TypeProvider interface returns an empty list of
   * TypeProvider objects
   */
  @Test
  public void testEmptyListMemberReturnsEmptyList() {
    List<TypeProvider> providers = TypeProvider.EMPTY_LIST;
    assertThat(providers, is(equalTo(new ArrayList<TypeProvider>())));
  }
}
