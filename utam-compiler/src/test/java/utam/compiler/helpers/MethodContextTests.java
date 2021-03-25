package utam.compiler.helpers;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public class MethodContextTests {

  @Test
  public void testIncorrectReturnTypeThrows() {
    MethodContext methodContext = new MethodContext("name", VOID, false);
    ComposeMethodStatement statement = mock(ComposeMethodStatement.class);
    when(statement.getReturnType()).thenReturn(BOOLEAN);
    assertThrows(() -> methodContext.getReturnType(Collections.singletonList(statement), null));
  }

  @Test
  public void testListOfVoidThrows() {
    assertThrows(() -> new MethodContext("name", VOID, true));
  }

  @Test
  public void testReturnList() {
    MethodContext methodContext = new MethodContext("name", STRING, true);
    assertThat(methodContext.getReturnType(null), is(instanceOf(ListOf.class)));
    List<TypeProvider> imports = methodContext.getReturnTypeImports(Collections.EMPTY_LIST);
    assertThat(imports.size(), is(2));
    assertThat(imports.get(0).isSameType(LIST_IMPORT), is(true));
    assertThat(imports.get(1).isSameType(STRING), is(true));
  }
}
