package utam.compiler.translator;

import utam.compiler.helpers.PrimitiveType;
import declarative.representation.MethodDeclaration;
import declarative.representation.PageObjectClass;
import declarative.representation.PageObjectMethod;
import org.testng.annotations.Test;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.translator.TranslationUtilities.getLastStatement;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ClassSerializerTests {

  private static String getImplementationCode(String json) {
    PageObjectClass pageObject = getJsonStringDeserializer(json).getObject().getImplementation();
    return new ClassSerializer(pageObject, getTestTranslationContext()).toString();
  }

  @Test
  public void testToString() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"submitInfo\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testTextBox\","
            + "          \"apply\": \"setText\","
            + "          \"args\": ["
            + "            {"
            + "              \"name\": \"text\","
            + "              \"type\": \"string\""
            + "            }"
            + "          ]"
            + "        },"
            + "        {"
            + "          \"element\": \"testButton\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testButton\","
            + "      \"type\": \"clickable\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    },"
            + "    {"
            + "      \"name\": \"testTextBox\","
            + "      \"type\": \"editable\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeTextSelector\""
            + "      }"
            + "    },"
            + "    {"
            + "      \"name\": \"testFlickableMobileElement\","
            + "      \"type\": \"touchable\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeTextSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    String code = getImplementationCode(json);
    assertThat(code, containsString("package utam.test.pageobjects.test.impl;"));
    assertThat(code, containsString("import framework.base.BasePageObject;"));
    assertThat(code, containsString("import selenium.element.Clickable;"));
    assertThat(code, containsString("import selenium.element.Editable;"));
    assertThat(code, containsString("import selenium.element.Touchable;"));
    assertThat(code, containsString("import selenium.element.ElementMarker;"));
    assertThat(code, containsString("import utam.test.pageobjects.test.Test;"));
    assertThat(
        code, containsString("public final class TestImpl extends BasePageObject implements Test"));
    assertThat(code, containsString("private Clickable testButton;"));
    assertThat(code, containsString("private Editable testTextBox;"));
    assertThat(code, containsString("private Touchable testFlickableMobileElement;"));
    assertThat(code, containsString("public final Clickable getTestButton()"));
    assertThat(code, containsString("public final void submitInfo(String text)"));
  }

  @Test
  void getLastStatementTest() {
    PageObjectMethod method = mock(PageObjectMethod.class);
    when(method.getCodeLines()).thenReturn(Stream.of("returnValue").collect(Collectors.toList()));
    MethodDeclaration methodDeclaration = mock(MethodDeclaration.class);
    when(method.getDeclaration()).thenReturn(methodDeclaration);
    when(methodDeclaration.getReturnType()).thenReturn(PrimitiveType.VOID);
    assertThat(getLastStatement(method), is(equalTo("returnValue;")));
  }
}
