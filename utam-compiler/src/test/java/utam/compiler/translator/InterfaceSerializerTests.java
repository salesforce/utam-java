package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;

import org.testng.annotations.Test;
import utam.core.declarative.representation.PageObjectInterface;

public class InterfaceSerializerTests {

  private static String getImplementationCode(String json) {
    PageObjectInterface pageObject = getJsonStringDeserializer(json).getObject().getInterface();
    return new InterfaceSerializer(pageObject).toString();
  }

  @Test
  public void testToString() {
    String json =
        "{"
            + "  \"beforeLoad\": ["
            + "    {"
            + "      \"apply\": \"waitFor\","
            + "      \"args\": ["
            + "        {"
            + "          \"type\": \"function\","
            + "          \"predicate\": ["
            + "            {"
            + "              \"element\": \"root\","
            + "              \"apply\": \"containsElement\","
            + "              \"args\": ["
            + "                {"
            + "                  \"value\": {"
            + "                    \"css\": \".fakeSelector\""
            + "                  },"
            + "                  \"type\": \"locator\""
            + "                },"
            + "                {"
            + "                  \"value\": true"
            + "                }"
            + "              ]"
            + "            }"
            + "          ]"
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
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
            + "      \"type\": [\"clickable\"],"
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    },"
            + "    {"
            + "      \"name\": \"testTextBox\","
            + "      \"type\": [\"editable\"],"
            + "      \"selector\": {"
            + "        \"css\": \".fakeTextSelector\""
            + "      }"
            + "    },"
            + "    {"
            + "      \"name\": \"testFlickableMobileElement\","
            + "      \"type\": [\"touchable\"],"
            + "      \"selector\": {"
            + "        \"css\": \".fakeTextSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    String code = getImplementationCode(json);
    assertThat(code, containsString("package utam.test.pageobjects.test;"));
    assertThat(code, containsString("import utam.core.framework.base.PageObject;"));
    assertThat(code, containsString("import utam.core.element.Editable;"));
    assertThat(code, containsString("import utam.core.element.Clickable;"));
    assertThat(code, containsString("public interface Test extends PageObject"));
    assertThat(code, containsString("TestButtonElement getTestButton();"));
    assertThat(code, containsString("void submitInfo(String text)"));
    assertThat(code, not(containsString("Object load();")));
  }
}
