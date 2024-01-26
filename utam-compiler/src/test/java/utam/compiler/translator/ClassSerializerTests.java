/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.base.ElementLocation;

public class ClassSerializerTests {

  private static String getImplementationCode(String json) {
    PageObjectClass pageObject = getJsonStringDeserializer(json).getObject().getImplementation();
    return new ClassSerializer(pageObject).toString();
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
    assertThat(code, containsString("package utam.test.pageobjects.test.impl;"));
    assertThat(code, containsString("import utam.core.framework.base.BasePageObject;"));
    assertThat(code, containsString(String.format("import %s;", ElementLocation.class.getName())));
    assertThat(code, containsString("import utam.core.framework.base.ElementMarker;"));
    assertThat(code, containsString("import utam.test.pageobjects.test.Test;"));
    assertThat(
        code, containsString("public final class TestImpl extends BasePageObject implements Test"));
    assertThat(code, containsString("private ElementLocation testButton;"));
    assertThat(code, containsString("private ElementLocation testTextBox;"));
    assertThat(code, containsString("private ElementLocation testFlickableMobileElement;"));
    assertThat(code, containsString("public final TestButtonElement getTestButton()"));
    assertThat(code, containsString("public final void submitInfo(String text)"));
  }

  @Test
  void getLastStatementTest() {
    PageObjectMethod method = mock(PageObjectMethod.class);
    when(method.getCodeLines()).thenReturn(Stream.of("returnValue").collect(Collectors.toList()));
    MethodDeclaration methodDeclaration = mock(MethodDeclaration.class);
    when(method.getDeclaration()).thenReturn(methodDeclaration);
    when(methodDeclaration.getReturnType()).thenReturn(VOID);
  }
}
