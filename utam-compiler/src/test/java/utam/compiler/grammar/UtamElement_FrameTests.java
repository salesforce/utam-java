package utam.compiler.grammar;

import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.framework.consumer.FrameElement;
import utam.core.framework.consumer.UtamError;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamElement.*;

public class UtamElement_FrameTests {

  @Test
  public void testValidFrameElementNode() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamElement.Traversal abstraction = utamElement.getAbstraction();
    assertThat(abstraction.getClass(), is(equalTo(UtamElement.Frame.class)));
    ElementContext elementContext =
        abstraction.testRootTraverse(TestUtilities.getTestTranslationContext());
    assertThat(
        elementContext.getElementMethod().getDeclaration().getName(),
        is(equalTo("getSimpleFrameElement")));
    assertThat(elementContext.getElementMethod().isPublic(), is(true));
    assertThat(elementContext.getType().isSameType(new TypeUtilities.FromClass(FrameElement.class)), is(equalTo(true)));
  }

  @Test
  public void testPrivateFrameElementThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_FRAME_SHOULD_BE_PUBLIC, "simpleFrameElement"))));
  }

  @Test
  public void testFrameElementWithReturnAllSelectorThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\","
            + "    \"returnAll\": true"
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED, "simpleFrameElement"))));
  }

  @Test
  public void testFrameElementWithNoSelectorThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, "simpleFrameElement"))));
  }

  @Test
  public void testFrameElementWithNullableThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"nullable\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo("frame element 'simpleFrameElement': only properties { name,public,selector } are supported")));
  }

  @Test
  public void testFrameElementWithExternalThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"external\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo("frame element 'simpleFrameElement': only properties { name,public,selector } are supported")));
  }

  @Test
  public void testFrameElementWithElementsThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  },"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"elementInFrame\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".inner\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo("frame element 'simpleFrameElement': only properties { name,public,selector } are supported")));
  }

  @Test
  public void testFrameElementWithFilterThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  },"
            + "  \"filter\": {"
            + "    \"apply\": \"getItemText\","
            + "    \"findFirst\": true,"
            + "    \"matcher\": {"
            + "      \"type\": \"stringContains\","
            + "      \"args\": ["
            + "        {"
            + "          \"name\": \"itemText\","
            + "          \"type\": \"string\""
            + "        }"
            + "      ]"
            + "    }"
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo("frame element 'simpleFrameElement': only properties { name,public,selector } are supported")));
  }

  @Test
  public void testFrameElementWithShadowThrows() {
    String json =
        "{"
            + "  \"name\": \"simpleFrameElement\","
            + "  \"type\": \"frame\","
            + "  \"public\": true,"
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  },"
            + "  \"shadow\": {"
            + "    \"elements\": ["
            + "      {"
            + "        \"name\": \"elementInFrame\","
            + "        \"public\": true,"
            + "        \"selector\": {"
            + "          \"css\": \".inner\""
            + "        }"
            + "      }"
            + "    ]"
            + "  }"
            + "}";
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(e.getMessage(), is(equalTo("frame element 'simpleFrameElement': only properties { name,public,selector } are supported")));
  }
}
