package declarative.representation;

import java.util.List;

import static declarative.helpers.AnnotationUtils.EMPTY_ANNOTATIONS;
import static declarative.translator.TranslationUtilities.EMPTY_COMMENTS;

/**
 * representation of the page object element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public final class UtilityReferenceField implements PageClassField {

  private final String name;
  private final TypeProvider type;

  public UtilityReferenceField(TypeProvider type) {
    this.name = "util" + type.getSimpleName();
    this.type = type;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public List<AnnotationProvider> getAnnotations() {
    return EMPTY_ANNOTATIONS;
  }

  @Override
  public TypeProvider getType() {
    return type;
  }

  @Override
  public String getDeclaration() {
    String typeName = getType().getSimpleName();
    return String.format("private final %s %s = getUtility(%s.class)", typeName, getName(), typeName);
  }

  @Override
  public String getComments() {
    return EMPTY_COMMENTS;
  }
}
