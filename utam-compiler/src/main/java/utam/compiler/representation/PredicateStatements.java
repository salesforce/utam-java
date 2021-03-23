package utam.compiler.representation;

import java.util.ArrayList;
import java.util.List;

/**
 * list of predicate statements inside function argument
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class PredicateStatements {

  private final List<ComposeMethodStatement> statements = new ArrayList<>();

  public void addStatement(ComposeMethodStatement statement) {
    this.statements.add(statement);
  }
}
