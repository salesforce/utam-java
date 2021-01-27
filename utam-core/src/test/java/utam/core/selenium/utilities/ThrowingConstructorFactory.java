package utam.core.selenium.utilities;

public class ThrowingConstructorFactory extends TestObjectFactory {
  public ThrowingConstructorFactory() {
    throw new RuntimeException("constructor exception");
  }
}
