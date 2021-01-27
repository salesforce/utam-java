package selenium.utilities;

class PrivateConstructorFactory extends TestObjectFactory {
  private PrivateConstructorFactory() {
  }
  
  static TestObjectFactory getFactory() {
    return new PrivateConstructorFactory();
  }
}
