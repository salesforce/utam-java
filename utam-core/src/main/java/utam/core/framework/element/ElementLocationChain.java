package utam.core.framework.element;

import static utam.core.selenium.element.ElementAdapter.NULL_ELEMENT;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Stream;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;

/**
 * chain of locators for a page element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementLocationChain implements ElementLocation {

  private final Selector[] chain;
  private final boolean isNullable;
  private String chainString;

  private ElementLocationChain(Selector[] chain, boolean isNullable) {
    this.chain = chain;
    this.isNullable = isNullable;
  }

  public ElementLocationChain(Locator locator, FindContext finderContext) {
    this(new Selector[]{new Selector(locator, finderContext)}, finderContext.isNullable());
  }

  public ElementLocationChain(Element found) {
    this(new Selector[]{new Instance(found)}, found.isNull());
  }

  @Override
  public List<Element> findElements(Driver driver) {
    if (chain.length == 1) {
      return chain[0].findElementsInsideDriver(driver);
    }
    Element current = chain[0].findElementInsideDriver(driver);
    for (int i = 1; i < chain.length - 1; i++) {
      current = chain[i].findElementInsideElement(current);
    }
    return chain[chain.length - 1].findElementsInsideElement(current);
  }

  @Override
  public Element findElement(Driver driver) {
    Element current = chain[0].findElementInsideDriver(driver);
    for (int i = 1; i < chain.length; i++) {
      current = chain[i].findElementInsideElement(current);
    }
    return current;
  }

  @Override
  public String getLocatorChainString() {
    if (chainString != null) {
      return chainString;
    }
    List<String> str = new ArrayList<>();
    str.add("driver");
    Stream.of(chain).forEach(link -> str.add(link.getString()));
    chainString = String.join("", str);
    return chainString;
  }

  @Override
  public ElementLocation scope(Locator locator, FindContext finderContext) {
    Selector[] links = Stream.of(chain).map(Selector::copy)
        .toArray(Selector[]::new);
    Selector[] copy = Arrays.copyOf(links, chain.length + 1);
    copy[chain.length] = new Selector(locator, finderContext);
    return new ElementLocationChain(copy, finderContext.isNullable());
  }

  @Override
  public ElementLocation setParameters(Object... parameters) {
    if (parameters == null || parameters.length == 0) {
      return this;
    }
    int index = 0;
    Selector[] withParameters = new Selector[chain.length];
    for (int i = 0; i < chain.length; i++) {
      Entry<Integer, Selector> updated = chain[i].setParameters(index, parameters);
      index += updated.getKey();
      withParameters[i] = updated.getValue();
    }
    return new ElementLocationChain(withParameters, isNullable);
  }

  @Override
  public boolean isNullable() {
    return isNullable;
  }

  static class Selector {

    final Locator locator;
    final FindContext findContext;

    Selector(Locator locator, FindContext findContext) {
      this.locator = locator;
      this.findContext = findContext;
    }

    String getString() {
      return String.format(" %s %s", findContext.getSeparator(), locator.getValue().toString());
    }

    Entry<Integer, Selector> setParameters(int index, Object... values) {
      if (values == null || values.length == 0) {
        return new SimpleEntry<>(index, this);
      }
      Entry<Integer, Locator> withParameters = this.locator.setParameters(index, values);
      return new SimpleEntry<>(withParameters.getKey(),
          new Selector(withParameters.getValue(), findContext));
    }

    Element findElementInsideDriver(Driver driver) {
      return driver.findElement(locator, findContext);
    }

    Element findElementInsideElement(Element element) {
      return element.findElement(locator, findContext);
    }

    List<Element> findElementsInsideDriver(Driver driver) {
      return driver.findElements(locator, findContext);
    }

    List<Element> findElementsInsideElement(Element element) {
      return element.findElements(locator, findContext);
    }

    Selector copy() {
      return new Selector(locator.getCopy(), findContext);
    }
  }

  static class Instance extends Selector {

    static final String FOUND_ELEMENT_NULL = " > null";
    static final String FOUND_ELEMENT = " > element";
    final List<Element> elements;

    Instance(Element element) {
      super(null, null);
      this.elements =
          element.isNull() ? Collections.EMPTY_LIST : Collections.singletonList(element);
    }

    @Override
    String getString() {
      if (elements.isEmpty()) {
        return FOUND_ELEMENT_NULL;
      }
      // replace hash of mock
      String elementStr = elements.get(0).toString();
      return elementStr.startsWith("Mock") ? FOUND_ELEMENT : elementStr;
    }

    @Override
    Entry<Integer, Selector> setParameters(int index, Object... values) {
      return new SimpleEntry<>(index, this);
    }

    @Override
    Element findElementInsideDriver(Driver driver) {
      return elements.isEmpty() ? NULL_ELEMENT : elements.get(0);
    }

    @Override
    Element findElementInsideElement(Element element) {
      return elements.isEmpty() ? NULL_ELEMENT : elements.get(0);
    }

    @Override
    List<Element> findElementsInsideDriver(Driver driver) {
      return elements;
    }

    @Override
    List<Element> findElementsInsideElement(Element element) {
      return elements;
    }

    @Override
    Instance copy() {
      return this;
    }
  }
}
