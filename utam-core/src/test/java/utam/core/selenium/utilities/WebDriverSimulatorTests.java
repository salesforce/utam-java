/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.utilities;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.utilities.WebDriverSimulator.ELEMENT_ALREADY_EXISTS;
import static utam.core.selenium.utilities.WebDriverSimulator.ELEMENT_SELECTOR_NOT_SET;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

public class WebDriverSimulatorTests {

  @Test
  public void testGetDriver() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    assertThat(simulator.getDriver(), is(not(nullValue())));
  }

  @Test
  public void testGetDriverWithRepeatedCall() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    WebDriver driver = simulator.getDriver();
    assertThat(driver, is(sameInstance(simulator.getDriver())));
  }

  @Test
  public void testRegisterElement() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    WebDriverSimulator.WebElementInfo info =
        simulator.registerElement("testElement", ".fakeSelector");
    assertThat(info.getName(), is(equalTo("testElement")));
    assertThat(info.getSelector(), is(equalTo(".fakeSelector")));
    assertThat(info.isInShadowDOM(), is(equalTo(false)));
    assertThat(info.getParentElementName(), is(nullValue()));
    assertThat(info.getElement(), is(not(nullValue())));
  }

  @Test
  public void testRegisterElementWithOnlySelector() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    WebDriverSimulator.WebElementInfo info = simulator.registerElement(".fakeSelector");
    assertThat(info.getName(), is(not(emptyString())));
    assertThat(info.getSelector(), is(equalTo(".fakeSelector")));
  }

  @Test
  public void testRegisterElementWithNullNameThrows() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    UnsupportedOperationException e =
        expectThrows(
            UnsupportedOperationException.class,
            () -> simulator.registerElement(null, ".fakeSelector"));
    assertThat(e.getMessage(), is(equalTo("element name may not be null or the empty string")));
  }

  @Test
  public void testRegisterElementWithEmptyNameThrows() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    UnsupportedOperationException e =
        expectThrows(
            UnsupportedOperationException.class,
            () -> simulator.registerElement("", ".fakeSelector"));
    assertThat(e.getMessage(), is(equalTo("element name may not be null or the empty string")));
  }

  @Test
  public void testRegisterElementWithNullSelectorThrows() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    final String ELEMENT_NAME = "testElement";
    UnsupportedOperationException e =
        expectThrows(
            UnsupportedOperationException.class,
            () -> simulator.registerElement(ELEMENT_NAME, null));
    assertThat(e.getMessage(), is(equalTo(String.format(ELEMENT_SELECTOR_NOT_SET, ELEMENT_NAME))));
  }

  @Test
  public void testRegisterElementWithEmptySelectorThrows() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    final String ELEMENT_NAME = "testElement";
    UnsupportedOperationException e =
        expectThrows(
            UnsupportedOperationException.class, () -> simulator.registerElement(ELEMENT_NAME, ""));
    assertThat(e.getMessage(), is(equalTo(String.format(ELEMENT_SELECTOR_NOT_SET, ELEMENT_NAME))));
  }

  @Test
  public void testRegisterElementWithDuplicateNameThrows() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    final String ELEMENT_NAME = "testElement";
    simulator.registerElement(ELEMENT_NAME, ".fakeSelector");
    UnsupportedOperationException e =
        expectThrows(
            UnsupportedOperationException.class,
            () -> simulator.registerElement(ELEMENT_NAME, ".newFakeSelector"));
    assertThat(e.getMessage(), containsString(String.format(ELEMENT_ALREADY_EXISTS, ELEMENT_NAME)));
  }

  @Test
  public void testWithChild() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    WebDriverSimulator.WebElementInfo info =
        simulator.registerElement("testElement", ".fakeSelector");
    WebDriverSimulator.WebElementInfo shadowInfo =
        simulator.registerElement("testShadowElement", ".fakeShadowSelector");
    simulator
        .registerElement("parentElement", ".fakeParentSelector")
        .withChild(info)
        .withChildInShadowDOM(shadowInfo);
    simulator.getDriver();
    assertThat(info.getParentElementName(), is(equalTo("parentElement")));
    assertThat(info.isInShadowDOM(), is(equalTo(false)));
  }

  @Test
  public void testWithChildInShadowDOM() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    WebDriverSimulator.WebElementInfo info =
        simulator.registerElement("testElement", ".fakeSelector");
    WebDriverSimulator.WebElementInfo shadowInfo =
        simulator.registerElement("testShadowElement", ".fakeShadowSelector");
    simulator
        .registerElement("parentElement", ".fakeParentSelector")
        .withChild(info)
        .withChildInShadowDOM(shadowInfo);
    assertThat(shadowInfo.getParentElementName(), is(equalTo("parentElement")));
    assertThat(shadowInfo.isInShadowDOM(), is(equalTo(true)));
  }

  @Test
  public void testWithAttributeValue() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    simulator
        .registerElement("testElement", ".fakeSelector")
        .withAttributeValue("testAttributeName", "testAttributeValue");
    WebDriver driver = simulator.getDriver();
    WebElement element = driver.findElements(By.cssSelector(".fakeSelector")).get(0);
    assertThat(element.getAttribute("testAttributeName"), is(equalTo("testAttributeValue")));
  }

  @Test
  public void testWithText() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    simulator.registerElement("testElement", ".fakeSelector").withText("testText");
    WebDriver driver = simulator.getDriver();
    WebElement element = driver.findElements(By.cssSelector(".fakeSelector")).get(0);
    assertThat(element.getText(), is(equalTo("testText")));
  }

  @Test
  public void testWithVisibility() {
    WebDriverSimulator simulator = new WebDriverSimulator(TestObjectFactory.class);
    simulator.registerElement("testElement", ".fakeSelector").withVisibility(true);
    WebDriver driver = simulator.getDriver();
    WebElement element = driver.findElements(By.cssSelector(".fakeSelector")).get(0);
    assertThat(element.isDisplayed(), is(equalTo(true)));
  }

  @Test
  public void testNoPublicConstructorThrows() {
    assertThat(PrivateConstructorFactory.getFactory(), is(not(nullValue())));
    UtamError e =
        expectThrows(
            UtamError.class, () -> new WebDriverSimulator(PrivateConstructorFactory.class));
    assertThat(e.getMessage(), containsString("does not have a public parameterless constructor"));
  }

  @Test
  public void testConstructorThrowingExceptionThrows() {
    UtamError e =
        expectThrows(
            UtamError.class, () -> new WebDriverSimulator(ThrowingConstructorFactory.class));
    assertThat(
        e.getMessage(), containsString("unexpected error executing constructor to factory class"));
  }
}
