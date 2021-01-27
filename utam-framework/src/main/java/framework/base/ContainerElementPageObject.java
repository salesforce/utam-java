package framework.base;

import framework.consumer.ContainerElement;

/**
 * Page Object that acts as a wrapper for a ContainerElement.
 */
public class ContainerElementPageObject implements PageObject {
	private final ContainerElement container;

	/**
	 * Initializes a new instance of the ContainerElementPageObject class
	 * @param container the ContainerElement used for scope in integration with external Page Objects
	 */
	public ContainerElementPageObject(ContainerElement container) {
		this.container = container;
	}

	/**
	 * Gets the ContainerElement instance
	 * @return the ContainerElement used for scope in integration with external Page Objects
	 */
	public ContainerElement getContainerElement() {
		return container;
	}
}
