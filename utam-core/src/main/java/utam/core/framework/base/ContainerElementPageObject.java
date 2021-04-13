/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.framework.consumer.ContainerElement;

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
