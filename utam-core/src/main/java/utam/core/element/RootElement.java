/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * Temporary interface that extends all existing element action interfaces. This will be removed
 * and replaced once the interface inheritance hierarchy is resolved.
 *
 * @since 234
 */
public interface RootElement extends Actionable, Clickable, Editable, Touchable, Draggable {
}
