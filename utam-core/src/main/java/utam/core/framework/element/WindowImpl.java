/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.Window;
import utam.core.selenium.element.Rect;

/**
 * Window implementation
 *
 * @author william.sandy
 */
public class WindowImpl implements Window {

    private final Driver driverAdapter;
    private final String windowHandle;

    public WindowImpl(Driver driverAdapter) {
        this.driverAdapter = driverAdapter;
        this.windowHandle = driverAdapter.getWindowHandle();
    }

    @Override
    public Rect getRect() { return this.driverAdapter.getRect(); }

    @Override
    public void setRect(Rect rect) { this.driverAdapter.setRect(rect); }

    @Override
    public void close() {
        String currentWindowHandle = this.driverAdapter.getWindowHandle();

        if (currentWindowHandle.equals(this.windowHandle)) {
            this.driverAdapter.close();
        } else {
            this.driverAdapter.switchTo().window(this.windowHandle);
            this.driverAdapter.close();
            this.driverAdapter.switchTo().window(currentWindowHandle);
        }
    }

    @Override
    public Document getDocument() {
        //TODO return relevant document, Jim should reply on slack to tell us what he wants this to do
        return null;
    }
}