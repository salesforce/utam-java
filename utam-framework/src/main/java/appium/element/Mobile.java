package appium.element;

import selenium.element.Selector;
import selenium.element.Web;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class Mobile extends Web {

    public static Selector byAccessibilityId(String value) {
        return new SelectorImpl(Selector.Type.ACCESSID, value);
    }

    public static Selector byClassChain(String value) {
        return new SelectorImpl(Selector.Type.CLASSCHAIN, value);
    }

    public static Selector byUiAutomator(String value) {
        return new SelectorImpl(Selector.Type.UIAUTOMATOR, UIAutomator.getSelectorWithPrefix(value));
    }
}
