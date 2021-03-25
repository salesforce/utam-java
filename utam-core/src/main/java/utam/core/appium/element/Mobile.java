package utam.core.appium.element;

import utam.core.selenium.element.Selector;
import utam.core.selenium.element.Web;

/**
 * @deprecated functionality moved to Selector interface
 *
 * @author elizaveta.ivanova
 * @since 230
 */
@Deprecated
public abstract class Mobile extends Web {

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
