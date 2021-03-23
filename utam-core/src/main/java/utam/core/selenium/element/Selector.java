package utam.core.selenium.element;

import org.openqa.selenium.By;
import utam.core.appium.element.UIAutomator;
import utam.core.selenium.element.Web.SelectorImpl;

/**
 * combines selector string with its type into one object used to generate selector and annotation
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface Selector {

    /**
     * get String value of the selector
     *
     * @return selector string
     */
    String getValue();

    /**
     * get selector type
     *
     * @return one of the supported types
     */
    Type getType();

    /**
     * get selenium friendly element locator
     * @return By
     */
    By by();

    enum Type {
        CSS,
        ACCESSID,
        CLASSCHAIN,
        UIAUTOMATOR
    }

    static Selector byCss(String value) {
        return new SelectorImpl(Type.CSS, value);
    }

    static Selector byAccessibilityId(String value) {
        return new SelectorImpl(Selector.Type.ACCESSID, value);
    }

    static Selector byClassChain(String value) {
        return new SelectorImpl(Selector.Type.CLASSCHAIN, value);
    }

    static Selector byUiAutomator(String value) {
        return new SelectorImpl(Selector.Type.UIAUTOMATOR, UIAutomator.getSelectorWithPrefix(value));
    }
}
