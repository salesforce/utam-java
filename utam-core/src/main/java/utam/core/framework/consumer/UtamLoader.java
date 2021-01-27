package utam.core.framework.consumer;

import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.selenium.element.Selector;

/**
 * loader to instantiate and load UTAM POs based on consumer configuration
 * @author elizaveta.ivanova
 * @since 230
 */
public interface UtamLoader {

    /**
     * creates instance of the Root Page Object inside the browser
     * @param type type of the object
     * @return instance of the Page Object, not loaded
     */
    <T extends RootPageObject> T create(Class<T> type);

    /**
     * same as create method, but also checks if PO is actually present, otherwise throws exception
     * @param type type of the object
     * @return instance of the Page Object after we checked that it's loaded
     */
    <T extends RootPageObject> T load(Class<T> type);

    /**
     * creates instance of the UTAM Page Object inside external parent
     *
     * @param parent external parent
     * @param type type of the object
     * @param injectedSelector injected selector
     * @return instance of the Page Object, not loaded
     */
    <T extends PageObject> T create(
            Container parent,
            Class<T> type,
            Selector injectedSelector);
}
