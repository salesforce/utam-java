package utam.compiler.representation;


import utam.core.framework.base.PageObject;

public class UtamUtilitiesContext {
    private final PageObject pageObject;

    public UtamUtilitiesContext(PageObject pageObjectInstance) {
        this.pageObject = pageObjectInstance;
    }

    public PageObject getPageObject() {
        return pageObject;
    }
}
