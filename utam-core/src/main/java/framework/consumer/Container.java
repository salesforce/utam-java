package framework.consumer;

import org.openqa.selenium.SearchContext;

import java.util.function.Supplier;

/**
 * can act as UTAM PO container
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Container {

    Supplier<SearchContext> getScope();
}
