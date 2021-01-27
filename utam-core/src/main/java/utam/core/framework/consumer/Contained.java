package utam.core.framework.consumer;

import org.openqa.selenium.SearchContext;

import java.util.function.Supplier;

/**
 * external PO scoped inside UTAM
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Contained {

    void setRoot(Supplier<SearchContext> rootSupplier);

    void setScope(Supplier<SearchContext> scopeSupplier);
}
