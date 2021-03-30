package utam.core.framework.consumer;

import java.util.function.Supplier;
import org.openqa.selenium.SearchContext;

/**
 * external PO scoped inside UTAM, only supported for Selenium!
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Contained {

  void setRoot(Supplier<SearchContext> rootSupplier);

  void setScope(Supplier<SearchContext> scopeSupplier);
}
