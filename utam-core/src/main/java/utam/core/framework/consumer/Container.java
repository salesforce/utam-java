package utam.core.framework.consumer;

import java.util.function.Supplier;
import org.openqa.selenium.SearchContext;

/**
 * can act as UTAM PO container, only supported for Selenium!
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Container {

  Supplier<SearchContext> getScope();
}
