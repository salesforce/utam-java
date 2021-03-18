package utam.core.framework.consumer;

import java.util.Collections;

/**
 * utilities to make package private methods of PageObjectContext
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class Utilities {

    public static PageObjectContext test() {
        return new PageObjectContextImpl(Collections.emptyMap());
    }
}
