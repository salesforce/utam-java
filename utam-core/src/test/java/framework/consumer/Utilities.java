package framework.consumer;

import java.util.Collections;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class Utilities {

    public static PageObjectContext test() {
        return new PageObjectContextImpl(Collections.emptyMap(), Collections.emptyMap());
    }
}
