package selenium.element;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * container for annotations
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class ElementMarker {

    @Target({ElementType.FIELD})
    @Retention(RetentionPolicy.RUNTIME)
    public @interface Find {

        String css() default "";

        String accessid() default "";

        String classchain() default "";

        String uiautomator() default "";

        String scope() default "";

        // defines if parent scope shadow root should be expanded
        boolean expand() default false;
    }
}
