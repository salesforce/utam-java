package framework.consumer;

/**
 * types of elements location in runtime
 * @author elizaveta.ivanova
 * @since 230
 */
public enum LocationPolicyType implements LocationPolicy {

    CHAIN, JAVASCRIPT;

    public static LocationPolicy getDefault() {
        return CHAIN;
    }
}
