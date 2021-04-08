/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

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
