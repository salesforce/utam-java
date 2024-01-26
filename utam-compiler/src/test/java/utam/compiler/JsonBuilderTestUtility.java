/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import static org.testng.Assert.expectThrows;

import org.testng.Assert.ThrowingRunnable;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.grammar.DeserializerUtilities.Result;

/**
 * test json parsing from a string
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonBuilderTestUtility {

  private String json = "{";

  private String getJsonString() {
    if (!json.endsWith("}")) {
      json = json.concat("}");
    }
    return json;
  }

  public JsonBuilderTestUtility addString(String propertyName, String value) {
    if (json.length() > 1) {
      json = json.concat(", ");
    }
    json = json.concat(String.format("\"%s\" : \"%s\"", propertyName, value));
    return this;
  }

  public JsonBuilderTestUtility addRawString(String propertyName, String value) {
    if (json.length() > 1) {
      json = json.concat(", ");
    }
    json = json.concat(String.format("\"%s\" : %s", propertyName, value));
    return this;
  }

  public Result getDeserializedJson() {
    return new DeserializerUtilities().getResultFromString(getJsonString());
  }

  private ThrowingRunnable throwError() {
    return () -> new DeserializerUtilities().getResultFromString(getJsonString());
  }

  public UtamCompilationError expectCompilerError() {
    return expectThrows(UtamCompilationError.class, throwError());
  }
}
