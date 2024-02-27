package utam.compiler.translator;

import static utam.core.framework.UtamLogger.info;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Writes manifest of generated files
 *
 * @author james.evans
 * @since 250
 */
public class Manifest {
  private static final String ERR_CREATING_MANIFEST_TEMPLATE = "error creating manifest %s";
  private final List<String> resourcePaths = new ArrayList<>();

  /**
   * Writes the path of a geneated resource to the manifest
   *
   * @param resourcePath the path of the generated resource
   */
  void addWrittenResource(String resourcePath) {
    resourcePaths.add(resourcePath);
  }

  /**
   * Writes the manifest to the specified path
   *
   * @param manifestPath the path to write the manifest to
   */
  void writeToPath(String manifestPath) {
    info("write artifact manifest " + manifestPath);
    try {
      Collections.sort(resourcePaths);
      Writer writer = new FileWriter(manifestPath);
      writeManifest(writer);
    } catch (IOException e) {
      String err = String.format(ERR_CREATING_MANIFEST_TEMPLATE, manifestPath);
      throw new UtamRunnerError(err, e);
    }
  }

  private void writeManifest(Writer writer) throws IOException {
    SimpleModule module = new SimpleModule();
    module.addSerializer(Manifest.class, new Manifest.Serializer());
    ObjectMapper mapper = new ObjectMapper();
    DefaultPrettyPrinter formatter =
        new DefaultPrettyPrinter()
            .withObjectIndenter(new DefaultIndenter("  ", "\n"))
            .withArrayIndenter(new DefaultIndenter("  ", "\n"));
    mapper.registerModule(module).writer(formatter).writeValue(writer, this);
  }

  /**
   * custom serializer to write manifest JSON object as a file
   *
   * @author james.evans
   * @since 250
   */
  private static class Serializer extends JsonSerializer<Manifest> {

    @Override
    public void serialize(
        Manifest manifest, JsonGenerator output, SerializerProvider serializerProvider)
        throws IOException {
      output.writeStartObject();
      output.writeArrayFieldStart("pageObjects");
      for (String resourcePath : manifest.resourcePaths) {
        output.writeObject(resourcePath);
      }
      output.writeEndArray();
      output.writeEndObject();
      output.writeRaw("\n");
    }
  }
}
