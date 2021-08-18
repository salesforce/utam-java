package utam.core.framework.base;

import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.framework.consumer.FrameElement;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;

public class FrameElementTests {

  @Test
  public void testGetElement() {
    MockUtilities mock = new MockUtilities();
    Element mockElement = mock.getElementAdapter();
    FrameElement frame = new FrameElementImpl();
    ((FrameElementImpl)frame).initialize(mock.getFactory(), mockElement);
    assertThat(frame.getFrameElement(), is(sameInstance(mockElement)));
  }
}
