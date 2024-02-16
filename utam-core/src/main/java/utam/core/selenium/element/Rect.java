package utam.core.selenium.element;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;

/**
 * Rect wrapper A wrapper around Selenium's Rect (rectangle) object
 *
 * @author william.sandy
 */
public class Rect {

  private final int xPosition;
  private final int yPosition;
  private final int rectWidth;
  private final int rectHeight;

  public Rect(Point windowPoint, Dimension windowSize) {
    this.xPosition = windowPoint.getX();
    this.yPosition = windowPoint.getY();
    this.rectWidth = windowSize.getWidth();
    this.rectHeight = windowSize.getHeight();
  }

  /**
   * Gets the x position of this Rect
   *
   * @return the x coordinate of this Rect
   */
  public int getX() {
    return this.xPosition;
  }

  /**
   * Gets the y position of this Rect
   *
   * @return the y coordinate of this Rect
   */
  public int getY() {
    return this.yPosition;
  }

  /**
   * Gets the width position of this Rect
   *
   * @return the width coordinate of this Rect
   */
  public int getWidth() {
    return this.rectWidth;
  }

  /**
   * Gets the height position of this Rect
   *
   * @return the height coordinate of this Rect
   */
  public int getHeight() {
    return this.rectHeight;
  }
}
