package utam.core.selenium.element;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;

public class Rect {

    private int xPosition;
    private int yPosition;
    private int rectWidth;
    private int rectHeight;

    public Rect(Point windowPoint, Dimension windowSize) {
        this.xPosition = windowPoint.getX();
        this.yPosition = windowPoint.getY();
        this.rectWidth = windowSize.getWidth();
        this.rectHeight = windowSize.getHeight();
    }

    public int getX() {
        return this.xPosition;
    }

    public int getY() {
        return this.yPosition;
    }

    public int getWidth() {
        return this.rectWidth;
    }

    public int getHeight() {
        return this.rectHeight;
    }
}
