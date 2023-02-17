package utam.core.selenium.element;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;

public class Rect {

    private final Dimension windowSize;
    private Point windowPoint;

    public Rect(Point windowPoint, Dimension windowSize) {
        this.windowPoint = windowPoint;
        this.windowSize = windowSize;
    }

    public Point getPoint() {
        return this.windowPoint;
    }

    public Dimension getDimension() {
        return this.windowSize;
    }
}
