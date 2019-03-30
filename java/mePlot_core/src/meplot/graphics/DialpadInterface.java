package meplot.graphics;

import platform.log.Log;
import platform.log.LogLevel;

public final class DialpadInterface {
	public static final int FREE_ROAM = 12;
	public static final int ZOOM = 5;
	public static final int UP = 2;
	public static final int LEFT = 4;
	public static final int RIGHT = 6;
	public static final int DOWN = 8;
	public static final int DEZOOM = 11;
	public static final int NEXT_MODE = 3;
	public static final int RESET = 10;

	private DialpadInterface() {
	}

	public static boolean input(final int num, final DrawController controller) {
		switch (num) {
		case ZOOM:
			controller.zoomIn();
			break;

		case DEZOOM:
			controller.zoomOut();
			break;

		case UP:
			if (controller.isFreeroam())
				controller.rotateUp();
			else
				controller.goUp();
			break;

		case LEFT:
			if (controller.isFreeroam())
				controller.rotateLeft();
			else
				controller.goLeft();
			break;

		case RIGHT:
			if (controller.isFreeroam())
				controller.rotateRight();
			else
				controller.goRight();
			break;

		case DOWN:
			if (controller.isFreeroam())
				controller.rotateDown();
			else
				controller.goDown();
			break;

		case RESET:
			controller.resetPosition();
			break;

		case 12:
			controller.toggleFreeRoam();
			break;

		case NEXT_MODE:
			controller.next3DMode();
			break;

		case 1:
			controller.previous3DMode();
			break;

		// ESCA-JAVA0040:
		case 9:
			break;

		case 7: // Do nothing
		case 100: // Back
			return false;

		default:
			Log.log(LogLevel.WARNING, "Unrecognized key was pressed: " + num);
			return false;

		}
		return true;
	}
}
