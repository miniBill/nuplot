package platform.javax;

import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Display;

import platform.log.LogLevel;
import platform.log.MyLogger;

public class AlertLogger implements MyLogger{
	private static Display display;

	public static void setDisplay(final Display value){
		display = value;
	}

	public void println(final LogLevel level, final String string){
		if(display == null)
			return;
		final Alert alert = new Alert(level.toString(), string, null,
				AlertType.INFO);
		alert.setTimeout(Alert.FOREVER);
		display.setCurrent(alert);
	}

}
