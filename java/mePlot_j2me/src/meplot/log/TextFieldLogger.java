package meplot.log;

import java.util.Calendar;
import java.util.Date;

import javax.microedition.lcdui.TextField;

import platform.log.LogLevel;
import platform.log.MyLogger;

public final class TextFieldLogger implements MyLogger{
	private final TextField field;

	public TextFieldLogger(final TextField field){
		this.field = field;
	}

	private final Calendar cal = Calendar.getInstance();

	public void println(final LogLevel level, final String string){
		final String previous = field.getString();
		cal.setTime(new Date());
		final int min = cal.get(Calendar.MINUTE);
		final int sec = cal.get(Calendar.SECOND);
		final int milli = cal.get(Calendar.MILLISECOND) / 10;
		final String time = (min < 10 ? "0" : "") + min + ':'
				+ (sec < 10 ? "0" : "") + sec + ',' + (milli < 10 ? "0" : "")
				+ milli;
		String toset = previous + "\n[" + time + ']'
				+ level.toString().charAt(0) + ':' + string;
		if(toset.length() >= field.getMaxSize() - 1)
			toset = toset.substring(0, field.getMaxSize() - 10);
		field.setString(toset);
	}
}
