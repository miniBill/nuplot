package platform.android.log;

import platform.log.LogLevel;
import platform.log.MyLogger;
import android.content.Context;
import android.widget.Toast;

public final class ToasterLogger implements MyLogger{
	private final Context context;

	public ToasterLogger(final Context context){
		this.context = context;
	}

	@Override
	public void println(final LogLevel level, final String string){
		Toast.makeText(context, level.toString() + ':' + string,
				Toast.LENGTH_LONG);
	}
}
