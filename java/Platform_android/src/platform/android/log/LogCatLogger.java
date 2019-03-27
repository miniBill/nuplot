package platform.android.log;

import platform.log.LogLevel;
import platform.log.MyLogger;
import android.util.Log;

public final class LogCatLogger implements MyLogger{
	private final String appName;

	public LogCatLogger(final String tag){
		appName = tag;
	}

	@Override
	public void println(final LogLevel level, final String msg){
		if(level.equals(LogLevel.DEBUG))
			Log.d(appName, msg);
		if(level.equals(LogLevel.ERROR))
			Log.e(appName, msg);
		if(level.equals(LogLevel.INFO))
			Log.i(appName, msg);
		if(level.equals(LogLevel.WARNING))
			Log.w(appName, msg);
		if(level.equals(LogLevel.ERROR))
			Log.v(appName, msg);
	}
}
