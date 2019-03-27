package test;

import static org.junit.Assert.fail;
import platform.log.LogLevel;
import platform.log.MyLogger;

final class FailLog implements MyLogger{
	@Override
	public void println(final LogLevel level, final String string){
		fail(level.toString() + ":" + string);
	}
}