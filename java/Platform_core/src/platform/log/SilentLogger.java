package platform.log;


public final class SilentLogger implements MyLogger{
	public void println(final LogLevel level, final String string){
		// Do nothing.
	}
}
