package platform.log;

public class FilterLogger implements MyLogger{
	private final LogLevel level;
	private final MyLogger innerLogger;

	public FilterLogger(final LogLevel level, final MyLogger log){
		this.level = level;
		innerLogger = log;
	}

	public void println(final LogLevel level, final String string){
		if(level.getValue() >= this.level.getValue())
			innerLogger.println(level, string);
	}

}
