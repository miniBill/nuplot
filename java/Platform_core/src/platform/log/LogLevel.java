package platform.log;

public final class LogLevel{
	public static final LogLevel PARSER = new LogLevel(-1);
	public static final LogLevel DEBUG = new LogLevel(0);
	public static final LogLevel INFO = new LogLevel(1);
	public static final LogLevel WARNING = new LogLevel(2);
	public static final LogLevel ERROR = new LogLevel(3);
	public static final LogLevel MAX = new LogLevel(4);
	private final int value;

	private LogLevel(final int value){
		this.value = value;
	}

	public int getValue(){
		return value;
	}

	public String toString(){
		switch(value){
			case -1:
				return "PARSER";
			case 0:
				return "DEBUG";
			case 1:
				return "INFO";
			case 2:
				return "WARNING";
			case 3:
				return "ERROR";
			case 4:
				return "MAX";
			default:
				return "UKNOWN";
		}
	}
}
