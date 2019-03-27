package platform.log;

public class ConsoleLogger implements MyLogger{
	public void println(final LogLevel level, final String string){
		System.out.print(level.toString() + ':' + string);
	}
}
