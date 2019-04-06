package platform.memory;

import platform.log.LogLevel;
import platform.log.MyLogger;

/**
 * BEWARE: DANGEROUS.
 * TENDS TO USE A LOT OF MEMORY
 *
 * @author Leonardo Taglialegne
 */
public class MemoryLogger implements MyLogger{
	private final StringBuffer buffer = new StringBuffer();

	public void println(final LogLevel level, final String string){
		buffer.append(level);
		buffer.append(": ");
		buffer.append(string);
		buffer.append('\n');
	}
}
