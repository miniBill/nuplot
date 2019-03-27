package meplot.gui.help;

import java.io.IOException;
import java.io.Reader;

public final class StringReader extends Reader{
	private final String value;
	private boolean closed = false;
	private final int length;

	public StringReader(final String input){
		value = input;
		length = input.length();
	}

	public void close() throws IOException{
		closed = true;
	}

	private int boff = 0;

	public int read(final char[] cbuf, final int off, final int len)
			throws IOException{
		if(closed)
			throw new IOException("Already closed stream");
		boff += off;
		if(boff >= value.length())
			return -1;
		int i;
		for(i = 0; i < len && i + boff < length; i++)
			cbuf[i] = value.charAt(i + boff);
		boff += i;
		return i;
	}

}
