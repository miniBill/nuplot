package platform.log;


public final class RewriteLogger implements MyLogger{
	private final LogLevel from, to;
	private final MyLogger inner;

	public RewriteLogger(final LogLevel from, final LogLevel to,
			final MyLogger inner){
		this.from = from;
		this.to = to;
		this.inner = inner;
	}

	public void println(final LogLevel level, final String string){
		if(level.equals(from))
			inner.println(to, string);
		else
			inner.println(level, string);
	}
}
