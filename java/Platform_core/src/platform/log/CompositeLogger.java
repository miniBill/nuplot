package platform.log;


public final class CompositeLogger implements MyLogger{
	private final MyLogger[] list;

	public CompositeLogger(final MyLogger first, final MyLogger second){
		list = new MyLogger[2];
		list[0] = first;
		list[1] = second;
	}

	public void println(final LogLevel level, final String string){
		if(list == null)
			return;
		for(int i = 0; i < list.length; i++)
			list[i].println(level, string);
	}
}
