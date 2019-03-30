package meplot.help;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public final class GuidePages{
	public static final String HEADER = "<!DOCTYPE html>"
			+ "<html lang=\"en\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">"
			+ "<head><meta charset=\"utf-8\" />"
			+ "<link rel=\"stylesheet\" href=\"jqmath-0.2.0.css\">"
			+ "<link rel=\"stylesheet\" href=\"standard.css\">"
			+ "<script src=\"jquery-1.4.3.min.js\" />"
			+ "<script src=\"jqmath-etc-0.2.0.min.js\" /></head><body>";

	private GuidePages(){
	}

	public static String getPage(final String page){
		if(page.startsWith("functions/"))
			return FunctionGuide.getPage(page.substring(page.indexOf('/') + 1));
		final InputStream input = GuidePages.class.getResourceAsStream("/guide/" + page
				+ ".res");
		if(input != null)
			try{
				final String readAll = readAll(input);
				input.close();
				return readAll;
			}
			catch(final IOException e){
				return HEADER + "Couldn't load page" + e.getMessage() + "</body></html>";
			}

		return "Couldn't load page.";
	}

	private static String readAll(final InputStream input) throws IOException{
		final StringBuffer buffer = new StringBuffer(HEADER);

		final char[] cbuf = new char[1024];
		final Reader inputReader = new InputStreamReader(input, StandardCharsets.UTF_8);
		int read;
		do{
			read = inputReader.read(cbuf, 0, cbuf.length);
			if(read > 0)
				buffer.append(cbuf, 0, read);
		}while(read >= 0);

		buffer.append("</body></html>");
		return buffer.toString();
	}

	public static String getHeader(){
		return HEADER;
	}
}
