package meplot.help;

import java.util.Vector;

import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunctor;
import meplot.parser.ParserException;
import meplot.parser.tokens.FunctionToken;

public final class FunctionGuide{
	private FunctionGuide(){

	}

	public static String getPage(final String page){
		if("list".equals(page))
			return getList();
		try{
			final IFunctor match = FunctionToken.match(page);
			final StringBuffer toret = new StringBuffer(GuidePages.getHeader());
			genHelp(match, toret);
			toret.append("</body></html>");
			return toret.toString();
		}
		catch(final ParserException e){
			return GuidePages.getHeader() + "<h1>Function not found!</h1></body></html>";
		}
	}

	private static void genHelp(final IFunctor match, final StringBuffer toret){
		if(match instanceof IHelpFunction){
			final IHelpFunction help = (IHelpFunction)match;
			genEasyHelp(help, toret);
		}
		else
			genHardHelp(match, toret);
	}

	private static void genHardHelp(final IFunctor match, final StringBuffer toret){
		toret.append("<h1>");
		toret.append(match.getName());
		toret.append('(');
		char var = 'x';
		for(int i = 0; i < match.needs(); i++, var++){
			toret.append(var);
			if(i < match.needs() - 1)
				toret.append(',');
			if(var == 'z')
				var = 's';
		}
		toret.append(")</h1>\n<b>Category</b>: ");
		toret.append(match.getCategory());
		toret.append(".<br/>\nNo documentation for this function. :(");
	}

	private static void genEasyHelp(final IHelpFunction match, final StringBuffer toret){
		toret.append("<h1>");
		toret.append(match.getName());
		toret.append('(');
		char var = 'x';
		for(int i = 0; i < match.needs(); i++){
			final String argumentName = match.argumentName(i);
			if(argumentName == null)
				toret.append(var++);
			else
				toret.append(argumentName);
			if(i < match.needs() - 1)
				toret.append(',');
		}
		toret.append(")</h1>\n<b>Category</b>: ");
		toret.append(match.getCategory());

		toret.append('\n');
		if(hasArgumentDescriptions(match)){
			toret.append("<h2>Arguments</h2>\n");
			for(int i = 0; i < match.needs(); i++){
				final String desc = match.argumentDescription(i);
				if(desc != null && desc.length() > 0){
					toret.append("<b>");
					toret.append(match.argumentName(i));
					toret.append("</b>:");
					toret.append(desc);
					toret.append(".<br/>\n");
				}
			}
		}
		final String description = match.getDescription();
		if(description != null && description.length() > 0){
			toret.append("<h2>Description</h2>");
			toret.append(description);
			toret.append('.');
		}
	}

	private static boolean hasArgumentDescriptions(final IHelpFunction match){
		for(int i = 0; i < match.needs(); i++){
			final String desc = match.argumentDescription(i);
			if(desc != null && desc.length() > 0)
				return true;
		}
		return false;
	}

	private static String getList(){
		final StringBuffer toret = new StringBuffer(GuidePages.getHeader());
		toret.append("<h1>Function list</h1>\n");
		final IFunctor[] fun = FunctionToken.getDefaultFunctions();
		final String[] categories = FunctionCategory.getAll();
		final Vector[] lists = new Vector[categories.length];
		for(int j = 0; j < lists.length; j++)
			lists[j] = new Vector();
		for(int i = 0; i < fun.length; i++){
			final int index = indexof(categories, fun[i].getCategory());
			if(index != -1)
				lists[index].addElement(fun[i]);
		}
		for(int j = 0; j < lists.length; j++)
			if(!categories[j].equals(FunctionCategory.USER_DEFINED)){
				toret.append("<h2>");
				toret.append(categories[j]);
				toret.append("</h2>\n<ul>\n");
				for(int i = 0; i < lists[j].size(); i++){
					final IFunctor funct = (IFunctor)lists[j].elementAt(i);
					final String name = funct.getName();
					toret.append("<a href=\"functions/");
					toret.append(name);
					toret.append("\">");
					toret.append(name);
					if(i < lists[j].size() - 1)
						toret.append("</a>, \n");
					else
						toret.append("</a>.\n");
				}
				toret.append("</ul>\n");
			}
		toret.append("</body></html>");
		return toret.toString();
	}

	private static int indexof(final String[] categories, final String category){
		for(int i = 0; i < categories.length; i++)
			if(categories[i].equals(category))
				return i;
		return -1;
	}
}
