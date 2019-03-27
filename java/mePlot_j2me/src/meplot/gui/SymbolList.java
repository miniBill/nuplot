package meplot.gui;

import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.CommandListener;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;

import meplot.expressions.functions.IFunctor;
import meplot.localization.L10N;
import meplot.parser.tokens.FunctionToken;
import meplot.parser.utils.MatrixDivider;
import meplot.parser.utils.SymbolListener;

public final class SymbolList extends List implements SymbolListener{
	private final Display display;

	public SymbolList(final Display display, final String title){
		super(title, Choice.IMPLICIT);
		this.display = display;
	}

	private final SymbolListList submenus = new SymbolListList();

	private SymbolList createSubmenu(final String string){
		final SymbolList newList = new SymbolList(display, string);
		submenus.add(newList);
		append(string, null);
		return newList;
	}

	public boolean isChildren(final SymbolList symbolList){
		if(equals(symbolList))
			return true;
		final SymbolListIterator iterator = symbolList.submenus.getIterator();
		while(iterator.hasNext())
			if(isChildren(iterator.next()))
				return true;
		return false;
	}

	public String push(){
		final int index = getSelectedIndex();
		final String name = getString(index);
		final SymbolList list = search(name);
		if(list == null)
			return name;
		display.setCurrent(list);
		return "";
	}

	private SymbolList search(final String name){
		if(getTitle().equals(name))
			return this;
		final SymbolListIterator iterator = submenus.getIterator();
		while(iterator.hasNext()){
			final SymbolList look = iterator.next().search(name);
			if(look != null)
				return look;
		}
		return null;
	}

	public void addCommand(final Command cmd){
		super.addCommand(cmd);
		final SymbolListIterator iterator = submenus.getIterator();
		while(iterator.hasNext())
			iterator.next().addCommand(cmd);
	}

	public void setCommandListener(final CommandListener listener){
		super.setCommandListener(listener);
		final SymbolListIterator iterator = submenus.getIterator();
		while(iterator.hasNext())
			iterator.next().setCommandListener(listener);
	}

	private SymbolList matrix;

	public void loadDefaultList(){
		final SymbolList ops = createSubmenu(L10N.get(L10N.OPERATIONS));
		ops.appendRange(new String[]{"(", ")", "+", "-", "*", "/", "^", ",",
				";"});

		final SymbolList bool = createSubmenu(L10N.get(L10N.BOOLEAN));
		bool.appendRange(new String[]{"<", "<=", ">", ">=", "==", "<>"});

		final IFunctor[] defaultFunctions = FunctionToken.getDefaultFunctions();
		final int len = defaultFunctions.length;
		for(int c = 0; c < len; c++)
			add(defaultFunctions[c]);

		matrix = getOrCreateCategory(L10N.get(L10N.MATRICES));
		matrix.append("{{");
		matrix.append("},{");
		matrix.append("}}");
		MatrixDivider.setListener(this);
	}

	private void append(final String string){
		append(string, null);
	}

	private void appendRange(final String[] strings){
		for(int c = 0; c < strings.length; c++)
			append(strings[c], null);
	}

	private SymbolList getOrCreateCategory(final String string){
		final SymbolList toret = submenus.getCategory(string);
		if(toret == null)
			return createSubmenu(string);
		return toret;
	}

	private void add(final IFunctor abstractFunctor){
		final String category = abstractFunctor.getCategory();
		if("Hide".equals(category))
			return;
		final SymbolList cat = getOrCreateCategory(category);
		cat.append(abstractFunctor.getName() + '(', null);
	}

	private int lastMatPointer;
	private final String[] lastMat = new String[10];

	public void addMatrix(final String string){
		for(int c = 0; c < lastMat.length; c++)
			if(lastMat[c] != null && lastMat[c].equals(string))
				return;
		matrix.deleteAll();
		matrix.append("{{");
		matrix.append("},{");
		matrix.append("}}");
		lastMat[lastMatPointer++] = string;
		for(int c = 0; c < lastMat.length; c++)
			if(lastMat[c] != null)
				matrix.append(lastMat[c]);
	}
}
