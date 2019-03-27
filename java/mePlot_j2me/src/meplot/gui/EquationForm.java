package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Item;
import javax.microedition.lcdui.ItemCommandListener;
import javax.microedition.lcdui.TextField;

import meplot.graphics.MeDrawCanvas;
import meplot.graphics.graphs.Graph;
import meplot.localization.L10N;
import meplot.parser.GraphParser;
import meplot.parser.ParserException;
import platform.persistence.Persistence;

public final class EquationForm extends Form implements ItemCommandListener,
		CommandHandler{
	private static final String DEFAULT_EXPR = "y=";
	private final TextFieldList fields = new TextFieldList();
	private static final int[] COLORS = new int[]{0xFF0000, 0x00FF00, 0x0000FF,
			0xFF00FF};

	public EquationForm(){
		super("Equations");

		final String draw = L10N.get(L10N.DRAW);
		final String add = L10N.get(L10N.ADDEQUATION);
		final String del = L10N.get(L10N.DELEQUATION);
		final String insert = L10N.get(L10N.INSERT);

		delCommand = new Command(del, Command.ITEM, 0);
		addCommand = new Command(add, Command.ITEM, 0);
		drawCommand = new Command(draw, Command.ITEM, 0);
		insertCommand = new Command(insert, Command.ITEM, 0);

		addField("y=sin(x)");
		final int num = Persistence.loadInt("eqNumber");
		if(num > 0)
			// 1 is not a mistake: we already added the first field
			for(int c = 1; c < num; c++)
				addField(DEFAULT_EXPR);
	}

	private void addField(final String string){
		initField("eq" + fields.length(), string);
	}

	private void initField(final String fieldName, final String defaultString){
		String text = Persistence.loadString(fieldName);
		if(text.length() == 0)
			text = defaultString;
		final TextField field = new TextField(L10N.get(L10N.INPUT) + ':', text,
				500, TextField.NON_PREDICTIVE);
		field.setInitialInputMode("MIDP_LOWERCASE_LATIN");
		field.addCommand(drawCommand);
		field.addCommand(insertCommand);
		field.addCommand(addCommand);
		field.addCommand(delCommand);
		field.setDefaultCommand(drawCommand);
		field.setItemCommandListener(this);
		this.append(field);
		fields.add(field);
	}

	private TextField last;

	private void pushExpressionsTo(final MeDrawCanvas drawCanvas){
		drawCanvas.clearF();
		final TextFieldIterator tfi = fields.getIterator();
		for(int i = 0; tfi.hasNext(); i++){
			final TextField field = tfi.next();
			final String name = "eq" + i;
			final String content = field.getString();
			Persistence.saveString(name, content);
			final int col = COLORS[i % COLORS.length];
			try{
				drawCanvas.addF(GraphParser.parse(content, col));
			}
			catch(final ParserException e){
				drawCanvas.addF(Graph.EMPTY);
			}
		}
		Persistence.saveInt("eqNumber", tfi.length());
	}

	public void appendSymbol(final String val){
		if(last == null)
			return;
		last.insert(val, last.getCaretPosition());
	}

	private void del(final Item item){
		if(!(item instanceof TextField))
			return;
		final TextField field = (TextField)item;
		int index = fields.indexOf(field);
		if(index < 0)
			return;
		if(fields.length() > 1){
			final String string = field.getString();
			Persistence.saveString("eq" + index, string);
			fields.removeAt(index);
			delete(index);
			if(index == 0)
				index++;
			last = fields.elementAt(index - 1);
			afterAppend(MidletMenju.getDisplay());
		}
		else
			field.setString("y=sin(x)");
	}

	private final Command addCommand;
	private final Command delCommand;
	private final Command drawCommand;
	private final Command insertCommand;

	public boolean handle(final Command command){
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());

		setCommandListener(midletMenju);
	}

	public void commandAction(final Command command, final Item item){
		if(command.equals(insertCommand) && item instanceof TextField){
			last = (TextField)item;
			MidletMenju.setSymbolInD(false);
			MidletMenju.viewSymbolList();
			return;
		}
		if(command.equals(drawCommand)){
			pushExpressionsTo(MidletMenju.getDrawCanvas());
			MidletMenju.getDisplay().setCurrent(MidletMenju.getDrawCanvas());
			return;
		}
		if(command.equals(addCommand)){
			addField(DEFAULT_EXPR);
			return;
		}
		if(command.equals(delCommand))
			del(item);
	}

	public void afterAppend(final Display display){
		if(last == null)
			return;
		// So, this whole method is a big HACK,
		// caused by how j2me is implemented.
		for(int c = 0; c < fields.length(); c++)
			if(get(c).equals(last)){
				delete(c);
				insert(c, last);
				break;
			}
		display.setCurrentItem(last);
		display.setCurrent(this);
	}
}
