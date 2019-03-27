package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;

import meplot.expressions.functions.UserFunction;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import meplot.parser.tokens.FunctionToken;
import meplot.parser.tokens.UserFunctionList;
import platform.persistence.Persistence;

public final class UserFunctionsForm extends Form implements CommandHandler{
	private final TextFieldList fields = new TextFieldList();

	public UserFunctionsForm(){
		super("Functions:");

		addCommand = new Command("Add function", Command.ITEM, 0);
		delCommand = new Command("Delete function", Command.ITEM, 0);

		addField("ball(r,d,x,y):=norm(d,x,y)<r");
		final int num = Persistence.loadInt("userFunctNumber");
		if(num > 0)
			// 1 is not a mistake
			for(int c = 1; c < num; c++)
				add();
	}

	private void addField(final String string){
		initField("userFunct" + fields.length(), string);
	}

	private void initField(final String fieldName, final String defaultString){
		String text = Persistence.loadString(fieldName);
		if(text.length() == 0)
			text = defaultString;
		final TextField field = new TextField("Function:", text, 500,
				TextField.NON_PREDICTIVE);
		field.setInitialInputMode("MIDP_LOWERCASE_LATIN");
		this.append(field);
		fields.add(field);
	}

	private void saveFunctions(){
		final UserFunctionList toadd = new UserFunctionList();
		final TextFieldIterator tfi = fields.getIterator();
		for(int count = 0; tfi.hasNext(); count++){
			final TextField field = tfi.next();
			final String name = "userFunct" + count;
			final String content = field.getString();
			Persistence.saveString(name, content);
			try{
				final UserFunction parsed = Parser.parseUserFunction(content);
				toadd.add(parsed);
			}
			catch(final ParserException e){
			}
		}
		Persistence.saveInt("userFunctNumber", fields.length());
		FunctionToken.setUserFunctions(toadd);
	}

	private void add(){
		addField("f():=");
	}

	private void del(){
		final TextField field = fields.getLast();
		final String string = field.getString();
		Persistence.saveString("userFunct" + (fields.length() - 1), string);
		fields.dequeue();
		delete(fields.length());
	}

	private final Command addCommand;
	private final Command delCommand;

	public boolean handle(final Command command){
		if(command.equals(addCommand)){
			add();
			return true;
		}
		if(command.equals(delCommand)){
			del();
			return true;
		}
		if(command.equals(MidletMenju.getBackCommand()))
			saveFunctions();

		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(addCommand);
		addCommand(delCommand);
		addCommand(MidletMenju.getBackCommand());
		setCommandListener(midletMenju);
	}
}
