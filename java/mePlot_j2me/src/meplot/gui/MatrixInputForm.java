package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;

import meplot.localization.L10N;

public final class MatrixInputForm extends Form implements CommandHandler{
	private final TextField inField;

	public MatrixInputForm(){
		super("MatrixInputForm");
		final String next = L10N.get(L10N.NEXT);
		elementCommand = new Command(next, Command.OK, 0);

		inField = new TextField(L10N.get(L10N.INPUT) + ':', "{{", 500,
				TextField.NON_PREDICTIVE);
		inField.setInitialInputMode("MIDP_LOWERCASE_LATIN");
		this.append(inField);
	}

	private void endElement(){
		final int caret = inField.getCaretPosition();
		final String text = inField.getString();
		if(text.charAt(caret - 1) == ','){
			inField.delete(caret - 1, 1);
			inField.insert("},{", caret - 1);
		}
		else
			if(text.charAt(caret - 1) == '{'){
				inField.delete(caret - 2, 2);
				// don't use insert, but setString, to make cursor go to end
				inField.setString(inField.getString() + '}');
				MidletMenju.addMatrix(inField.getString());
			}
			else
				inField.insert(",", caret);
	}

	private final Command elementCommand;

	public boolean handle(final Command command){
		if(command.equals(elementCommand)){
			endElement();
			return true;
		}
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		// miForm.addCommand(miInsertCommand);
		addCommand(elementCommand);
		addCommand(MidletMenju.getBackCommand());

		setCommandListener(midletMenju);
	}
}
