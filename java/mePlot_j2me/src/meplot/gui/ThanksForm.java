package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Item;
import javax.microedition.lcdui.TextField;

public final class ThanksForm extends Form implements CommandHandler{
	public ThanksForm(){
		super("");
		final Item box = new TextField(
				"Persone",
				"Chi ha contribuito:\n\n"
						+ "Code monkey:\nminiBill\n\nIdee:\nMarco e Fabio\n"
						+ "minder2k\nCorralx\nf.sorr\nMezzo IV\nI creatori di GraphViewer",
				500, TextField.UNEDITABLE);
		append(box);
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());

		setCommandListener(midletMenju);
	}

	public boolean handle(final Command command){
		return false;
	}
}
