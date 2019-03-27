package meplot.gui;

import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.ChoiceGroup;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Item;
import javax.microedition.lcdui.ItemStateListener;

import meplot.expressions.operations.Division;
import meplot.localization.L10N;
import meplot.persistence.Setting;
import meplot.persistence.Settings;
import platform.persistence.Persistence;

public final class SettingsForm extends Form implements CommandHandler{
	private final ChoiceGroup[] choices;

	public SettingsForm(){
		super(L10N.get(L10N.OPTIONS));

		final Setting[] usersettings = Settings.getUsersettings();
		final int len = usersettings.length;
		choices = new ChoiceGroup[len];

		for(int c = 0; c < len; c++)
			addChoice(c);

		Persistence.registerListener(Division.EMPTY);

		setItemStateListener(new ItemStateListener(){
			public void itemStateChanged(final Item arg0){
				for(int c = 0; c < len; c++)
					if(arg0 == choices[c])
						Persistence.saveInt(usersettings[c].getName(),
								choices[c].getSelectedIndex());
			}
		});

		for(int c = 0; c < len; c++)
			load(choices[c], usersettings[c].getName());
	}

	private void addChoice(final int index){
		final Setting setting = Settings.getUsersettings()[index];
		choices[index] = new ChoiceGroup(setting.getDescription(),
				Choice.EXCLUSIVE, setting.getChoices(), null);
		append(choices[index]);
	}

	private static void load(final ChoiceGroup choice, final String setting){
		choice.setSelectedIndex(Persistence.loadInt(setting), true);
		if(choice.getSelectedIndex() != 0)
			Persistence.saveInt(setting, choice.getSelectedIndex());
	}

	public boolean handle(final Command command){
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());

		setCommandListener(midletMenju);
	}
}
