package meplot.android.gui.activities;

import meplot.android.AndroidSettings;
import meplot.android.R;
import meplot.expressions.operations.Division;
import meplot.persistence.Setting;
import meplot.persistence.Settings;
import platform.android.AndroidPlatform;
import platform.persistence.Persistence;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.preference.PreferenceActivity;

public class Preferences extends PreferenceActivity{
	public static void loadAndApplySettings(final SharedPreferences settings){
		Persistence.registerListener(new Division(null, null));
		final Editor editor = settings.edit();
		editor.putBoolean(AndroidSettings.CUSTOMIME, true);
		editor.commit();
		AndroidPlatform.setSettings(settings);

		final Setting[] usersettings = Settings.getUsersettings();
		final int len = usersettings.length;
		for(int c = 0; c < len; c++){
			final String name = usersettings[c].getName();
			final int value = Persistence.loadInt(name);
			if(value != 0)
				Persistence.changedSetting(name, value);
		}
	}

	@Override
	protected final void onCreate(final Bundle savedState){
		super.onCreate(savedState);
		// TODO: Fix hack
		Persistence.saveBoolean(AndroidSettings.CUSTOMIME,
				Persistence.loadBoolean(AndroidSettings.CUSTOMIME, true));
		addPreferencesFromResource(R.xml.preferences);
	}
}
