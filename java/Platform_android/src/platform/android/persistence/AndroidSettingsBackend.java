package platform.android.persistence;

import platform.log.Log;
import platform.log.LogLevel;
import platform.persistence.Persistence;
import platform.persistence.SettingsBackend;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;

public class AndroidSettingsBackend implements SettingsBackend,
		OnSharedPreferenceChangeListener{
	private final SharedPreferences settings;

	public AndroidSettingsBackend(final SharedPreferences settings){
		this.settings = settings;
		settings.registerOnSharedPreferenceChangeListener(this);
	}

	@Override
	public int loadInt(final String name, final int ifEmpty){
		try{
			final int got = settings.getInt(name, 0);
			log('L' + name, Integer.toString(got));
			return got;
		}
		catch(final ClassCastException e){
		}
		return ifEmpty;
	}

	@Override
	public String loadString(final String name, final String ifEmpty){
		try{
			final String got = settings.getString(name, "");
			log('L' + name, got);
			return got;
		}
		catch(final ClassCastException e){
		}
		return ifEmpty;
	}

	@Override
	public void saveString(final String name, final String value){
		final Editor editor = settings.edit();
		log('S' + name, value);
		editor.putString(name, value);
		if(!editor.commit())
			return;
	}

	@Override
	public void saveInt(final String name, final int arg){
		final Editor editor = settings.edit();
		log('S' + name, Integer.toString(arg));
		editor.putInt(name, arg);
		if(!editor.commit())
			return;
	}

	@Override
	public void onSharedPreferenceChanged(
			final SharedPreferences sharedPreferences, final String key){
		try{
			final String changed = sharedPreferences.getString(key, "");
			log('C' + key, changed);
			Persistence.changedSetting(key, changed);
			return;
		}
		catch(final ClassCastException e){
		}
		try{
			final int changed = sharedPreferences.getInt(key, 0);
			log('C' + key, Integer.toString(changed));
			Persistence.changedSetting(key, changed);
			return;
		}
		catch(final ClassCastException e){
		}
		try{
			final boolean changed = sharedPreferences.getBoolean(key, false);
			log('C' + key, Boolean.toString(changed));
			Persistence.changedSetting(key, changed ? 1 : 0);
			return;
		}
		catch(final ClassCastException e){
		}
	}

	private static void log(final String name, final String value){
		Log.log(LogLevel.INFO, name + ": " + value);
	}

	@Override
	public void saveBoolean(final String name, final boolean arg){
		final Editor editor = settings.edit();
		log('S' + name, Boolean.toString(arg));
		editor.putBoolean(name, arg);
		if(!editor.commit())
			return;
	}

	@Override
	public boolean loadBoolean(final String name, final boolean ifEmpty){
		try{
			final boolean got = settings.getBoolean(name, ifEmpty);
			log('L' + name, Boolean.toString(got));
			return got;
		}
		catch(final ClassCastException e){
		}
		return ifEmpty;
	}
}
