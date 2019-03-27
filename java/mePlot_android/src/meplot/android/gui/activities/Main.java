package meplot.android.gui.activities;

import meplot.android.AndroidSettings;
import meplot.android.R;
import meplot.android.gui.activities.calc.Calc;
import meplot.android.gui.activities.equations.Equations;
import meplot.android.gui.activities.matrix.Matrix;
import meplot.android.gui.input.NoImeEditText;
import meplot.persistence.Settings;
import platform.Platform;
import platform.android.AndroidPlatform;
import platform.android.log.LogCatLogger;
import platform.android.log.ToasterLogger;
import platform.log.CompositeLogger;
import platform.log.FilterLogger;
import platform.log.Log;
import platform.log.LogLevel;
import platform.persistence.Persistence;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class Main extends Activity{
	@Override
	public boolean onCreateOptionsMenu(final Menu menu){
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.main_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item){
		if(item.getItemId() == R.id.menu_preferences){
			final Intent intent = new Intent(this, Preferences.class);
			startActivity(intent);
		}
		return true;
	}

	@Override
	public void onCreate(final Bundle savedState){
		super.onCreate(savedState);
		setContentView(R.layout.main);

		final boolean isAndroid = Platform.getPlatform() instanceof AndroidPlatform;
		if(Platform.getPlatform() == null || !isAndroid)
			Platform.set(AndroidPlatform.getIstance());

		setupLogger();

		setButtons();

		setupSettings();
	}

	protected void setButtons(){
		setGenericButtons();
		setButton(R.id.main_solve, Solver.class);
	}

	protected final void setGenericButtons(){
		setButton(R.id.main_draw, Equations.class);
		setButton(R.id.main_calc, Calc.class);
		setButton(R.id.main_credits, Credits.class);
		setButton(R.id.main_guide, Guide.class);
		setButton(R.id.main_matrix, Matrix.class);
		//setButton(R.id.main_functions, Functions.class);
		setFeedbackButton(R.id.main_feedback);
	}

	private void setFeedbackButton(final int id){
		final Button feedback = (Button)findViewById(id);
		if(feedback != null)
			feedback.setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(final View v){
					final String version = getResources().getString(R.string.app_version);

					final Intent intent = new Intent(Intent.ACTION_SEND);
					intent.setType("plain/text");
					intent.putExtra(Intent.EXTRA_EMAIL, new String[] {"leonardo.taglialegne+meplot@gmail.com"});
					intent.putExtra(Intent.EXTRA_SUBJECT, "[mePlot " + version + "]Feedback");
					intent.putExtra(Intent.EXTRA_TEXT, "");
					startActivity(Intent.createChooser(intent, ""));
				}
			});
	}

	private void setupLogger(){
		final ToasterLogger toast = new ToasterLogger(this);
		final FilterLogger filter = new FilterLogger(LogLevel.WARNING, toast);
		final LogCatLogger logcat = new LogCatLogger("DaPlot");
		final CompositeLogger composite = new CompositeLogger(logcat, filter);
		Log.setLogger(composite);
	}

	private void setupSettings(){
		Preferences.loadAndApplySettings(PreferenceManager.getDefaultSharedPreferences(this));
		if(Persistence.loadInt(Settings.INVERTCONTROLS) != 1)
			Persistence.saveInt(Settings.INVERTCONTROLS, 1);

		final boolean customIme = Persistence.loadBoolean(AndroidSettings.CUSTOMIME, true);
		final NoImeEditText niet = new NoImeEditText(this);
		NoImeEditText.setIme(customIme);
		Persistence.registerListener(niet);
	}

	protected final void setButton(final int id, final Class<?> iclass){
		final Button button = (Button)findViewById(id);
		if(button != null)
			button.setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(final View view){
					final Intent intent = new Intent(view.getContext(), iclass);
					startActivity(intent);
				}
			});
	}
}
