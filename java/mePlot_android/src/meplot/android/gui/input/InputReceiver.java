package meplot.android.gui.input;

import java.util.Timer;
import java.util.TimerTask;

import meplot.android.AndroidSettings;
import meplot.android.R;
import platform.persistence.Persistence;
import platform.persistence.listeners.BooleanSettingsListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.SystemClock;
import android.support.v4.app.FragmentActivity;
import android.support.v4.view.ViewPager;
import android.view.KeyCharacterMap;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

import com.viewpagerindicator.CirclePageIndicator;

public abstract class InputReceiver extends FragmentActivity implements IInputReceiver{
	private Timer t;
	private final boolean forcePortrait;

	protected InputReceiver(){
		forcePortrait = false;
	}

	protected InputReceiver(final boolean portrait){
		forcePortrait = portrait;
	}

	public final void input(final CharSequence text){
		final KeyEvent kev = new KeyEvent(SystemClock.uptimeMillis(), text.toString(), KeyCharacterMap.ALPHA,
				KeyEvent.FLAG_FROM_SYSTEM);
		dispatchKeyEvent(kev);
	}

	protected final boolean isCustomIme(){
		return customime;
	}

	private boolean customime = true;
	private ViewGroup ime;

	protected boolean isTextviewSelected(){
		return true;
	}

	private BooleanSettingsListener getSettingsListener(final ViewGroup ime){
		return new BooleanSettingsListener(){
			@Override
			public void changedSetting(final String name, final boolean arg){
				if(name.equals(AndroidSettings.CUSTOMIME)){
					customime = arg;
					setImeVisibility();
					if(arg){
						final InputMethodManager imm = (InputMethodManager)getSystemService(INPUT_METHOD_SERVICE);
						imm.hideSoftInputFromWindow(ime.getWindowToken(), 0);
					}
				}
			}
		};
	}

	@Override
	protected void onPause(){
		super.onPause();
		Persistence.saveBoolean(AndroidSettings.CUSTOMIME, customime);
	}

	@Override
	protected final void onResume(){
		super.onResume();
		setupIme(getKeyboardButton());
	}

	private void setupIme(final View btKeyboard){
		final ViewPager pager = (ViewPager)findViewById(R.id.input_flipper);
		ime = (ViewGroup)findViewById(R.id.ime_include);
		final CirclePageIndicator indicator = (CirclePageIndicator)findViewById(R.id.input_indicator);
		InputInitializer.init(pager, ime, this, indicator, forcePortrait);

		customime = Persistence.loadBoolean(AndroidSettings.CUSTOMIME, true);
		setImeVisibility();
		Persistence.registerListener(getSettingsListener(ime));
		btKeyboard.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				customime = !customime;
				Persistence.saveBoolean(AndroidSettings.CUSTOMIME, customime);
				setImeVisibility();
			}
		});
	}

	@Override
	protected final void onCreate(final Bundle arg0){
		super.onCreate(arg0);
		handler = new Handler(){
			public void handleMessage(final Message msg){
				final KeyEvent kev = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL);
				dispatchKeyEvent(kev);
			}
		};
		onAfterCreate(arg0);
	}

	protected abstract void onAfterCreate(final Bundle arg0);

	protected abstract View getKeyboardButton();

	private Handler handler;

	public final void backspaceDown(){
		synchronized(this){
			t = new Timer();
			t.schedule(new TimerTask(){
				@Override
				public void run(){
					handler.sendEmptyMessage(0);
				}
			}, 0, 100);
		}
	}

	public final void backspaceUp(){
		synchronized(this){
			t.cancel();
			t.purge();
		}
		final KeyEvent kev = new KeyEvent(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_DEL);
		dispatchKeyEvent(kev);
	}

	protected final void setImeVisibility(){
		if(customime && isTextviewSelected())
			ime.setVisibility(View.VISIBLE);
		else
			ime.setVisibility(View.GONE);
	}
}
