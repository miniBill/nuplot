package meplot.android.gui.input;

import meplot.android.AndroidSettings;
import platform.persistence.Persistence;
import platform.persistence.listeners.BooleanSettingsListener;
import android.content.Context;
import android.text.InputType;
import android.util.AttributeSet;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.widget.EditText;

public class NoImeEditText extends EditText implements BooleanSettingsListener{
	private static volatile boolean customime;

	public static final void setIme(final boolean value){
		customime = value;
	}

	public NoImeEditText(final Context context){
		super(context);
	}

	public NoImeEditText(final Context context, final AttributeSet attrs){
		super(context, attrs);
		customime = Persistence.loadBoolean(AndroidSettings.CUSTOMIME, true);
		setInputType();
	}

	public NoImeEditText(final Context context, final AttributeSet attrs,
			final int defStyle){
		super(context, attrs, defStyle);
		customime = Persistence.loadBoolean(AndroidSettings.CUSTOMIME, true);
		setInputType();
	}

	public void changedSetting(final String name, final boolean arg){
		if(name.equals(AndroidSettings.CUSTOMIME))
			customime = arg;
	}

	@Override
	public final boolean onCheckIsTextEditor(){
		if(customime)
			return false;
		return super.onCheckIsTextEditor();
	}

	@Override
	public final InputConnection onCreateInputConnection(final EditorInfo outAttrs){
		if(customime)
			return null;
		return super.onCreateInputConnection(outAttrs);
	}

	private void setInputType(){
		setInputType(InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS);
	}
}
