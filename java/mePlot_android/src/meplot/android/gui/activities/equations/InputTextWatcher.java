package meplot.android.gui.activities.equations;

import platform.log.Log;
import platform.log.LogLevel;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;

final class InputTextWatcher implements TextWatcher{
	private final InputListAdapter equationListAdapter;
	private final int index;
	private final View deleter;
	private final IInputList context;

	InputTextWatcher(final InputListAdapter equationListAdapter, final int index, final View deleter,
			final IInputList context){
		this.equationListAdapter = equationListAdapter;
		this.index = index;
		this.deleter = deleter;
		this.context = context;
	}

	@Override
	public void onTextChanged(final CharSequence text, final int start, final int before, final int length){
		Log.log(LogLevel.INFO, Integer.toString(index) + ':' + text);
		equationListAdapter.setItem(index, text.toString());
		if(text.length() == 0){
			deleter.setEnabled(false);
			if(equationListAdapter.cleanTail())
				context.setSelection(index, start + length, 0);
		}
		else{
			deleter.setEnabled(true);
			if(equationListAdapter.addEmpty())
				context.setSelection(index, start + length, 0);
		}

		context.updateLines();
	}

	@Override
	public void beforeTextChanged(final CharSequence text, final int start, final int count, final int after){
		// Not needed
	}

	@Override
	public void afterTextChanged(final Editable arg0){
		// Already handling elsewhere
	}
}