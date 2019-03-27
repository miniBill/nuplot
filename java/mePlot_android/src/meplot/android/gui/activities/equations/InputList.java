package meplot.android.gui.activities.equations;

import meplot.android.gui.input.InputReceiver;
import android.content.Context;

public abstract class InputList extends InputReceiver implements IInputList{
	@Override
	public final Context getContext(){
		return this;
	}
}
