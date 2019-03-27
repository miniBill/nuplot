package meplot.android.gui.activities.equations;

import android.content.Context;

public interface IInputList{
	Context getContext();

	void setSelection(int index, int from, int length);

	void updateLines();
}
