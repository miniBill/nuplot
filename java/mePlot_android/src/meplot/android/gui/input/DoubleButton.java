package meplot.android.gui.input;

import android.content.Context;
import android.graphics.Canvas;
import android.widget.Button;

public final class DoubleButton extends Button{
	public DoubleButton(final Context context){
		super(context);
		super.setBackgroundColor(0xFF2020A0);
	}

	@Override
	protected void onDraw(final Canvas canvas){
		super.onDraw(canvas);
	}
}
