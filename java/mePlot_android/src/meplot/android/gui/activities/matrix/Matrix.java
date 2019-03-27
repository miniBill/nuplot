package meplot.android.gui.activities.matrix;

import meplot.android.R;
import meplot.android.gui.input.InputReceiver;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.GridView;

public final class Matrix extends InputReceiver{
	private static final class OutputListener implements OnClickListener{
		private final MatrixAdapter adapter;
		private final EditText text;

		private OutputListener(final MatrixAdapter adapter, final EditText output){
			if(adapter == null)
				throw new NullPointerException("adapter is null");
			this.adapter = adapter;
			text = output;
		}

		@Override
		public void onClick(final View arg0){
			final StringBuilder toret = new StringBuilder("{");
			for(int i = 0; i < adapter.getColumns(); i++){
				toret.append('{');

				for(int j = 0; j < adapter.getRows(); j++){
					toret.append(adapter.get(i, j));
					if(j < adapter.getRows() - 1)
						toret.append(',');
				}

				if(i == adapter.getColumns() - 1)
					toret.append('}');
				else
					toret.append("},");
			}
			toret.append('}');
			text.setText(toret);
		}
	}

	private MatrixAdapter adapter;
	private EditText output;

	@Override
	protected void onAfterCreate(final Bundle arg0){
		setContentView(R.layout.matrix);

		final GridView grid = (GridView)findViewById(R.id.matrix_grid);
		adapter = new MatrixAdapter(this, grid);
		grid.setAdapter(adapter);

		if(arg0 != null){
			final String values = arg0.getString("values");
			adapter.setString(values);
		}

		output = (EditText)findViewById(R.id.matrix_output);

		final View btOutput = findViewById(R.id.matrix_output_button);
		if(btOutput != null)
			btOutput.setOnClickListener(new OutputListener(adapter, output));
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState){
		super.onSaveInstanceState(outState);
		outState.putString("values", adapter.getString());
	}

	@Override
	protected View getKeyboardButton(){
		return findViewById(R.id.matrix_ime);
	}
}
