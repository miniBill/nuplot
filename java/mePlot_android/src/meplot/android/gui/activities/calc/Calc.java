package meplot.android.gui.activities.calc;

import meplot.android.R;
import meplot.android.gui.input.InputReceiver;
import meplot.android.gui.input.NoImeEditText;
import meplot.expressions.Expression;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import platform.log.Log;
import platform.log.LogLevel;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;

public final class Calc extends InputReceiver{
	private final class RunListener implements OnClickListener{
		@Override
		public void onClick(final View view){
			final String inputText = editor.getText().toString();
			if(inputText.length() == 0)
				return;
			editor.setText("");

			Expression input = Int.ZERO;
			String out = null;
			String value = null;
			try{
				input = Parser.parse(inputText);
				final Expression eout = SimplificationHelper.simplify(input);
				out = eout.toString();
				value = eout.applyConstants().matrixDvalue(ValueList.EMPTY).toString();
			}
			catch(final CalcException e){
				if(out == null)
					out = e.getMessage();
				value = e.getMessage();
			}
			catch(final ParserException e){
				out = e.getMessage();
				value = e.getMessage();
			}

			final String[] res = new String[] {
					input.toString(), out, value
			};

			adapter.add(res);
		}
	}

	private NoImeEditText editor;
	private CalcListAdapter adapter;

	@Override
	protected void onAfterCreate(final Bundle savedState){
		setContentView(R.layout.calc);

		final ListView listView = (ListView)findViewById(R.id.calc_list);
		if(adapter == null)
			adapter = new CalcListAdapter(this, "Calclines");
		listView.setAdapter(adapter);

		editor = (NoImeEditText)findViewById(R.id.calc_input);

		final View run = findViewById(R.id.calc_run);
		run.setOnClickListener(new RunListener());

		if(savedState != null){
			final String[] funArray = savedState.getStringArray("Lines");
			if(funArray != null)
				adapter.setLines(funArray);
		}
	}

	@Override
	protected void onPause(){
		super.onPause();
		adapter.saveLines();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState){
		super.onSaveInstanceState(outState);
		Log.log(LogLevel.INFO, "onSaveIstanceState");
		outState.putStringArray("Lines", adapter.getLines());
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu){
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.calc_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item){
		if(item.getItemId() == R.id.menu_clear)
			adapter.clear();
		else
			Log.log(LogLevel.DEBUG, "Unknown option selected in Calc: " + item.getItemId());
		return true;
	}

	@Override
	protected View getKeyboardButton(){
		return findViewById(R.id.calc_ime);
	}
}
