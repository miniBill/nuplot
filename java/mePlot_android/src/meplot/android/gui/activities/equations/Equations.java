package meplot.android.gui.activities.equations;

import meplot.android.R;
import meplot.android.graphics.DrawView;
import meplot.android.graphics.OverlayView;
import meplot.android.gui.activities.output.Output;
import meplot.android.gui.input.NoImeEditText;
import platform.log.Log;
import platform.log.LogLevel;
import android.content.Intent;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.ListView;

public final class Equations extends InputList{
	private static final String PERSISTENCE_NAME = "Equations";
	private ListView list;
	private InputListAdapter adapter;

	@Override
	protected void onAfterCreate(final Bundle savedState){
		setContentView(R.layout.equations);

		if(adapter == null)
			adapter = new InputListAdapter(this, PERSISTENCE_NAME, new String[] {"y=x+1", "xx+1", "sinx"});

		if(savedState != null){
			final String[] funArray = savedState.getStringArray(PERSISTENCE_NAME);
			if(funArray != null)
				adapter.setFunctions(funArray);
		}

		list = (ListView)findViewById(R.id.equations_list);
		list.setAdapter(adapter);
		registerForContextMenu(list);
		list.setChoiceMode(AbsListView.CHOICE_MODE_SINGLE);

		final View btDraw = findViewById(R.id.equations_draw);
		btDraw.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View view){
				final Intent intent = new Intent(Equations.this, Output.class);
				intent.putExtra(PERSISTENCE_NAME, adapter.getFunctions());
				startActivity(intent);
			}
		});

		setupDrawView();
	}

	private void setupDrawView(){
		final DrawView drawView = (DrawView)findViewById(R.id.output_view);
		if(drawView == null)
			return;

		drawView.setOverlay((OverlayView)findViewById(R.id.output_overlay));
		drawView.setFunctions(adapter.getFunctions());
		drawView.postInvalidate();
	}

	@Override
	protected void onPause(){
		super.onPause();
		Log.log(LogLevel.INFO, "onPause");
		adapter.saveFunctions();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState){
		super.onSaveInstanceState(outState);
		Log.log(LogLevel.INFO, "onSaveIstanceState");
		outState.putStringArray(PERSISTENCE_NAME, adapter.getFunctions());
	}

	private String commandDelete = "";

	@Override
	public void onCreateContextMenu(final ContextMenu menu, final View view, final ContextMenuInfo menuInfo){
		super.onCreateContextMenu(menu, view, menuInfo);
		commandDelete = menu.add(R.string.command_delete_equation).getTitle().toString();
	}

	@Override
	public boolean onContextItemSelected(final MenuItem item){
		return handleContextMenu(item) || super.onContextItemSelected(item);
	}

	private boolean handleContextMenu(final MenuItem item){
		final AdapterContextMenuInfo info = (AdapterContextMenuInfo)item.getMenuInfo();
		if(item.getTitle().equals(commandDelete) && info != null){
			adapter.deleteLine((int)info.id);
			return true;
		}

		return false;
	}

	private void append(final CharSequence text, final View curr, final int index){
		if(curr instanceof NoImeEditText && curr.hasFocus())
			input(text, index, (NoImeEditText)curr);
		else
			if(curr instanceof ViewGroup)
				input(text, index, (ViewGroup)curr);
	}

	private void input(final CharSequence text, final int index, final ViewGroup curr){
		for(int j = 0; j < curr.getChildCount(); j++){
			final View child = curr.getChildAt(j);
			append(text, child, index); // Not an error
		}
	}

	private void input(final CharSequence text, final int index, final NoImeEditText editor){
		final int start = editor.getSelectionStart();
		final int end = editor.getSelectionEnd();
		adapter.appendText(index, text, start, end, false);
		if(start == end)
			editor.getText().insert(start, text);
		else
			editor.getText().replace(start, end, text);
	}

	private void updateDrawView(){
		final DrawView drawView = (DrawView)findViewById(R.id.output_view);
		if(drawView == null)
			return;

		if(!drawView.setFunctions(adapter.getFunctions()))
			return;
		drawView.postInvalidate();
		drawView.forceRefresh();
	}

	private void backspace(final View curr, final int index){
		if(curr instanceof NoImeEditText && curr.hasFocus())
			back(index, (NoImeEditText)curr);
		else
			if(curr instanceof ViewGroup)
				backspace(index, (ViewGroup)curr);
	}

	private void backspace(final int index, final ViewGroup curr){
		for(int j = 0; j < curr.getChildCount(); j++){
			final View child = curr.getChildAt(j);
			backspace(child, index); // Not an error
		}
	}

	private void back(final int index, final NoImeEditText editor){
		final int start = editor.getSelectionStart();
		final int end = editor.getSelectionEnd();
		adapter.backspace(index, start, end, false);
		if(editor.getText().length() != 0 && (start != end || start != 0))
			if(start == end)
				editor.getText().delete(start - 1, end);
			else
				editor.getText().delete(start, end);
	}

	public void setSelection(final int count, final int pos, final int length){
		list.post(new Runnable(){
			@Override
			public void run(){
				final View child = list.getChildAt(count);
				if(child == null)
					return;
				final EditText editor = (EditText)child.findViewById(R.id.line_text);
				if(editor == null)
					return;
				editor.requestFocusFromTouch();
				editor.setSelected(true);
				editor.setSelection(pos, pos + length);
			}
		});
	}

	public void updateLines(){
		updateDrawView();
	}

	@Override
	protected View getKeyboardButton(){
		return findViewById(R.id.equations_keyboard);
	}
}
