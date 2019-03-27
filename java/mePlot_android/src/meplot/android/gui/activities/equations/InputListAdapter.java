package meplot.android.gui.activities.equations;

import meplot.android.R;
import platform.persistence.Persistence;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.EditText;

public class InputListAdapter extends BaseAdapter{
	private String[] items;
	private final String persistenceName;
	private final IInputList context;

	public void saveFunctions(){
		if(items == null)
			return;
		final StringBuilder builder = new StringBuilder();
		for(int c = 0; c < items.length; c++)
			if(items[c].length() > 0){
				builder.append(items[c]);
				if(c != items.length - 1)
					builder.append(",,");
			}
		Persistence.saveString(persistenceName, builder.toString());
	}

	public InputListAdapter(final IInputList context, final String persistenceName, final String[] defaults){
		this.persistenceName = persistenceName;
		this.context = context;
		final String equations = Persistence.loadString(persistenceName);
		if(equations.length() == 0)
			items = defaults;
		else{
			final String[] found = equations.split(",,");
			items = found;
		}
		addEmpty();
	}

	public boolean addEmpty(){
		if(items[items.length - 1].length() == 0)
			return false;
		final String[] olditems = items;
		items = new String[olditems.length + 1];
		System.arraycopy(olditems, 0, items, 0, olditems.length);
		items[olditems.length] = "";
		notifyDataSetChanged();
		return true;
	}

	@Override
	public int getCount(){
		if(items == null)
			return 0;
		return items.length;
	}

	@Override
	public String getItem(final int arg0){
		return items[arg0];
	}

	@Override
	public long getItemId(final int arg0){
		return arg0;
	}

	@Override
	public View getView(final int count, final View convertView, final ViewGroup group){
		final LayoutInflater inflater = LayoutInflater.from(context.getContext());
		final View view = inflater.inflate(R.layout.inputlist_line, null);

		final View deleter = view.findViewById(R.id.line_button);
		deleter.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View view){
				InputListAdapter.this.deleteLine(count);
			}
		});
		deleter.setEnabled(items[count].length() > 0);

		final EditText container = (EditText)view.findViewById(R.id.line_text);
		container.setText(items[count]);
		final InputTextWatcher watcher = new InputTextWatcher(this, count, deleter, context);
		container.addTextChangedListener(watcher);
		return view;
	}

	public boolean cleanTail(){
		if(items.length == 0)
			return false;
		int lastempty = items.length;
		while(lastempty > 0 && items[lastempty - 1].length() == 0)
			lastempty--;
		if(lastempty == 0){
			items = new String[] {""};
			notifyDataSetChanged();
			return true;
		}
		if(lastempty == items.length - 1)
			return false;
		final String[] olditems = items;
		items = new String[lastempty + 1];
		System.arraycopy(olditems, 0, items, 0, lastempty);
		items[lastempty] = "";
		notifyDataSetChanged();
		return true;
	}

	public void deleteLine(final int index){
		if(index >= items.length)
			return;
		final String[] olditems = items;
		items = new String[olditems.length - 1];
		for(int c = 0; c < index; c++)
			items[c] = olditems[c];
		for(int c = index; c < olditems.length - 1; c++)
			items[c] = olditems[c + 1];
		context.updateLines();
		notifyDataSetChanged();
	}

	public String[] getFunctions(){
		if(items.length == 0)
			return new String[0];
		final String[] toret = new String[items.length - 1];
		System.arraycopy(items, 0, toret, 0, items.length - 1);
		return toret;
	}

	public void setFunctions(final String[] stringArray){
		if(!hasChanged(stringArray))
			return;
		items = stringArray;
		notifyDataSetChanged();
	}

	private boolean hasChanged(final String[] stringArray){
		if(stringArray.length != items.length)
			return true;
		for(int i = 0; i < items.length; i++)
			if(!items[i].equals(stringArray[i]))
				return true;
		return false;
	}

	public void appendText(final int index, final CharSequence text, final int start, final int end,
			final boolean notify){
		final String tapp = items[index];
		if(start < items[index].length())
			items[index] = items[index].substring(0, start) + text;
		else
			items[index] += text;
		if(tapp.length() > end)
			items[index] += tapp.substring(end);
		if(notify)
			notifyDataSetChanged();
	}

	public void backspace(final int index, final int start, final int end, final boolean notify){
		if(items[index].length() == 0 || start == end && start == 0)
			return;
		final int cut = start == end ? start - 1 : start;
		if(items[index].length() > cut)
			items[index] = items[index].substring(0, cut) + items[index].substring(end);
		if(notify)
			notifyDataSetChanged();
	}

	public void setItem(final int index, final String value){
		items[index] = value;
	}
}
