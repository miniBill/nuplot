package meplot.android.gui.activities.calc;

import java.util.ArrayList;
import java.util.List;

import meplot.android.R;
import meplot.android.gui.input.InputReceiver;
import platform.persistence.Persistence;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

final class CalcListAdapter extends BaseAdapter{
	private static final int MAX_LINES = 100;
	private final List<String[]> expressions = new ArrayList<String[]>();
	private final InputReceiver context;
	private final String persistenceName;

	public void saveLines(){
		if(expressions == null)
			return;
		final StringBuilder builder = new StringBuilder();
		for(int c = 0; c < expressions.size(); c++){
			final String[] curr = expressions.get(c);
			if(curr.length > 0){
				for(int d = 0; d < curr.length; d++){
					builder.append(curr[d]);
					if(d != curr.length - 1)
						builder.append(",,");
				}
				if(c != expressions.size() - 1)
					builder.append(",,,,");
			}
		}
		Persistence.saveString(persistenceName, builder.toString());
	}

	CalcListAdapter(final InputReceiver context, final String persistenceName){
		this.persistenceName = persistenceName;
		this.context = context;
		final String equations = Persistence.loadString(persistenceName);
		if(equations.length() > 0){
			final String[] found = equations.split(",,,,");
			setLines(found);
		}
	}

	@Override
	public int getCount(){
		return expressions.size();
	}

	@Override
	public String[] getItem(final int arg0){
		return expressions.get(arg0);
	}

	@Override
	public long getItemId(final int arg0){
		return arg0;
	}

	@Override
	public View getView(final int index, final View convert, final ViewGroup group){
		final String[] expr = expressions.get(index);

		final LayoutInflater inflater = LayoutInflater.from(context);
		final View view = inflater.inflate(R.layout.calcline, null);

		final TextView container = (TextView)view.findViewById(R.id.calcline_input);
		container.setText(expr[0].toString());
		final TextView container2 = (TextView)view.findViewById(R.id.calcline_output);
		container2.setText(expr[1].toString());
		final TextView container3 = (TextView)view.findViewById(R.id.calcline_dvalue);
		container3.setText(expr[2].toString());

		final OnClickListener clickListener = new OnClickListener(){
			@Override
			public void onClick(final View view){
				if(view instanceof TextView)
					context.input(((TextView)view).getText());
			}
		};

		container.setOnClickListener(clickListener);
		container2.setOnClickListener(clickListener);
		container3.setOnClickListener(clickListener);

		return view;
	}

	public void add(final String[] res){
		expressions.add(res);
		if(expressions.size() > MAX_LINES)
			expressions.remove(0);
		notifyDataSetChanged();
	}

	public String[] getLines(){
		final String[] toret = new String[expressions.size()];
		for(int i = 0; i < toret.length; i++){
			final StringBuilder builder = new StringBuilder();
			for(int j = 0; j < expressions.get(i).length; j++){
				builder.append(expressions.get(i)[j]);
				if(j < expressions.get(i).length - 1)
					builder.append(",,");
			}
			toret[i] = builder.toString();
		}
		return toret;
	}

	public void setLines(final String[] funArray){
		expressions.clear();
		for(final String curr : funArray){
			final String[] line = curr.split(",,");
			expressions.add(line);
		}
		notifyDataSetChanged();
	}

	public void clear(){
		expressions.clear();
		notifyDataSetChanged();
	}
}
