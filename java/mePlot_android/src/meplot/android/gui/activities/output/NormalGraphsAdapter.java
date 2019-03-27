package meplot.android.gui.activities.output;

import meplot.android.R;
import meplot.graphics.graphs.NormalGraph;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

public class NormalGraphsAdapter extends BaseAdapter{
	private final NormalGraph[] graphs;
	private final Context context;

	public NormalGraphsAdapter(final NormalGraph[] normalGraphs, final Context context){
		graphs = normalGraphs;
		this.context = context;
	}

	@Override
	public int getCount(){
		return graphs.length + 1;
	}

	@Override
	public Object getItem(final int position){
		if(position == 0)
			return "Manual";
		return graphs[position - 1];
	}

	@Override
	public long getItemId(final int position){
		return position;
	}

	@Override
	public View getView(final int position, final View convertView, final ViewGroup parent){
		final LayoutInflater inflater = LayoutInflater.from(context);
		final View view = inflater.inflate(R.layout.output_spinnerview, null);

		final View color = view.findViewById(R.id.spinnerview_color);
		if(position > 0)
			color.setBackgroundColor(0xFF000000 | graphs[position - 1].getColor());

		final View containerview = view.findViewById(R.id.spinnerview_text);
		final TextView container = (TextView)containerview;
		if(position > 0)
			container.setText(graphs[position - 1].getExpression().toString());
		else
			container.setText("Manual");
		return view;
	}
}
