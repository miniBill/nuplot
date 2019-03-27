package meplot.android.gui.activities.matrix;

import meplot.android.R;
import meplot.android.gui.input.NoImeEditText;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;

class MatrixAdapter extends BaseAdapter{
	private final Matrix context;
	private final GridView grid;
	private int columns = 3;
	private int rows = 3;
	private String[][] data = new String[rows][columns];

	MatrixAdapter(final Matrix matrix, final GridView grid){
		context = matrix;
		this.grid = grid;
	}

	@Override
	public int getCount(){
		return columns * rows;
	}

	@Override
	public String getItem(final int position){
		return data[position / columns][position % columns];
	}

	@Override
	public long getItemId(final int position){
		return position;
	}

	@Override
	public View getView(final int position, final View convertView, final ViewGroup parent){
		final View view;
		if(position % columns == columns - 1)
			if(position == columns * rows - 1)
				view = bottomRight();
			else
				view = right(position);
		else
			if(position / columns == rows - 1)
				view = bottom(position);
			else
				view = normal(position);

		return view;
	}

	private View normal(final int position){
		final NoImeEditText view = new NoImeEditText(context);
		final int row = position / columns;
		final int col = position % columns;
		view.setText(data[row][col] == null ? "" : data[row][col]);
		view.addTextChangedListener(new TextWatcher(){
			@Override
			public void onTextChanged(final CharSequence arg0, final int arg1, final int arg2, final int arg3){
			}

			@Override
			public void beforeTextChanged(final CharSequence arg0, final int arg1, final int arg2, final int arg3){
			}

			@Override
			public void afterTextChanged(final Editable arg0){
				data[row][col] = arg0.toString();
			}
		});
		return view;
	}

	private View bottom(final int position){
		final View view;
		final LayoutInflater inflater = LayoutInflater.from(context);
		view = inflater.inflate(R.layout.minusbutton, null);
		final View button = view.findViewById(R.id.minus_button);
		button.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				MatrixAdapter.this.deleteColumn(position % columns);
			}
		});
		return view;
	}

	private View right(final int position){
		final View view;
		final LayoutInflater inflater = LayoutInflater.from(context);
		view = inflater.inflate(R.layout.minusbutton, null);
		final View button = view.findViewById(R.id.minus_button);
		button.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				MatrixAdapter.this.deleteRow(position / columns);
			}
		});
		return view;
	}

	private View bottomRight(){
		final View view;
		final LayoutInflater inflater = LayoutInflater.from(context);
		view = inflater.inflate(R.layout.matrix_expand, null);
		final View newRow = view.findViewById(R.id.matrix_newrow);
		newRow.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				MatrixAdapter.this.newRow();
			}
		});
		final View newCol = view.findViewById(R.id.matrix_newcol);
		newCol.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				MatrixAdapter.this.newColumn();
			}
		});
		return view;
	}

	private static void copy(final String[][] source, final String[][] dest){
		final int len1 = Math.min(source.length, dest.length);
		final int len2 = Math.min(source[0].length, dest[0].length);
		for(int i = 0; i < len1; i++)
			for(int j = 0; j < len2; j++)
				dest[i][j] = source[i][j];
	}

	private void newRow(){
		final String[][] newdata = new String[rows + 1][columns];
		copy(data, newdata);
		for(int i = 0; i < columns; i++)
			newdata[rows][i] = "";
		data = newdata;
		rows++;
		notifyDataSetChanged();
	}

	private void newColumn(){
		final String[][] newdata = new String[rows][columns + 1];
		copy(data, newdata);
		for(int i = 0; i < rows; i++)
			newdata[i][columns] = "";
		data = newdata;
		columns++;
		grid.setNumColumns(columns);
		notifyDataSetChanged();
	}

	private void deleteRow(final int index){
		final String[][] newdata = new String[rows - 1][columns];
		for(int i = 0; i < rows - 1; i++)
			if(i < index)
				newdata[i] = data[i];
			else
				newdata[i] = data[i + 1];
		data = newdata;
		rows--;
		notifyDataSetChanged();
	}

	private void deleteColumn(final int index){
		final String[][] newdata = new String[rows][columns - 1];
		for(int i = 0; i < rows; i++)
			for(int j = 0; j < columns - 1; j++)
				if(j < index)
					newdata[i][j] = data[i][j];
				else
					newdata[i][j] = data[i][j + 1];
		data = newdata;
		columns--;
		grid.setNumColumns(columns);
		notifyDataSetChanged();
	}

	public int getColumns(){
		return columns - 1;
	}

	public int getRows(){
		return rows - 1;
	}

	public String get(final int i, final int j){
		return data[i][j] == null ? "0" : data[i][j];
	}

	public String getString(){
		final StringBuilder sb = new StringBuilder();
		sb.append(data.length);
		sb.append(",,");
		sb.append(data[0].length);
		sb.append(",,");
		for(final String[] dataslice : data)
			for(final String datum : dataslice){
				sb.append(datum);
				sb.append(",,");
			}
		return sb.toString();
	}

	public void setString(final String values){
		if(values == null)
			return;
		final String[] split = values.split(",,");
		rows = Integer.parseInt(split[0]);
		columns = Integer.parseInt(split[1]);
		data = new String[rows][columns];
		for(int i = 0; i < rows; i++)
			for(int j = 0; j < columns; j++)
				if(split.length > i * columns + j + 2)
					data[i][j] = split[i * columns + j + 2];
				else
					data[i][j] = "";
		notifyDataSetChanged();
	}
}
