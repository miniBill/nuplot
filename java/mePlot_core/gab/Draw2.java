package meplot.daplot.gab;

import meplot.daplot.R;
import meplot.daplot.gab.plotter.Engine3DView;
import meplot.daplot.gui.Main;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.LinearLayout;

public class Draw2 implements OnClickListener{
	// private Button btAggiungi = null;
	private Main activity;
	// private TableLayout tlInput = null;
	private LinearLayout tlDraw = null;

	// //////////////////////////////////////////////////////////
	/*
	 * private void addButton(){ // create a new TableRow TableRow row = new
	 * TableRow(activity); // create a new EditText EditText t = new
	 * EditText(activity); // create a CheckBox CheckBox c = new
	 * CheckBox(activity);
	 *
	 * // add the TextView and the CheckBox to the new TableRow row.addView(t);
	 * row.addView(c);
	 *
	 * // add the TableRow to the TableLayout tlInput.addView(row, new
	 * TableLayout.LayoutParams(
	 * android.view.ViewGroup.LayoutParams.WRAP_CONTENT,
	 * android.view.ViewGroup.LayoutParams.WRAP_CONTENT));
	 *
	 * }
	 */

	// //////////////////////////////////////////////////////////

	// Costruttore
	public Draw2(final Button btDraw, final Main activity){
		this.activity = activity;
		btDraw.setOnClickListener(this);
	}

	public void reDraw(final Button btDraw, final Main activity){
		this.activity = activity;
		btDraw.setOnClickListener(this);
	}

	// crea la pagina
	public void makeDraw(){
		activity.setContentView(R.layout.equations);

		// tlInput = (TableLayout)activity.findViewById(R.id.TableLayout_dr);
		tlDraw = null; // TODO: Fix hack
		// (LinearLayout)activity.findViewById(R.id.LinearLayout);
		// area openGl-disegno
		final Engine3DView view = new Engine3DView(activity);
		tlDraw.addView(view, new LinearLayout.LayoutParams(
				android.view.ViewGroup.LayoutParams.FILL_PARENT, 400, 5));
		// create a new TableRow
		/*
		 * btAggiungi = (Button)activity.findViewById(R.id.addinp_dr);
		 * btAggiungi.setOnClickListener(new OnClickListener(){ public void
		 * onClick(View v){ addButton(); } })
		 */
	}

	// evento click
	@Override
	public final void onClick(final View v){
		makeDraw();
	}
}
