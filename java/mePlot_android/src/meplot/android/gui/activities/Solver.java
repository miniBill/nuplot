package meplot.android.gui.activities;

import meplot.android.R;
import meplot.android.gui.input.InputReceiver;
import meplot.expressions.Expression;
import meplot.expressions.exceptions.CalcException;
import meplot.help.GuidePages;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import meplot.parser.utils.Cleaner;
import meplot.solver.Solution;
import platform.log.Log;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup.LayoutParams;
import android.webkit.WebView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;

public class Solver extends InputReceiver{
	private class SelectTouchListener implements OnTouchListener{
		private final boolean value;

		private SelectTouchListener(final boolean value){
			this.value = value;
		}

		@Override
		public boolean onTouch(final View v, final MotionEvent event){
			focus = value;
			final LayoutParams params = inputField.getLayoutParams();
			if(params instanceof LinearLayout.LayoutParams){
				final LinearLayout.LayoutParams lparams = (LinearLayout.LayoutParams)params;
				if(focus)
					lparams.weight = 2;
				else
					if(isCustomIme())
						lparams.weight = 0.5f;
					else
						lparams.weight = 1f;
				inputField.setLayoutParams(lparams);
			}
			setImeVisibility();
			return false;
		}

	}

	private final meplot.solver.Solver solver = new meplot.solver.Solver();

	public Solver(){
		super(true);
	}

	private final class ResolveListener implements OnClickListener{
		@Override
		public void onClick(final View view){
			final StringBuffer buffer = new StringBuffer(GuidePages.HEADER);
			try{
				final Expression input = Parser.parse(inputField.getText().toString());
				if(input == null)
					buffer.append("<div style=\"color:red\">Calculation error</div>");
				else{
					try{
						final Solution sol = solve(input);
						sol.toHtml(buffer);
					}
					catch(CalcException ce){
						buffer.append("<div style=\"color:red\">Calculation error: ");
						buffer.append(ce);
						buffer.append("</div>");
					}
				}
			}
			catch(final ParserException e){
				Log.log(e);
				buffer.append("<div style=\"color:red\">Parse Error</div>");
			}
			setContent(webView, buffer);
		}

		private void setContent(final WebView webView, final StringBuffer buffer){
			buffer.append("</body></html>");
			final String clean = Cleaner.clean(buffer.toString());
			webView.loadDataWithBaseURL("file:///android_asset/", clean, "text/html", "utf-8", null);
		}
	}

	private boolean focus = true;

	@Override
	protected boolean isTextviewSelected(){
		return focus;
	}

	protected Solution solve(final Expression input){
		return solver.solve(input);
	}

	private EditText inputField;
	private WebView webView;

	@Override
	protected final void onAfterCreate(final Bundle savedState){
		setContentView(R.layout.solver);

		final Button btResolve = (Button)findViewById(R.id.solver_solve);
		if(btResolve != null)
			btResolve.setOnClickListener(new ResolveListener());

		inputField = (EditText)findViewById(R.id.solver_input);
		inputField.setOnTouchListener(new SelectTouchListener(true));

		webView = (WebView)findViewById(R.id.solver_output);
		webView.getSettings().setJavaScriptEnabled(true);
		webView.setOnTouchListener(new SelectTouchListener(false));
	}

	@Override
	protected View getKeyboardButton(){
		return findViewById(R.id.solver_ime);
	}
}
