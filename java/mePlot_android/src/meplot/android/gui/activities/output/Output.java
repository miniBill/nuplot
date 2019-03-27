package meplot.android.gui.activities.output;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import meplot.android.R;
import meplot.android.graphics.CanvasGraphics;
import meplot.android.graphics.DrawView;
import meplot.android.graphics.OverlayView;
import meplot.expressions.Expression;
import meplot.graphics.DrawController;
import meplot.graphics.graphs.NormalGraph;
import platform.log.Log;
import platform.log.LogLevel;
import android.app.Activity;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Bitmap.Config;
import android.graphics.Canvas;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.Spinner;
import android.widget.TextView;

public class Output extends Activity{
	private class ProgressThread extends Thread{
		private static final int STATE_RUNNING = 1;

		private final Handler handler = new Handler(){
			public void handleMessage(final Message msg){
				final int total = msg.arg1;
				progressDialog.setProgress(total);
				if(total == 200)
					dismissDialog(PROGRESS_DIALOG);
			}
		};
		private int state;

		public void run(){
			new Thread(){
				private static final int STATE_DONE = 0;

				@Override
				public void run(){
					try{
						final Uri uri = saveToCache();
						state = STATE_DONE;
						if(uri != null)
							share(uri);
					}
					catch(final IOException ex){
						Log.log(ex);
					}
				}
			}.start();

			state = STATE_RUNNING;
			while(state == STATE_RUNNING){
				try{
					// ESCA-JAVA0087:
					Thread.sleep(100);
				}
				catch(final InterruptedException e){
					Log.log(LogLevel.ERROR, "Thread Interrupted");
				}
				final Message msg = handler.obtainMessage();
				if(shareDController != null)
					msg.arg1 = shareDController.getProgress();
				handler.sendMessage(msg);
			}

			final Message msg = handler.obtainMessage();
			msg.arg1 = 200;
			handler.sendMessage(msg);
		}
	}

	private static final int GOTO_DIALOG = 1;

	private static final int PROGRESS_DIALOG = 0;

	private Dialog gotoDialog;
	private ProgressDialog progressDialog;
	private DrawController shareDController;
	private Spinner spinner;
	private DrawView view;
	private TextView xText;
	private TextView yText;

	private Dialog getGotoDialog(){
		gotoDialog = new Dialog(this);
		gotoDialog.setContentView(R.layout.output_goto);
		gotoDialog.getWindow().setLayout(LayoutParams.FILL_PARENT,
				LayoutParams.WRAP_CONTENT);
		gotoDialog.setTitle("Goto");

		xText = (TextView)gotoDialog.findViewById(R.id.goto_x);
		xText.addTextChangedListener(new TextWatcher(){
			@Override
			public void afterTextChanged(final Editable s){
				if(yText.isEnabled())
					return;
				updateLabelInGoto();
			}

			@Override
			public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after){
			}

			@Override
			public void onTextChanged(final CharSequence s, final int start, final int before, final int count){
			}
		});

		yText = (TextView)gotoDialog.findViewById(R.id.goto_y);

		spinner = (Spinner)gotoDialog.findViewById(R.id.goto_spinner);
		spinner.setAdapter(new NormalGraphsAdapter(view.getNormalExplicitGraphs(), this));
		spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener(){
			@Override
			public void onItemSelected(final AdapterView<?> parent, final View view, final int position,
					final long id){
				if(position == 0)
					yText.setEnabled(true);
				else{
					yText.setEnabled(false);
					updateLabelInGoto();
				}
			}

			@Override
			public void onNothingSelected(final AdapterView<?> arg0){
				yText.setEnabled(true);
			}
		});

		final Button ok = (Button)gotoDialog.findViewById(R.id.goto_ok);
		ok.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View arg0){
				try{
					final String xstring = xText.getText().toString();
					final double cx = Double.parseDouble(xstring);
					final String ystring = yText.getText().toString();
					final double cy = Double.parseDouble(ystring);

					view.setCenter(cx, -cy);
				}
				catch(final NumberFormatException ex){
				}
				dismissDialog(GOTO_DIALOG);
			}
		});

		final Button cancel = (Button)gotoDialog.findViewById(R.id.goto_cancel);
		cancel.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(final View v){
				dismissDialog(GOTO_DIALOG);
			}
		});

		return gotoDialog;
	}

	private Bitmap getImg(){
		final int width = view.getWidth();
		final int height = view.getHeight();
		if(width * height == 0)
			return null;
		final Bitmap out = Bitmap.createBitmap(width, height, Config.ARGB_8888);
		final Canvas canvas = new Canvas();
		canvas.setBitmap(out);
		final CanvasGraphics graphics = new CanvasGraphics(canvas);
		shareDController = new DrawController();
		shareDController.syncroPaint(graphics, view.getGraphList(), width, height, 1);
		return out;
	}

	@Override
	protected final void onCreate(final Bundle savedState){
		super.onCreate(savedState);

		requestWindowFeature(Window.FEATURE_NO_TITLE);
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
				WindowManager.LayoutParams.FLAG_FULLSCREEN);

		setContentView(R.layout.output);

		view = (DrawView)findViewById(R.id.output_view);
		view.setOverlay((OverlayView)findViewById(R.id.output_overlay));
		final Intent caller = getIntent();
		if(caller != null)
			view.setFunctions(caller.getStringArrayExtra("Equations"));

		if(savedState != null)
			view.setData(savedState.getDoubleArray("Data"));

		view.postInvalidate();
	}

	@Override
	protected Dialog onCreateDialog(final int id){
		if(id == PROGRESS_DIALOG){
			progressDialog = new ProgressDialog(this);
			progressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			progressDialog.setMessage("Generating image...");
			return progressDialog;
		}
		if(id == GOTO_DIALOG)
			return getGotoDialog();

		return null;
	}

	@Override
	public final boolean onCreateOptionsMenu(final Menu menu){
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.output_menu, menu);
		return true;
	}

	@Override
	public final boolean onOptionsItemSelected(final MenuItem item){
		if(item.getItemId() == R.id.menu_reset_zoom)
			view.resetZoom();
		else
			if(item.getItemId() == R.id.menu_share)
				share();
			else
				if(item.getItemId() == R.id.menu_goto)
					showDialog(GOTO_DIALOG);
				else
					Log.log(LogLevel.DEBUG,
							"Unknown option selected in Draw: " + item.getItemId());
		return true;
	}

	@Override
	protected final void onPause(){
		super.onPause();
		view.stopDrawing();
	}

	@Override
	protected void onPrepareDialog(final int id, final Dialog dialog){
		if(id == PROGRESS_DIALOG){
			progressDialog.setProgress(0);
			final ProgressThread progressThread = new ProgressThread();
			progressThread.start();
			return;
		}

		if(id == GOTO_DIALOG){
			xText.setText(Double.toString(view.getCx()));
			yText.setText(Double.toString(view.getCy()));
		}
	}

	@Override
	protected final void onResume(){
		super.onResume();
		view.resumeDrawing();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState){
		super.onSaveInstanceState(outState);
		outState.putDoubleArray("Data", view.getData());
	}

	private Uri saveToCache() throws IOException{
		final String filename = "output" + view.getFunctions().hashCode() + ".png";
		final FileOutputStream oStream = getApplication().openFileOutput(filename,
				MODE_WORLD_READABLE);
		final Bitmap img = getImg();
		if(img == null)
			return null;
		img.compress(Bitmap.CompressFormat.PNG, 100, oStream);
		oStream.close();

		final File file = getApplication().getFileStreamPath(filename);
		return Uri.fromFile(file);
	}

	private void share(){
		showDialog(PROGRESS_DIALOG);
	}

	private void share(final Uri uri){
		final Intent sharingIntent = new Intent(Intent.ACTION_SEND);

		sharingIntent.setType("image/png");
		sharingIntent.putExtra(Intent.EXTRA_STREAM, uri);
		startActivity(Intent.createChooser(sharingIntent, "Share image using"));
	}

	private void updateLabelInGoto(){
		final Object item = spinner.getSelectedItem();
		if(item instanceof NormalGraph){
			final NormalGraph nitem = (NormalGraph)item;
			final Expression expr = nitem.getExpression();
			final String xString = xText.getText().toString();
			try{
				final double xValue = Double.parseDouble(xString);
				final double fValue = expr.dvalue(nitem.isRadial() ? 't' : 'x', xValue);
				final String fString = Double.toString(fValue);
				yText.setText(fString);
			}
			catch(final NumberFormatException e){
			}
		}
	}
}
