package meplot.android.gui.activities;

import java.util.Stack;

import meplot.android.R;
import meplot.help.GuidePages;
import platform.log.Log;
import platform.log.LogLevel;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.webkit.WebView;
import android.webkit.WebViewClient;

public final class Guide extends Activity{
	private final class GuideWebViewClient extends WebViewClient{
		@Override
		public boolean shouldOverrideUrlLoading(final WebView view, final String url){
			loadUrl(url);
			return true;
		}
	}

	private void loadUrl(final String url){
		final String pageName = url.substring(url.lastIndexOf('/') + 1);
		final String page;
		if(url.contains("functions"))
			page = GuidePages.getPage("functions/" + pageName);
		else
			page = GuidePages.getPage(pageName);
		if(page.length() > 0){
			history.push(url);
			webView.loadDataWithBaseURL("file:///android_asset/", page, "text/html",
					"utf-8", null);
		}
		else
			webView.loadData("<h1>ERROR LOADING GUIDE. CRY AND RUN IN DESPAIR.</h1>",
					"text/html", "utf-8");
	}

	private WebView webView;

	@Override
	protected void onCreate(final Bundle savedState){
		super.onCreate(savedState);
		setContentView(R.layout.guide);

		webView = (WebView)findViewById(R.id.guide_html);
		webView.setWebViewClient(new GuideWebViewClient());
		webView.setBackgroundColor(0);

		loadUrl("guide");
	}

	private final Stack<String> history = new Stack<String>();

	@Override
	public boolean onKeyDown(final int keyCode, final KeyEvent event){
		if(keyCode == KeyEvent.KEYCODE_BACK)
			if(goBack())
				return true;
		return super.onKeyDown(keyCode, event);
	}

	private boolean goBack(){
		if(history.isEmpty())
			return false;
		history.pop();
		if(!history.isEmpty()){
			final String url = history.pop();
			loadUrl(url);
			return true;
		}
		return false;
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu){
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.guide_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item){
		if(item.getItemId() == R.id.menu_back){
			if(!goBack())
				goHome();
		}
		else
			if(item.getItemId() == R.id.menu_home)
				goHome();
			else
				Log.log(LogLevel.DEBUG,
						"Unknown option selected in Guide: " + item.getItemId());
		return true;
	}

	private void goHome(){
		final Intent intent = new Intent(this, Main.class);
		intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
		startActivity(intent);
	}
}
