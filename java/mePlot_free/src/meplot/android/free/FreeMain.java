package meplot.android.free;

import meplot.android.R;
import meplot.android.gui.activities.Main;
import android.os.Bundle;

import com.google.ads.AdRequest;
import com.google.ads.AdView;

public final class FreeMain extends Main{
	private void setupAds(){
		final AdView adView = (AdView)findViewById(R.id.main_ad);
		if(adView != null){
			final AdRequest adRequest = new AdRequest();
			adRequest.addTestDevice("7C41025BEBD9B1BDB24B6E8E9A4A95E0");
			adView.loadAd(adRequest);
		}
	}

	@Override
	public void onCreate(final Bundle savedState){
		super.onCreate(savedState);

		setupAds();
	}
}
