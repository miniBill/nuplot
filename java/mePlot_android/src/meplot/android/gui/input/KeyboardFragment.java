package meplot.android.gui.input;

import meplot.android.R;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;

public final class KeyboardFragment extends Fragment{
	private int num;
	private boolean forcePortrait;
	private static OnClickListener clickListener;
	private static OnTouchListener touchListener;

	public static KeyboardFragment newInstance(final int num,
			final OnClickListener listener, final OnTouchListener tlistener,
			final boolean forcePortrait){
		clickListener = listener;
		touchListener = tlistener;
		final KeyboardFragment fragment = new KeyboardFragment();

		final Bundle args = new Bundle();
		args.putInt("num", num % 3);
		args.putBoolean("force_port", forcePortrait);
		fragment.setArguments(args);

		return fragment;
	}

	@Override
	public void onCreate(final Bundle savedState){
		super.onCreate(savedState);
		num = getArguments() == null ? 1 : getArguments().getInt("num");
		forcePortrait = getArguments() == null ? false : getArguments()
				.getBoolean("force_port");
	}

	@Override
	public View onCreateView(final LayoutInflater inflater,
			final ViewGroup container, final Bundle savedState){
		final int layout = getLayoutId();
		final View toret = inflater.inflate(layout, container, false);
		if(toret instanceof ViewGroup){
			final ViewGroup viewGroup = (ViewGroup)toret;
			InputInitializer.setOnClickListener(viewGroup, clickListener,
					touchListener);
		}
		return toret;
	}

	private int getLayoutId(){
		if(forcePortrait)
			switch(num){
				case 0:
					return R.layout.keyboard_dialpad_port;
				case 1:
					return R.layout.keyboard_functions1_port;
				default:
					return R.layout.keyboard_functions2_port;
			}
		switch(num){
			case 0:
				return R.layout.keyboard_dialpad;
			case 1:
				return R.layout.keyboard_functions1;
			default:
				return R.layout.keyboard_functions2;
		}
	}
}
