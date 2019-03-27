package meplot.android.gui.input;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.TextView;

import com.viewpagerindicator.CirclePageIndicator;

final class InputInitializer{
	private InputInitializer(){

	}

	private static final class MyAdapter extends FragmentPagerAdapter{
		private final OnClickListener clickListener;
		private final OnTouchListener touchListener;
		private final boolean forcePortrait;

		private MyAdapter(final FragmentManager manager, final OnClickListener listener,
				final OnTouchListener tlistener, final boolean forcePortrait){
			super(manager);
			clickListener = listener;
			touchListener = tlistener;
			this.forcePortrait = forcePortrait;
		}

		@Override
		public int getCount(){
			return 5;
		}

		@Override
		public Fragment getItem(final int position){
			return KeyboardFragment.newInstance(position, clickListener, touchListener,
					forcePortrait);
		}
	}

	public static void init(final ViewPager pager, final ViewGroup ime,
			final InputReceiver context, final CirclePageIndicator indicator,
			final boolean forcePortrait){
		final OnClickListener clickListener = getListener(context);
		final OnTouchListener touchListener = getTListener(context);
		setOnClickListener(ime, clickListener, touchListener);

		final MyAdapter mAdapter = new MyAdapter(context.getSupportFragmentManager(),
				clickListener, touchListener, forcePortrait);
		pager.setAdapter(mAdapter);
		if(indicator != null){
			indicator.setViewPager(pager);
			indicator.setModulus(3);
			indicator.setCurrentItem(3);
			indicator.setOnPageChangeListener(new OnPageChangeListener(){
				@Override
				public void onPageSelected(final int arg0){
					if(arg0 == 4)
						indicator.setCurrentItem(1);
					else
						if(arg0 == 0)
							indicator.setCurrentItem(3);
				}

				@Override
				public void onPageScrolled(final int arg0, final float arg1,
						final int arg2){
				}

				@Override
				public void onPageScrollStateChanged(final int arg0){
				}
			});
		}
	}

	private static OnTouchListener getTListener(final InputReceiver context){
		return new OnTouchListener(){
			@Override
			public boolean onTouch(final View view, final MotionEvent event){
				// ESCA-JAVA0032:
				switch(event.getAction() & MotionEvent.ACTION_MASK){
					case MotionEvent.ACTION_DOWN:
						context.backspaceDown();
						break;
					case MotionEvent.ACTION_UP:
						context.backspaceUp();
						break;
				}
				return true;
			}
		};
	}

	private static OnClickListener getListener(final IInputReceiver context){
		final OnClickListener clickListener = new OnClickListener(){
			@Override
			public void onClick(final View view){
				final TextView text = (TextView)view;
				final StringBuilder buffer = new StringBuilder(text.getText().toString());
				if(buffer.length() > 1)
					buffer.append('(');
				context.input(buffer.toString());
			}
		};
		return clickListener;
	}

	public static void setOnClickListener(final ViewGroup view,
			final OnClickListener clickListener, final OnTouchListener touchListener){
		for(int i = 0; i < view.getChildCount(); i++){
			final View child = view.getChildAt(i);
			if(child instanceof ViewGroup)
				setOnClickListener((ViewGroup)child, clickListener, touchListener);
			if(child instanceof Button)
				child.setOnClickListener(clickListener);
			if(child instanceof ImageButton)
				child.setOnTouchListener(touchListener);
		}
	}
}
