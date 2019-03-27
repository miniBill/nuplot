/*
 * Copyright 2011 Google Inc. Licensed under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
 * or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

package com.google.android.apps.iosched.ui.widget;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;

/**
 * Custom layout that arranges children in a grid-like manner, optimizing for
 * even horizontal and vertical whitespace.
 *
 * @author Google Inc.
 */
public class DashboardLayout extends ViewGroup{

	private static final int BAD_GRID_PENALTY = 10;

	private int mMaxChildWidth;
	private int mMaxChildHeight;

	public DashboardLayout(final Context context){
		super(context, null);
	}

	public DashboardLayout(final Context context, final AttributeSet attrs){
		super(context, attrs, 0);
	}

	public DashboardLayout(final Context context, final AttributeSet attrs,
			final int defStyle){
		super(context, attrs, defStyle);
	}

	@Override
	protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec){
		mMaxChildWidth = 0;
		mMaxChildHeight = 0;

		// Measure once to find the maximum child size.

		int childWidth = MeasureSpec.makeMeasureSpec(
				MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.AT_MOST);
		int childHeight = MeasureSpec.makeMeasureSpec(
				MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.AT_MOST);

		final int count = getChildCount();
		for(int i = 0; i < count; i++){
			final View child = getChildAt(i);
			if(child.getVisibility() == GONE)
				continue;

			child.measure(childWidth, childHeight);

			mMaxChildWidth = Math.max(mMaxChildWidth, child.getMeasuredWidth());
			mMaxChildHeight = Math.max(mMaxChildHeight, child.getMeasuredHeight());
		}

		// Measure again for each child to be exactly the same size.

		childWidth = MeasureSpec.makeMeasureSpec(mMaxChildWidth, MeasureSpec.EXACTLY);
		childHeight = MeasureSpec.makeMeasureSpec(mMaxChildHeight, MeasureSpec.EXACTLY);

		for(int i = 0; i < count; i++){
			final View child = getChildAt(i);
			if(child.getVisibility() == GONE)
				continue;

			child.measure(childWidth, childHeight);
		}

		setMeasuredDimension(resolveSize(mMaxChildWidth, widthMeasureSpec),
				resolveSize(mMaxChildHeight, heightMeasureSpec));
	}

	@Override
	protected void onLayout(final boolean changed, final int leftarg, final int toparg,
			final int rightarg, final int bottomarg){
		final int count = getChildCount();

		// Calculate the number of visible children.
		int visibleCount = 0;
		for(int i = 0; i < count; i++){
			final View child = getChildAt(i);
			if(child.getVisibility() == GONE)
				continue;
			++visibleCount;
		}

		if(visibleCount == 0)
			return;

		// Calculate what number of rows and columns will optimize for even
		// horizontal and
		// vertical whitespace between items. Start with a 1 x N grid, then try
		// 2 x N, and so on.
		int bestSpaceDiff = Integer.MAX_VALUE;

		// Horizontal and vertical space between items
		int hSpace;
		int vSpace;

		int cols = 1;
		int rows;

		int width = rightarg - leftarg;
		int height = bottomarg - toparg;

		while(true){
			rows = (visibleCount - 1) / cols + 1;

			hSpace = (width - mMaxChildWidth * cols) / (cols + 1);
			vSpace = (height - mMaxChildHeight * rows) / (rows + 1);

			int spaceDifference = Math.abs(vSpace - hSpace);
			if(rows * cols != visibleCount || rows * mMaxChildHeight > height
					|| cols * mMaxChildWidth > width || rows * mMaxChildHeight > height)
				spaceDifference *= BAD_GRID_PENALTY;

			if(spaceDifference < bestSpaceDiff){
				// Found a better whitespace squareness/ratio
				bestSpaceDiff = spaceDifference;

				// If we found a better whitespace squareness and there's only 1
				// row, this is
				// the best we can do.
				if(rows == 1)
					break;
			}
			else{
				// This is a worse whitespace ratio, use the previous value of
				// cols and exit.
				--cols;
				rows = (visibleCount - 1) / cols + 1;
				hSpace = (width - mMaxChildWidth * cols) / (cols + 1);
				vSpace = (height - mMaxChildHeight * rows) / (rows + 1);
				break;
			}

			++cols;
		}

		// Lay out children based on calculated best-fit number of rows and
		// cols.

		// If we chose a layout that has negative horizontal or vertical space,
		// force it to zero.
		hSpace = Math.max(0, hSpace);
		vSpace = Math.max(0, vSpace);

		// Re-use width/height variables to be child width/height.
		width = (width - hSpace * (cols + 1)) / cols;
		height = (height - vSpace * (rows + 1)) / rows;

		for(int i = 0, visibleIndex = 0; i < count; i++, visibleIndex++){
			final View child = getChildAt(i);
			if(child.getVisibility() == GONE){
				visibleIndex--;
				continue;
			}

			final int row = visibleIndex / cols;
			final int col = visibleIndex % cols;

			final int left = hSpace * (col + 1) + width * col;
			final int top = vSpace * (row + 1) + height * row;

			child.layout(left, top, hSpace == 0 && col == cols - 1 ? rightarg
					: left + width, vSpace == 0 && row == rows - 1 ? bottomarg
					: top + height);
		}
	}
}
