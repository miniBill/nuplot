package meplot.persistence;

import meplot.localization.L10N;

public final class Settings{
	public static final String INVERTCONTROLS = "Invert";
	public static final String DRAWDERIVATIVES = "Derivatives";
	public static final String CLEANCONTOUR = "CleanContour";
	public static final String TRYCROSS = "TryCross";
	public static final String LANGUAGE = "Language";
	public static final String AUTOSCALE = "Autoscale";
	public static final String ORIGINAL = "Original";

	private static final Setting[] USERSETTINGS = new Setting[]{
			new Setting(INVERTCONTROLS, "Invert 3D controls", new String[]{
					L10N.get(L10N.NO), "Left-Right", "Up-Down", L10N.get(L10N.BOTH)}),
			new Setting(DRAWDERIVATIVES, "Draw derivatives", new String[]{
					L10N.get(L10N.NONE), "First", L10N.get(L10N.BOTH)}),
			new Setting(CLEANCONTOUR, "Draw zero line in contour and 3d plot",
					new String[]{L10N.get(L10N.YES), L10N.get(L10N.NO)}),
			new Setting(TRYCROSS, "Try cross simplification of divisions "
					+ "[EXPERIMENTAL, do not use with 3D plotting]", new String[]{
					L10N.get(L10N.NO), L10N.get(L10N.YES)}),
			new Setting(LANGUAGE, "Language", new String[]{"Italiano", "English"}),
			new Setting(AUTOSCALE, "Auto Scale", new String[]{L10N.get(L10N.NO),
					L10N.get(L10N.YES)}),
			new Setting(ORIGINAL, "Show original in grid plot", new String[]{
					L10N.get(L10N.YES), L10N.get(L10N.NO)})};

	private Settings(){
	}

	public static int indexof(final String name, final String search){
		for(int i = 0; i < USERSETTINGS.length; i++)
			if(USERSETTINGS[i].getName().equals(name))
				for(int j = 0; j < USERSETTINGS[i].getChoices().length; j++)
					if(USERSETTINGS[i].getChoices()[j].equals(search))
						return j;
		return -1;
	}

	public static Setting[] getUsersettings(){
		return USERSETTINGS;
	}
}
