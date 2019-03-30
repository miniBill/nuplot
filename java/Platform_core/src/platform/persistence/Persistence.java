package platform.persistence;

import platform.Platform;
import platform.lists.IIterator;
import platform.lists.List;
import platform.persistence.listeners.BooleanSettingsListener;
import platform.persistence.listeners.IntSettingsListener;
import platform.persistence.listeners.StringSettingsListener;

public final class Persistence {
	private static final List<BooleanSettingsListener> BOOLEANLISTENERS = new List<BooleanSettingsListener>();
	private static final List<StringSettingsListener> STRINGLISTENERS = new List<StringSettingsListener>();
	private static final List<IntSettingsListener> INTLISTENERS = new List<IntSettingsListener>();

	public static void changedSetting(final String name, final boolean arg) {
		final IIterator<BooleanSettingsListener> iterator = BOOLEANLISTENERS.getIterator();
		while (iterator.hasNext()) {
			final BooleanSettingsListener next = iterator.next();
			next.changedSetting(name, arg);
		}
	}

	public static void changedSetting(final String name, final int arg) {
		final IIterator<IntSettingsListener> iterator = INTLISTENERS.getIterator();
		while (iterator.hasNext()) {
			final IntSettingsListener next = iterator.next();
			next.changedSetting(name, arg);
		}
	}

	public static void changedSetting(final String name, final String arg) {
		final IIterator<StringSettingsListener> iterator = STRINGLISTENERS.getIterator();
		while (iterator.hasNext()) {
			final StringSettingsListener next = iterator.next();
			next.changedSetting(name, arg);
		}
	}

	private static SettingsBackend getBackend() {
		return Platform.getSettingsBackend();
	}

	public static boolean loadBoolean(final String name) {
		return loadBoolean(name, false);
	}

	public static boolean loadBoolean(final String name, final boolean ifEmpty) {
		return getBackend().loadBoolean(name, ifEmpty);
	}

	public static int loadInt(final String name) {
		return loadInt(name, 0);
	}

	public static int loadInt(final String name, final int ifEmpty) {
		return getBackend().loadInt(name, ifEmpty);
	}

	public static String loadString(final String name) {
		return loadString(name, "");
	}

	public static String loadString(final String name, final String ifEmpty) {
		return getBackend().loadString(name, ifEmpty);
	}

	public static void registerListener(final BooleanSettingsListener listener) {
		BOOLEANLISTENERS.add(listener);
	}

	public static void registerListener(final IntSettingsListener listener) {
		INTLISTENERS.add(listener);
	}

	public static void registerListener(final StringSettingsListener listener) {
		STRINGLISTENERS.add(listener);
	}

	public static void saveBoolean(final String name, final boolean value) {
		getBackend().saveBoolean(name, value);
		Persistence.changedSetting(name, value);
	}

	public static void saveInt(final String name, final int arg) {
		getBackend().saveInt(name, arg);
		Persistence.changedSetting(name, arg);
	}

	public static void saveString(final String name, final String value) {
		saveString(name, value, true);
	}

	private static void saveString(final String name, final String value, final boolean fireEvent) {
		getBackend().saveString(name, value);
		if (fireEvent)
			changedSetting(name, value);
	}

	private Persistence() {
	}
}
