package platform.persistence;

import platform.Platform;
import platform.lists.List;
import platform.persistence.listeners.BooleanSettingsListener;
import platform.persistence.listeners.IntSettingsListener;
import platform.persistence.listeners.StringSettingsListener;

public final class Persistence {
	private static final List<BooleanSettingsListener> BOOLEANLISTENERS = new List<>();
	private static final List<StringSettingsListener> STRINGLISTENERS = new List<>();
	private static final List<IntSettingsListener> INTLISTENERS = new List<>();

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

	private Persistence() {
	}
}
