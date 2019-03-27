package platform.persistence;

public interface SettingsBackend{
	String loadString(String name, String ifEmpty);

	void saveString(String name, String value);

	boolean loadBoolean(String name, boolean ifEmpty);

	int loadInt(String name, int ifEmpty);

	void saveInt(String name, int arg);

	void saveBoolean(String name, boolean value);
}
