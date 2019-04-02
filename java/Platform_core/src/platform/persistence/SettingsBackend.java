package platform.persistence;

public interface SettingsBackend{
	String loadString(String name, String ifEmpty);

	boolean loadBoolean(String name, boolean ifEmpty);

	int loadInt(String name, int ifEmpty);

}
