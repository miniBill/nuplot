package platform.persistence;

public class NullBackend implements SettingsBackend{
	public boolean loadBoolean(final String name, final boolean ifEmpty){
		return ifEmpty;
	}

	public int loadInt(final String name, final int ifEmpty){
		return ifEmpty;
	}

	public String loadString(final String name, final String ifEmpty){
		return ifEmpty;
	}

	public void saveBoolean(final String name, final boolean value){
		// Null backend
	}

	public void saveInt(final String name, final int arg){
		// Null backend
	}

	public void saveString(final String name, final String value){
		// Null backend
	}
}
