package platform.memory;

import platform.persistence.SettingsBackend;

class MemoryPersistence implements SettingsBackend{
	private static int indexof(final String[] array, final String search){
		for(int i = 0; i < array.length; i++)
			if(array[i].equals(search))
				return i;
		return -1;
	}
	private final String[] booleannames = new String[0];
	private boolean[] booleans = new boolean[0];
	private final String[] intnames = new String[0];
	private int[] ints = new int[0];
	private final String[] stringnames = new String[0];

	private String[] strings = new String[0];

	public boolean loadBoolean(final String name, final boolean ifEmpty){
		final int index = indexof(booleannames, name);
		if(index < 0)
			return ifEmpty;
		return booleans[index];
	}

	public int loadInt(final String name, final int ifEmpty){
		final int index = indexof(intnames, name);
		if(index < 0)
			return ifEmpty;
		return ints[index];
	}

	public String loadString(final String name, final String ifEmpty){
		final int index = indexof(stringnames, name);
		if(index < 0)
			return ifEmpty;
		return strings[index];
	}

	public void saveBoolean(final String name, final boolean value){
		int index = indexof(booleannames, name);
		if(index < 0){
			final boolean[] newbooleans= new boolean[booleans.length + 1];
			for(int j = 0; j < booleans.length; j++)
				newbooleans[j] = booleans[j];
			index = booleans.length;
			booleans = newbooleans;
		}
		booleans[index] = value;
	}

	public void saveInt(final String name, final int arg){
		int index = indexof(intnames, name);
		if(index < 0){
			final int[] newints = new int[ints.length + 1];
			for(int j = 0; j < ints.length; j++)
				newints[j] = ints[j];
			index = ints.length;
			ints = newints;
		}
		ints[index] = arg;
	}

	public void saveString(final String name, final String value){
		int index = indexof(stringnames, name);
		if(index < 0){
			final String[] newstrings = new String[strings.length + 1];
			for(int j = 0; j < strings.length; j++)
				newstrings[j] = strings[j];
			index = strings.length;
			strings = newstrings;
		}
		strings[index] = value;
	}
}
