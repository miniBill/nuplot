package platform.javax;

import javax.microedition.rms.InvalidRecordIDException;
import javax.microedition.rms.RecordStore;
import javax.microedition.rms.RecordStoreException;
import javax.microedition.rms.RecordStoreFullException;
import javax.microedition.rms.RecordStoreNotFoundException;
import javax.microedition.rms.RecordStoreNotOpenException;

import platform.log.Log;
import platform.log.LogLevel;
import platform.persistence.SettingsBackend;

class MeBackend implements SettingsBackend{
	public int loadInt(final String name, final int ifEmpty){
		final String loaded = loadString(name, "");
		if(loaded.length() == 0)
			return ifEmpty;
		return Integer.parseInt(loaded, 16);
	}

	public String loadString(final String name, final String ifEmpty){
		final byte[] val = loadBytes(name);
		if(val == null || val.length == 0)
			return ifEmpty;
		return new String(val);
	}

	private static byte[] loadBytes(final String name){
		try{
			final RecordStore recordStore = RecordStore.openRecordStore(name,
					true);
			byte[] data = null;
			if(recordStore.getNumRecords() > 0)
				data = recordStore.getRecord(1);
			recordStore.closeRecordStore();
			return data;
		}
		catch(final RecordStoreFullException e){
			Log.log(LogLevel.INFO, e.toString());
		}
		catch(final RecordStoreNotFoundException e){
			Log.log(LogLevel.WARNING, e.toString());
		}
		catch(final RecordStoreException e){
			Log.log(LogLevel.ERROR, e.toString());
		}
		Log.log(LogLevel.INFO, "Persistence fail");
		return new byte[0];
	}

	private static void saveBytes(final String name, final byte[] data){
		try{
			final RecordStore recordStore = RecordStore.openRecordStore(name,
					true);
			if(recordStore.getNumRecords() > 0)
				recordStore.setRecord(1, data, 0, data.length);
			else
				recordStore.addRecord(data, 0, data.length);
			recordStore.closeRecordStore();
		}
		catch(final RecordStoreNotOpenException e){
			Log.log(LogLevel.ERROR, e.toString());
		}
		catch(final InvalidRecordIDException e){
			Log.log(LogLevel.ERROR, e.toString());
		}
		catch(final RecordStoreFullException e){
			Log.log(LogLevel.ERROR, e.toString());
		}
		catch(final RecordStoreException e){
			Log.log(LogLevel.ERROR, e.toString());
		}
	}

	public void saveString(final String name, final String value){
		final byte[] data = value.getBytes();
		saveBytes(name, data);
	}

	public void saveInt(final String name, final int arg){
		saveString(name, Integer.toHexString(arg));
	}

	public void saveBoolean(final String name, final boolean value){
		saveInt(name, value ? 1 : 0);
	}

	public boolean loadBoolean(final String name, final boolean ifEmpty){
		return loadInt(name, ifEmpty ? 1 : 0) == 1;
	}
}
