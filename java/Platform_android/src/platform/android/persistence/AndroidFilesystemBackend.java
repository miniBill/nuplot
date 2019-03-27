package platform.android.persistence;

import java.io.File;

import platform.persistence.disk.Directory;
import platform.persistence.disk.FilesystemBackend;
import android.os.Environment;

public class AndroidFilesystemBackend implements FilesystemBackend{
	@Override
	public Directory getApplicationDirectory(final String appname){
		final boolean extMemAvailable;
		final boolean extMemWriteable;
		final String state = Environment.getExternalStorageState();

		if(Environment.MEDIA_MOUNTED.equals(state)){
			// We can read and write the media
			extMemAvailable = true;
			extMemWriteable = true;
		}
		else
			if(Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)){
				// We can only read the media
				extMemAvailable = true;
				extMemWriteable = false;
			}
			else{
				// Something else is wrong. It may be one of many other states,
				// but all we need
				// to know is we can neither read nor write
				extMemAvailable = false;
				extMemWriteable = false;
			}

		// TODO:Fix
		if(!extMemAvailable || !extMemWriteable)
			return null;

		final File root = Environment.getExternalStorageDirectory();

		final File appDir = new File(root, "/Android/data/" + appname
				+ "/files/");

		return new AndroidDirectory(appDir);
	}

}
