package platform.android.persistence;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import platform.persistence.disk.AbstractFile;

class AndroidFile extends AbstractFile{
	private final java.io.File file;

	AndroidFile(final java.io.File file){
		this.file = file;
	}

	@Override
	public String getName(){
		return file.getName();
	}

	@Override
	public InputStream openInputStream(){
		try{
			return new FileInputStream(file);
		}
		catch(final FileNotFoundException e){
			return null;
		}
	}
}
