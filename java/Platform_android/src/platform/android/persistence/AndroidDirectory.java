package platform.android.persistence;

import java.io.File;

import platform.persistence.disk.Directory;
import platform.persistence.disk.FileIterator;
import platform.persistence.disk.FileList;

class AndroidDirectory implements Directory{
	private final File file;

	AndroidDirectory(final File directory){
		file = directory;
	}

	@Override
	public FileIterator getFiles(){
		if(!file.exists() && !file.mkdirs())
			return FileIterator.EMPTY;
		final File[] files = file.listFiles();
		if(files == null || files.length == 0)
			return FileIterator.EMPTY;
		final FileList toret = new FileList();
		for(final File curr : files)
			if(curr.isFile())
				toret.add(new AndroidFile(curr));
		return toret.getIterator();
	}

	@Override
	public Directory getSubdirectory(final String string){
		return new AndroidDirectory(new File(file, string));
	}

	@Override
	public String toString(){
		return file.getAbsolutePath();
	}
}
