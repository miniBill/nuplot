package platform.persistence.disk;

import platform.lists.EquatableList;
import platform.lists.IEquatableIterator;

public final class FileList extends EquatableList{
	public static final FileList EMPTY = new FileList();

	public void add(final File files){
		super.add(files);
	}

	public File elementAt(final int index){
		return (File)super.gelementAt(index);
	}

	public FileIterator getIterator(){
		return getIterator(0);
	}

	private FileIterator getIterator(int index){
		return new FileIterator(this, index);
	}

	public IEquatableIterator egetIterator(int index){
		return getIterator(index);
	}
}
