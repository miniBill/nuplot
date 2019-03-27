package platform.persistence.disk;

public abstract class AbstractFile implements File{
	public final String toString(){
		return getName();
	}

	public final void toString(final StringBuffer buffer){
		buffer.append(getName());
	}
}