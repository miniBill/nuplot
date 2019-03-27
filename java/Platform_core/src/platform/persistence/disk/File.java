package platform.persistence.disk;

import java.io.InputStream;

import platform.lists.IToString;

public interface File extends IToString{
	InputStream openInputStream();

	String getName();
}
