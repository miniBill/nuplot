package meplot.localization;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;
import java.util.Vector;

public final class Localizator{
	private Localizator(){

	}

	public static void main(final String[] args){
		final String language = "it";

		final List<String> text = new Vector<String>();

		try{
			final BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

			for(int c = 0; c < L10N.count(); c++){
				System.out.print(L10N.get(c) + ": ");
				text.add(in.readLine());
			}

			// write it for easy reading in J2ME
			final OutputStream out = new FileOutputStream(language + ".res");
			final DataOutputStream dout = new DataOutputStream(out);
			try{
				// first item is the number of strings
				dout.writeShort(text.size());
				// then the string themselves
				for(String s : text)
					if(s != null)
						dout.writeUTF(s);
			}
			finally{
				dout.close();
			}
		}
		catch(final IOException e){
			// ESCA-JAVA0265:
			e.printStackTrace();
		}
	}
}
