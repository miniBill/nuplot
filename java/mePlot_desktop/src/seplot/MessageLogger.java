package seplot;

import javax.swing.JOptionPane;

import platform.log.LogLevel;
import platform.log.MyLogger;

class MessageLogger implements MyLogger{

	public void println(final LogLevel level, final String string){
		JOptionPane.showMessageDialog(null, string);
	}
}