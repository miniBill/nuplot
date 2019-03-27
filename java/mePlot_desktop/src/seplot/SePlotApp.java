/*
 * MePlotApp.java
 */
package seplot;

import org.jdesktop.application.Application;
import org.jdesktop.application.SingleFrameApplication;

/**
 * The main class of the application.
 * @author Leonardo Taglialegne
 */
public class SePlotApp extends SingleFrameApplication{
	/**
	 * At startup create and show the main frame of the application.
	 */
	@Override
	protected void startup(){
		show(new SePlotView(this));
	}

	// ESCA-JAVA0025:
	/**
	 * This method is to initialize the specified window by injecting resources.
	 * Windows shown in our application come fully initialized from the GUI
	 * builder, so this additional configuration is not needed.
	 */
	@Override
	protected void configureWindow(final java.awt.Window root){
	}

	/**
	 * A convenient static getter for the application instance.
	 *
	 * @return the instance of MePlotApp
	 */
	public static SePlotApp getApplication(){
		return Application.getInstance(SePlotApp.class);
	}

	/**
	 * Main method launching the application.
	 * @param args The arguments to the application
	 */
	public static void main(final String[] args){
		launch(SePlotApp.class, args);
	}
}
