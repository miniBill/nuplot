package meplot.gui;

import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.CommandListener;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Displayable;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.List;
import javax.microedition.lcdui.StringItem;
import javax.microedition.midlet.MIDlet;

import meplot.graphics.MeDrawCanvas;
import meplot.gui.help.HelpForm;
import meplot.localization.L10N;
import platform.Platform;
import platform.javax.AlertLogger;
import platform.javax.J2MEPlatform;
import platform.log.Log;
import platform.log.LogLevel;

public final class MidletMenju extends MIDlet implements CommandListener{
	// The MIDlet's Display object
	private static Display display;
	// Flag indicating first call of startApp
	private boolean started;

	protected void startApp(){
		if(!started){
			started = true;
			Platform.set(new J2MEPlatform());

			display = Display.getDisplay(this);
			AlertLogger.setDisplay(display);

			if(instance != null)
				Log.log(LogLevel.WARNING, "instance already opened?");
			instance = this;

			createCommands();

			createOptionsForm();

			createSymbolList();

			loadAndDisplayList();
		}
	}

	private static SymbolList symbolList;

	private void createSymbolList(){
		symbolList = new SymbolList(display, L10N.get(L10N.SYMBOLS) + ':');

		symbolList.loadDefaultList();

		symbolList.addCommand(backCommand);
		symbolList.setCommandListener(this);
	}

	protected void pauseApp(){
		// TODO: implement pause logic
	}

	private static Command backCommand;
	private Command exitCommand;

	private void createCommands(){
		final String back = L10N.get(L10N.BACK);
		final String exit = L10N.get(L10N.EXIT);

		backCommand = new Command(back, Command.BACK, 0);
		exitCommand = new Command(exit, Command.EXIT, 0);
	}

	private SettingsForm opForm;

	private void createOptionsForm(){
		opForm = new SettingsForm();
		opForm.addCommands(this);
	}

	private EquationForm eqForm;

	private void createEquationForm(){
		eqForm = new EquationForm();
		eqForm.addCommands(instance);
	}

	private AnalyzeForm dForm;

	private void createDForm(){
		dForm = new AnalyzeForm();
		dForm.addCommands(this);
	}

	public static MeDrawCanvas getDrawCanvas(){
		if(drawCanvas == null)
			createDrawCanvas();
		return drawCanvas;
	}

	private static MeDrawCanvas drawCanvas;

	private static void createDrawCanvas(){
		drawCanvas = new MeDrawCanvas();

		drawCanvas.addCommand(backCommand);

		drawCanvas.setCommandListener(instance);
	}

	private static MidletMenju instance;

	private SolverForm svForm;

	private void createSolverForm(){
		svForm = new SolverForm();
		svForm.addCommands(this);
	}

	private MatrixInputForm miForm;

	private void createMatrixInputForm(){
		miForm = new MatrixInputForm();
		miForm.addCommands(this);
	}

	private UserFunctionsForm ufForm;

	private void createUserFunctionsForm(){
		ufForm = new UserFunctionsForm();
		ufForm.addCommands(this);
	}

	private ThanksForm tkForm;

	private void createThanksForm(){
		tkForm = new ThanksForm();
		tkForm.addCommands(this);
	}

	private HelpForm hpForm;

	private void createHelpForm(){
		hpForm = new HelpForm();
		hpForm.addCommands(this);
	}

	public void commandAction(final Command command, final Displayable form){
		if(command.equals(exitCommand)){
			notifyDestroyed();
			return;
		}
		if(form.equals(menuList)){
			// ESCA-JAVA0166:
			try{
				handleMenuList();
			}
			catch(final Error e){
				StringItem string = new StringItem("Error", e.getMessage());
				Form f = new Form("Error");
				f.append(string);
				f.addCommand(backCommand);
				f.setCommandListener(this);
				display.setCurrent(f);
			}
		}

		if(fhandle(command, dForm) || fhandle(command, svForm)
				|| fhandle(command, eqForm) || fhandle(command, ufForm)
				|| fhandle(command, miForm) || fhandle(command, opForm)
				|| fhandle(command, tkForm) || fhandle(command, hpForm))
			return;
		if(handleBackCommand(command, form))
			return;
		if(form instanceof SymbolList
				&& ((SymbolList)form).isChildren(symbolList))
			handleSymbolList((SymbolList)form);
	}

	private static boolean fhandle(final Command command,
			final CommandHandler form){
		return form != null && form.handle(command);
	}

	private void handleSymbolList(final SymbolList list){
		final String val = list.push();
		if(val.length() > 0)
			if(symbolInD){
				dForm.appendSymbol(val);
				display.setCurrent(dForm);
			}
			else{
				eqForm.appendSymbol(val);
				display.setCurrent(eqForm);
				eqForm.afterAppend(display);
			}
	}

	private void handleMenuList(){
		final int index = menuList.getSelectedIndex();
		switch(index){
			case 0:
				if(eqForm == null)
					createEquationForm();
				display.setCurrent(eqForm);
				break;
			case 1:
				if(dForm == null)
					createDForm();
				display.setCurrent(dForm);
				break;
			case 2:
				if(svForm == null)
					createSolverForm();
				display.setCurrent(svForm);
				break;
			case 3:
				if(miForm == null)
					createMatrixInputForm();
				display.setCurrent(miForm);
				break;
			case 4:
				if(ufForm == null)
					createUserFunctionsForm();
				display.setCurrent(ufForm);
				break;
			case 5:
				if(opForm == null)
					createOptionsForm();
				display.setCurrent(opForm);
				break;
			case 6:
				if(tkForm == null)
					createThanksForm();
				display.setCurrent(tkForm);
				break;
			case 7:
				if(hpForm == null)
					createHelpForm();
				hpForm.reset();
				display.setCurrent(hpForm);
				break;
			default:
				break;
		}
	}

	private boolean handleBackCommand(final Command command,
			final Displayable form){
		if(command.equals(backCommand)){
			if(form instanceof SymbolList){
				if(form.equals(symbolList))
					if(symbolInD)
						display.setCurrent(dForm);
					else
						display.setCurrent(eqForm);
				else
					display.setCurrent(symbolList);
			}
			else
				if(form.equals(drawCanvas)){
					drawCanvas.stopDrawing();
					display.setCurrent(eqForm);
				}
				else
					display.setCurrent(menuList);
			return true;
		}
		return false;
	}

	private static boolean symbolInD;

	private List menuList;

	private void loadAndDisplayList(){
		menuList = new List(L10N.get(L10N.MAINTITLE), Choice.IMPLICIT);
		menuList.append(L10N.get(L10N.DRAW), null);
		menuList.append(L10N.get(L10N.ANALYZE), null);
		menuList.append(L10N.get(L10N.SOLVER), null);
		menuList.append(L10N.get(L10N.INPUTMATRIX), null);
		menuList.append(L10N.get(L10N.DEFINEFUNCTIONS), null);
		menuList.append(L10N.get(L10N.OPTIONS), null);
		menuList.append(L10N.get(L10N.ABOUT), null);
		menuList.append("Help", null);
		menuList.addCommand(exitCommand);
		menuList.setCommandListener(this);
		display.setCurrent(menuList);
	}

	protected void destroyApp(final boolean arg0){
		// TODO: implement end logic
	}

	public static void viewSymbolList(){
		display.setCurrent(symbolList);
	}

	public static void addMatrix(final String string){
		symbolList.addMatrix(string);
	}

	public static void setSymbolInD(final boolean value){
		MidletMenju.symbolInD = value;
	}

	public static Command getBackCommand(){
		return backCommand;
	}

	public static Display getDisplay(){
		return display;
	}
}
