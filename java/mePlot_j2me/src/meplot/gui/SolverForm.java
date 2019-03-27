package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;

import meplot.expressions.Expression;
import meplot.localization.L10N;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import meplot.solver.Solution;
import meplot.solver.Solver;
import platform.log.Log;
import platform.persistence.Persistence;

public final class SolverForm extends Form implements CommandHandler{
	private final TextField inputField;
	private final TextField solField;
	private final Solver solver = new Solver();

	public SolverForm(){
		super(L10N.get(L10N.SOLVER));

		solveCommand = new Command(L10N.get(L10N.SOLVE), Command.ITEM, 0);

		final String funct = Persistence.loadString("slvInput");
		inputField = new TextField(L10N.get(L10N.INPUT) + ':',
				funct.length() > 0 ? funct : "3x+2y=1;x+y=0", 500,
				TextField.NON_PREDICTIVE);
		inputField.setInitialInputMode("MIDP_LOWERCASE_LATIN");
		this.append(inputField);

		solField = new TextField(L10N.get(L10N.SOLUTION) + ':', "", 1500,
				TextField.NON_PREDICTIVE);
		this.append(solField);
	}

	private void doSolve(){
		try{
			final Expression input = Parser.parse(inputField.getString());
			Persistence.saveString("sbInput", input.toString());
			final Solution sol = solver.solve(input);
			solField.setString(sol.toString());
		}
		catch(final ParserException e){
			Log.log(e);
			solField.setString("Parse Error");
		}
	}

	private final Command solveCommand;

	public boolean handle(final Command command){
		if(command.equals(solveCommand)){
			doSolve();
			return true;
		}
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());
		addCommand(solveCommand);

		setCommandListener(midletMenju);
	}
}
