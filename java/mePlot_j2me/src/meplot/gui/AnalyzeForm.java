package meplot.gui;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;

import meplot.expressions.Expression;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.localization.L10N;
import meplot.parser.Parser;
import meplot.parser.utils.Cleaner;
import platform.persistence.Persistence;

public final class AnalyzeForm extends Form implements CommandHandler{
	private final TextField textField;
	private final TextField sField;
	private final TextField dField;
	private final TextField ddField;

	public AnalyzeForm(){
		super("AnalyzeForm");
		dCommand = new Command(L10N.get(L10N.DERIVATIVE), Command.ITEM, 0);
		insertCommand = new Command(L10N.get(L10N.INSERT), Command.OK, 0);

		final String function = Persistence.loadString("dInput");

		textField = new TextField(L10N.get(L10N.INPUT) + ':',
				function.length() > 0 ? function : "sin(x)", 500,
				TextField.NON_PREDICTIVE);
		textField.setInitialInputMode("MIDP_LOWERCASE_LATIN");
		this.append(textField);

		sField = new TextField("f(x):", "", 500, TextField.NON_PREDICTIVE);
		this.append(sField);

		dField = new TextField("f'(x):", "", 1000, TextField.NON_PREDICTIVE);
		this.append(dField);

		ddField = new TextField("f''(x):", "", 2000, TextField.NON_PREDICTIVE);
		this.append(ddField);
	}

	public Expression getExpression(){
		Persistence.saveString("dInput", textField.getString());
		return Parser.parseOrDefault(textField.getString());
	}

	public void appendSymbol(final String val){
		textField.insert(val, textField.getCaretPosition());
	}

	private void doDerivative(){
		final Expression expr = getExpression();

		final Expression simplifiedExpr = simplifyCleanAndSet(expr, sField);

		final Expression der1 = DerivativeHelper.derivativeOrDefault(
				simplifiedExpr, 'x');

		final Expression der1clean = simplifyCleanAndSet(der1, dField);

		final Expression der2 = DerivativeHelper.derivativeOrDefault(
				der1clean, 'x');

		simplifyCleanAndSet(der2, ddField);
	}

	private static Expression simplifyCleanAndSet(
			final Expression expression, final TextField field){
		final Expression tps = SimplificationHelper.simplify(expression);
		final String tpsc = Cleaner.clean(tps.toString());
		field.setString(tpsc);
		return tps;
	}

	private final Command insertCommand;
	private final Command dCommand;

	public boolean handle(final Command command){
		if(command.equals(insertCommand)){
			MidletMenju.setSymbolInD(true);
			MidletMenju.viewSymbolList();
			return true;
		}
		if(command.equals(dCommand)){
			doDerivative();
			return true;
		}
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());
		addCommand(insertCommand);
		addCommand(dCommand);

		setCommandListener(midletMenju);
	}

}
