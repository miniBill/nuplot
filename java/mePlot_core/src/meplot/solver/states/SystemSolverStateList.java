package meplot.solver.states;

import platform.lists.List;

class SystemSolverStateList extends List<SystemSolverState> {
	public final SystemSolverState pop() {
		final SystemSolverState toret = getFirst();
		super.removeAt(0);
		return toret;
	}
}
