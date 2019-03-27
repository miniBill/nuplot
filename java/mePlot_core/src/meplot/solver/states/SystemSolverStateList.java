package meplot.solver.states;

import platform.lists.List;

class SystemSolverStateList extends List{
	public void add(final ISystemSolverState state){
		super.add(state);
	}

	public void insert(final ISystemSolverState state, final int index){
		super.insert(state, index);
	}

	public final SystemSolverState pop(){
		final SystemSolverState toret = (SystemSolverState)ggetFirst();
		super.removeAt(0);
		return toret;
	}
}
