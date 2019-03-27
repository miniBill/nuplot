package platform.database;

public class SelectClause implements Clause{
	private final String column;

	public SelectClause(final String column){
		this.column = column;
	}

	public String getColumn(){
		return column;
	}

	public void toString(final StringBuffer buffer){
		buffer.append("SELECT ");
		buffer.append(column);
		buffer.append(' ');
	}
}
