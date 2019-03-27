package platform.database;

public class WhereClause implements Clause{
	private final String column;
	private final String value;

	public WhereClause(final String column, final String value){
		this.column = column;
		this.value = value;
	}

	public String getColumn(){
		return column;
	}

	public String getValue(){
		return value;
	}

	public void toString(final StringBuffer buffer){
		buffer.append("WHERE ");
		buffer.append(column);
		buffer.append(" = ");
		buffer.append(value);
		buffer.append(' ');
	}
}
