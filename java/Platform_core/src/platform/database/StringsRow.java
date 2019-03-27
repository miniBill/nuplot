package platform.database;

public class StringsRow implements Row{
	private final String[] values;
	private final String[] columns;

	public StringsRow(final String[] values, final String[] columns){
		this.values = values;
		this.columns = columns;
	}

	public StringsRow(final String value, final String column){
		values = new String[]{value};
		columns = new String[]{column};
	}

	public String get(final int index){
		return values[index];
	}

	public String get(final String key){
		if(columns == null || columns.length < 2)
			return values[0];
		for(int i = 0; i < columns.length; i++)
			if(columns[i].equals(key))
				return get(i);
		return null;
	}

	public String toString(){
		final StringBuffer sb = new StringBuffer("{");
		toString(sb);
		return sb.toString();
	}

	public void toString(final StringBuffer sb){
		for(int i = 0; i < columns.length; i++){
			sb.append(values[i]);
			if(i < columns.length - 1)
				sb.append(',');
		}
		sb.append('}');
	}
}
