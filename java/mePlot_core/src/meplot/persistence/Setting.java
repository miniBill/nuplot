package meplot.persistence;

public final class Setting{ // NO_UCD
	private final String[] choices;
	private final String description;
	private final String name;

	public String[] getChoices(){
		return choices;
	}

	public String getDescription(){
		return description;
	}

	public String getName(){
		return name;
	}

	public Setting(final String name, final String description, final String[] choices){
		this.choices = choices;
		this.description = description;
		this.name = name;
	}

	public String toString(){
		return name + ": " + description;
	}
}
