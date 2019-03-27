package platform.log;

import platform.Platform;

public final class Log{
	private static MyLogger istance;

	private Log(){

	}

	public static void log(final LogLevel level, final String string){
		if(istance == null)
			istance = new FilterLogger(LogLevel.WARNING, Platform.getLogger());
		Log.istance.println(level, string);
	}

	public static void log(final LogLevel level, final String string,
			final String string2){
		log(level, string);
		log(level, string2);
	}

	public static void log(final LogLevel level, final String string,
			final String string2, final String string3){
		log(level, string);
		log(level, string2);
		log(level, string3);
	}

	public static void log(final LogLevel level, final String string1,
			final String string2, final String string3, final String string4){
		log(level, string1);
		log(level, string2);
		log(level, string3);
		log(level, string4);
	}

	public static void log(final Exception exc){
		log(LogLevel.WARNING, exc);
	}

	public static void log(final Error err){
		log(LogLevel.WARNING, err);
	}

	public static void setLogger(final MyLogger val){
		istance = val;
	}

	private static void log(final LogLevel level, final Exception exc){
		log(level, "Exception: " + exc.getMessage());
	}

	private static void log(final LogLevel level, final Error err){
		log(level, "Error: " + err.getMessage());
	}

	public static void log(final LogLevel level, final float float1,
			final float float2, final float float3, final float float4){
		log(level, float1);
		log(level, float2);
		log(level, float3);
		log(level, float4);
	}

	private static void log(final LogLevel level, final float arg){
		log(level, Float.toString(arg));
	}
}
