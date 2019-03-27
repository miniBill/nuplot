package meplot.graphics;

import meplot.expressions.numbers.Complex;

public final class DrawUtils{
	private DrawUtils(){
	}

	public static int getColor(final double real, final double immaginary,
			final double minv, final double maxv){
		final double hue;
		if(immaginary == 0)
			hue = (real - minv) * 6.0 / (maxv - minv);
		else
			hue = Complex.arg(real, immaginary) * 3 / Math.PI + 3;

		final double red;
		final double green;
		final double blue;

		final int index = (int)Math.floor(hue);
		final double floor = hue - index;

		switch(index % 6){
			case 0:
				red = 1;
				green = floor;
				blue = 0;
				break;
			case 1:
				red = 1 - floor;
				green = 1;
				blue = 0;
				break;
			case 2:
				red = 0;
				green = 1;
				blue = floor;
				break;
			case 3:
				red = 0;
				green = 1 - floor;
				blue = 1;
				break;
			case 4:
				red = floor;
				green = 0;
				blue = 1;
				break;
			default: // case 5:
				red = 1;
				green = 0;
				blue = 1 - floor;
				break;
		}

		if(immaginary == 0)
			return fromRGB(red, green, blue);

		final double normalizedNorm = (Complex.norm(real, immaginary) - minv)
				/ (maxv - minv) / 1.5;
		final double oppositeNorm = 1.0 - normalizedNorm;
		final double nred = normalizedNorm + red * oppositeNorm;
		final double ngreen = normalizedNorm + green * oppositeNorm;
		final double nblue = normalizedNorm + blue * oppositeNorm;
		return fromRGB(nred, ngreen, nblue);
	}

	private static final int COLORCOEFF = 0x4;

	private static int fromRGB(final double red, final double green, final double blue){
		if(red < 0 || green < 0 || blue < 0)
			return 0xFFFFFF;

		final double redMultiplied = red * 0xFF;
		final double greenMultiplied = green * 0xFF;
		final double blueMultiplied = blue * 0xFF;

		final int redInt = (int)Math.floor(redMultiplied / COLORCOEFF);
		final int greenInt = (int)Math.floor(greenMultiplied / COLORCOEFF);
		final int blueInt = (int)Math.floor(blueMultiplied / COLORCOEFF);

		return 0x010000 * COLORCOEFF * redInt + 0x000100 * COLORCOEFF * greenInt
				+ 0x000001 * COLORCOEFF * blueInt;
	}

	public static int getColor(final double real, final double minv, final double maxv){
		final double hue = (real - minv) * 6.0 / (maxv - minv);

		final double red;
		final double green;
		final double blue;

		final int index = (int)Math.floor(hue);
		final double floor = hue - index;

		switch(index % 6){
			case 0:
				red = 1;
				green = floor;
				blue = 0;
				break;
			case 1:
				red = 1 - floor;
				green = 1;
				blue = 0;
				break;
			case 2:
				red = 0;
				green = 1;
				blue = floor;
				break;
			case 3:
				red = 0;
				green = 1 - floor;
				blue = 1;
				break;
			case 4:
				red = floor;
				green = 0;
				blue = 1;
				break;
			default: // case 5:
				red = 1;
				green = 0;
				blue = 1 - floor;
				break;
		}

		return fromRGB(red, green, blue);
	}
}
