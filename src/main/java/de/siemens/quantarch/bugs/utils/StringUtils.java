package de.siemens.quantarch.bugs.utils;

/**
 * Helper class for string manipulations
 * 
 * @author Deepak Srinathan
 * 
 */
public final class StringUtils {

	/**
	 * Check if the string is NULL or blank
	 * 
	 * @param input
	 *            the input string
	 * @return true if the string is null or blank , false otherwise
	 */
	public static boolean isBlankOrNull(String input) {
		boolean flag = false;
		if (null == input || input.equalsIgnoreCase("")) {
			flag = true;
		}
		return flag;
	}
}
