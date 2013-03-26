package de.siemens.quantarch.bugs.history;

import java.util.List;

public interface FetchHistory {

	/**
	 * Fetch the bug history given the bugId
	 * 
	 * @param bugId
	 * @return List of changes done to the Status and Severity of the bug
	 */
	public List<BugHistory> fetchBugHistory(String bugId);

}
