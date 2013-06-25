package de.siemens.quantarch.bugs;

import de.siemens.quantarch.bugs.dao.IssueTrackerDao;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;

public interface IssueTracker {

	public void setDao(IssueTrackerDao dao);
	
	public void setConfig(BugExtractorConfig config);

	public void parseIssues();
}
