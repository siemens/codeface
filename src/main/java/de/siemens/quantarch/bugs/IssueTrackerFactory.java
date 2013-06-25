package de.siemens.quantarch.bugs;

import de.siemens.quantarch.bugs.impl.BugzillaTracker;
import de.siemens.quantarch.bugs.impl.JiraTracker;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;

public class IssueTrackerFactory {

	public static IssueTracker getIssueTracker(BugExtractorConfig config) {
		IssueTracker issueTracker = null;
		if (config.getIssueTrackerType().equalsIgnoreCase("bugzilla")) {
			issueTracker = new BugzillaTracker();
		} else if (config.getIssueTrackerType().equalsIgnoreCase("jira")) {
			issueTracker = new JiraTracker();
		}
		issueTracker.setConfig(config);
		return issueTracker;
	}
}
