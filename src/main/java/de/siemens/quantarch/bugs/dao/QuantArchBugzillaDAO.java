package de.siemens.quantarch.bugs.dao;

import java.util.List;

import de.siemens.quantarch.bugs.history.BugHistory;

import b4j.core.Issue;

public interface QuantArchBugzillaDAO {

	public long addIssue(Issue issue, long projectId,
			List<BugHistory> bugHistoryList);

	public long addProject(String name, String issueTrackerURL,
			String mailingListURL);

	public void dumpAdjacencyMatrix(String fileName, long projectId);

	public long addProduct(String name, long projectId);

}
