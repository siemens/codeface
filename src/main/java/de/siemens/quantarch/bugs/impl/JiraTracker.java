package de.siemens.quantarch.bugs.impl;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import de.siemens.quantarch.bugs.IssueTracker;
import de.siemens.quantarch.bugs.dao.IssueTrackerDao;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;

@SuppressWarnings("unused")
public class JiraTracker implements IssueTracker {

	private IssueTrackerDao dao;
	private BugExtractorConfig config;

	@Override
	public void setDao(IssueTrackerDao dao) {
		this.dao = dao;
	}

	@Override
	public void setConfig(BugExtractorConfig config) {
		this.config = config;
	}

	@Override
	public void parseIssues() {
		throw new NotImplementedException();

	}

}
