package de.siemens.quantarch.bugs.parser;

import org.apache.commons.configuration.ConfigurationException;
import org.springframework.beans.factory.annotation.Autowired;

import b4j.core.SearchResultCountCallback;

import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAO;

public abstract class IssueParser implements SearchResultCountCallback {

	@Autowired
	protected QuantArchBugzillaDAO bugzillaDAO = null;

	/**
	 * @param bugzillaDAO
	 *            the bugzillaDAO to set
	 */
	public void setBugzillaDAO(QuantArchBugzillaDAO bugzillaDAO) {
		this.bugzillaDAO = bugzillaDAO;
	}

	public abstract void parseBugs(long projectId)
			throws ConfigurationException;

}
