package de.siemens.quantarch.bugs;

import b4j.core.SearchResultCountCallback;
import de.siemens.quantarch.bugs.scraper.ProductFetcher;
import de.siemens.quantarch.bugs.scraper.StatusFetcher;

public interface BugzillaParser extends SearchResultCountCallback {

	public abstract void parseIssues(String projectName, String bugzillaURL,
			String proxyServer, int proxyPort, StatusFetcher statusFetcher,
			ProductFetcher productFetcher);

}
