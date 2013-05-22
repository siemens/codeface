package de.siemens.quantarch.bugs;

import b4j.core.SearchResultCountCallback;
import de.siemens.quantarch.bugs.scraper.ProductFetcher;
import de.siemens.quantarch.bugs.scraper.StatusFetcher;

public interface BugzillaParser extends SearchResultCountCallback {

	public abstract void parseIssues(StatusFetcher statusFetcher,
			ProductFetcher productFetcher);

}
