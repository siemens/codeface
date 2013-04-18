package de.siemens.quantarch.bugs.scraper;

import java.util.List;

public interface StatusFetcher {

	public List<String> fetchStatus(String bugzillaURL);

}
