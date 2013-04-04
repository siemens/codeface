package de.siemens.quantarch.bugs.parser;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.log4j.Logger;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.session.HttpBugzillaSession;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.history.FetchBugzillaHistroy;
import de.siemens.quantarch.bugs.history.FetchHistory;
import de.siemens.quantarch.bugs.products.ParseProducts;

public class LinuxKernelBugParser extends IssueParser {

	private static Logger log = Logger.getLogger(LinuxKernelBugParser.class);

	private List<String> possibleStatuses = new ArrayList<String>();

	// initialize possible statuses during instance creation
	{
		possibleStatuses.add("NEW");
		possibleStatuses.add("ASSIGNED");
		possibleStatuses.add("REOPENED");
		possibleStatuses.add("RESOLVED");
		possibleStatuses.add("VERIFIED");
		possibleStatuses.add("REJECTED");
		possibleStatuses.add("DEFERRED");
		possibleStatuses.add("NEEDINFO");
		possibleStatuses.add("CLOSED");
	}

	@Override
	public void parseBugs(long projectId) throws ConfigurationException {
		FetchHistory historyFetcher = new FetchBugzillaHistroy(
				"https://bugzilla.kernel.org");
		ParseProducts prodParser = new ParseProducts(
				"https://bugzilla.kernel.org");

		// Add products into the database
		List<String> products = prodParser.fetchProducts();
		for (String product : products) {
			bugzillaDAO.addProduct(product, projectId);
		}

		// Configure from file
		File file = new File("linuxKernelBugzilla.xml");
		if (file.exists()) {
			XMLConfiguration myConfig = new XMLConfiguration(file);

			// Create the session
			HttpBugzillaSession session = new HttpBugzillaSession();
			session.configure(myConfig);

			// Open the session
			if (session.open()) {
				// Search a bug

				for (String product : products) {
					for (String status : possibleStatuses) {
						DefaultSearchData searchData = new DefaultSearchData();
						searchData.add("bug_status", status);
						searchData.add("product", product);
						// Perform the search
						Iterator<Issue> i = session
								.searchBugs(searchData, this);
						while (i.hasNext()) {
							Issue issue = i.next();
							List<BugHistory> bugHistoryList = historyFetcher
									.fetchBugHistory(issue.getId());
							bugzillaDAO.addIssue(issue, projectId,
									bugHistoryList);
						}
					}
				}
				session.close();
			}
		} else {
			log.error("File does not exist");
		}
	}

	@Override
	public void setResultCount(int resultCount) {
		log.debug("Number of bugs:" + resultCount);
	}

}
