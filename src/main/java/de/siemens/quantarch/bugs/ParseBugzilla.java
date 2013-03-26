package de.siemens.quantarch.bugs;

import java.io.File;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAO;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.history.FetchBugzillaHistroy;
import de.siemens.quantarch.bugs.history.FetchHistory;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.SearchResultCountCallback;
import b4j.core.session.HttpBugzillaSession;

public class ParseBugzilla implements SearchResultCountCallback {

	public void parseBugs() throws ConfigurationException {

		ApplicationContext context = new ClassPathXmlApplicationContext(
				"beans.xml");
		QuantArchBugzillaDAO bugzillaDAO = (QuantArchBugzillaDAO) context
				.getBean("bugzillaDAO");
		FetchHistory historyFetcher = new FetchBugzillaHistroy(
				"https://bugzilla.kernel.org");

		// Configure from file
		File file = new File("myConfig.xml");
		if (file.exists()) {
			XMLConfiguration myConfig = new XMLConfiguration(file);

			// Create the session
			HttpBugzillaSession session = new HttpBugzillaSession();
			session.configure(myConfig);

			// Open the session
			if (session.open()) {
				// Search a bug
				DefaultSearchData searchData = new DefaultSearchData();
				searchData.add("bug_status", "NEW");
				
				// TODO: This list needs to be passed from the driver as the person
				// adding the bugzilla URL needs to the various possible statuses of the
				// project
				/*
				 * searchData.add("bug_status", "ASSIGNED");
				 * searchData.add("bug_status", "REOPENED");
				 * searchData.add("bug_status", "RESOLVED");
				 * searchData.add("bug_status", "VERIFIED");
				 * searchData.add("bug_status", "REJECTED");
				 * searchData.add("bug_status", "DEFERRED");
				 * searchData.add("bug_status", "NEEDINFO");
				 * searchData.add("bug_status", "CLOSED");
				 */

				// Perform the search
				Iterator<Issue> i = session.searchBugs(searchData, this);
				while (i.hasNext()) {
					Issue issue = i.next();
					List<BugHistory> bugHistoryList = historyFetcher
							.fetchBugHistory(issue.getId());
					bugzillaDAO.addIssue(issue, 1L, bugHistoryList);
				}
				session.close();
			}
		} else {
			System.out.println("File does not exist");
		}
	}

	public static void main(String[] args) throws ConfigurationException {
		ParseBugzilla b = new ParseBugzilla();
		b.parseBugs();
	}

	@Override
	public void setResultCount(int resultCount) {
		System.out.println("Number of bugs:" + resultCount);
	}

}
