package de.siemens.quantarch.bugs;

import java.io.File;
import java.util.Iterator;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.SearchResultCountCallback;
import b4j.core.session.HttpBugzillaSession;

public class BugzillaTest implements SearchResultCountCallback {

	public void queryBug() throws ConfigurationException {
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
				searchData.add("bug_status", "ASSIGNED");
				searchData.add("bug_status", "REOPENED");
				// Perform the search
				Iterator<Issue> i = session.searchBugs(searchData, this);
				while (i.hasNext()) {
					Issue issue = i.next();
					System.out.println(issue
							.getCustomField("cf_kernel_version"));
					System.out.println(issue);
				}
				session.close();
			}
		} else {
			System.out.println("File does not exist");
		}
	}

	public static void main(String[] args) throws ConfigurationException {
		BugzillaTest b = new BugzillaTest();
		b.queryBug();
	}

	@Override
	public void setResultCount(int resultCount) {
		System.out.println("Number of bugs:" + resultCount);
	}

}
