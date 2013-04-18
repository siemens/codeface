package de.siemens.quantarch.bugs;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.log4j.Logger;

import de.siemens.quantarch.bugs.scraper.GenericProductFetcher;
import de.siemens.quantarch.bugs.scraper.GenericStatusFetcher;
import de.siemens.quantarch.bugs.scraper.ProductFetcher;
import de.siemens.quantarch.bugs.scraper.StatusFetcher;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.SearchResultCountCallback;
import b4j.core.session.HttpBugzillaSession;

public class BugzillaTest implements SearchResultCountCallback {

	private int count = 0;

	private static Logger log = Logger.getLogger(BugzillaTest.class);

	public void queryBug(String bugzillaURL) throws ConfigurationException,
			UnsupportedEncodingException {

		HttpBugzillaSession session = null;
		StatusFetcher statusFetcher = new GenericStatusFetcher();
		ProductFetcher productFetcher = new GenericProductFetcher();

		File file = new File("linuxKernelBugzilla.xml");
		if (file.exists()) {
			try {
				XMLConfiguration myConfig = new XMLConfiguration(file);

				// Create the session
				session = new HttpBugzillaSession();
				session.configure(myConfig);

				// Step 2: Parse bugs based on Products and Statuses
				List<String> prodcuts = productFetcher
						.fetchProducts(bugzillaURL);
				List<String> statuses = statusFetcher.fetchStatus(bugzillaURL);

				// Open the session
				if (session.open()) {

					DefaultSearchData searchData = new DefaultSearchData();
					searchData.add("product",
							URLEncoder.encode("Drivers", "UTF-8"));
					searchData.add("bug_status", "RESOLVED");
					Iterator<Issue> issueIter = session.searchBugs(searchData,
							this);
					int i = 0;
					while (issueIter.hasNext()) {
						Issue issue = issueIter.next();
						log.info("[" + i++ + "]" + "Hello Bug:" + issue.getId());
					}
				}
				log.info("Total bugs: " + count);
			} catch (NumberFormatException e) {
				log.error(e);
			} finally {
				if (null != session) {
					session.close();
				}
			}
		} else {
			System.out.println("File does not exist");
		}
	}

	public static void main(String[] args) throws ConfigurationException,
			UnsupportedEncodingException {
		BugzillaTest b = new BugzillaTest();
		b.queryBug("https://bugzilla.kernel.org");
	}

	@Override
	public void setResultCount(int resultCount) {
		count += resultCount;
		log.info("Number of bugs:" + resultCount);
	}

}
