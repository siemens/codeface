package de.siemens.quantarch.bugs;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.log4j.Logger;

import b4j.core.DefaultSearchData;
import b4j.core.SearchResultCountCallback;
import b4j.core.session.HttpBugzillaSession;
import de.siemens.quantarch.bugs.products.ParseProducts;

public class BugzillaTest implements SearchResultCountCallback {

	private int count = 0;

	private static Logger log = Logger.getLogger(BugzillaTest.class);

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

	private String stat = null;
	private String prod = null;

	public void queryBug() throws ConfigurationException,
			UnsupportedEncodingException {

		ParseProducts prodParser = new ParseProducts(
				"https://bugzilla.kernel.org");

		List<String> products = prodParser.fetchProducts();
		// Configure from file
		File file = new File("linuxKernelBugzilla.xml");
		if (file.exists()) {
			XMLConfiguration myConfig = new XMLConfiguration(file);

			// Create the session

			HttpBugzillaSession session = new HttpBugzillaSession();
			session.configure(myConfig);

			// Open the session
			if (session.open()) {
				for (String status : possibleStatuses) {
					for (String product : products) {
						stat = status;
						prod = product;
						DefaultSearchData searchData = new DefaultSearchData();
						searchData.add("product",
								URLEncoder.encode(product, "UTF-8"));
						searchData.add("bug_status", status);
						session.searchBugs(searchData, this);
					}
				}
				session.close();
				log.info("Total bugs: " + count);
			}
		} else {
			System.out.println("File does not exist");
		}
	}

	public static void main(String[] args) throws ConfigurationException,
			UnsupportedEncodingException {
		BugzillaTest b = new BugzillaTest();
		b.queryBug();

	}

	@Override
	public void setResultCount(int resultCount) {
		count += resultCount;
		log.info("[" + stat + "][" + prod + "]Number of bugs:" + resultCount);
	}

}
