package de.siemens.quantarch.bugs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URLEncoder;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.log4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.session.HttpBugzillaSession;

import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAO;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.history.GenericHistoryFetcher;
import de.siemens.quantarch.bugs.history.FetchHistory;
import de.siemens.quantarch.bugs.scraper.GenericProductFetcher;
import de.siemens.quantarch.bugs.scraper.GenericStatusFetcher;
import de.siemens.quantarch.bugs.scraper.ProductFetcher;
import de.siemens.quantarch.bugs.scraper.StatusFetcher;

public class GenericBugzillaParser implements BugzillaParser {

	private static Logger log = Logger.getLogger(BugzillaTest.class);

	private static final String CONFIG_FILE_CONTENT = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><bugzilla-session class=\"b4j.core.session.HttpBugzillaSession\"><bugzilla-home>$BUGZILLA_URL$</bugzilla-home><proxy-host>$PROXY_HOST$</proxy-host><BugzillaBug class=\"b4j.core.DefaultIssue\" /></bugzilla-session>";

	private String stat = null;
	private String prod = null;

	@Override
	public void parseIssues(String projectName, String bugzillaURL,
			String proxyServer, int proxyPort, StatusFetcher statusFetcher,
			ProductFetcher productFetcher) {

		// Spring beans
		ApplicationContext context = new ClassPathXmlApplicationContext(
				"beans.xml");
		QuantArchBugzillaDAO bugzillaDAO = (QuantArchBugzillaDAO) context
				.getBean("bugzillaDAO");

		// history fetcher
		FetchHistory historyFetcher = new GenericHistoryFetcher(bugzillaURL);

		long projectId = bugzillaDAO.addProject(projectName, bugzillaURL, null);

		// Step 1: create a temporary xml configuration file
		// for b4j using bugzillaURL, proxy server and proxy port
		File tempFile = null;
		HttpBugzillaSession session = null;
		try {
			tempFile = new File("tempConfig " + new Date().getTime() + ".xml");
			writeIntoConfigFile(tempFile, bugzillaURL, proxyServer, proxyPort);

			// Step 2: Parse bugs based on Products and Statuses
			List<String> prodcuts = productFetcher.fetchProducts(bugzillaURL);
			List<String> statuses = statusFetcher.fetchStatus(bugzillaURL);

			XMLConfiguration myConfig;
			myConfig = new XMLConfiguration(tempFile);

			// Create the session
			session = new HttpBugzillaSession();
			session.configure(myConfig);

			if (session.open()) {
				for (String status : statuses) {
					for (String product : prodcuts) {
						if ((status.equalsIgnoreCase("CLOSED") && product
								.equalsIgnoreCase("Drivers"))) {

							// to maintain the names
							stat = status;
							prod = product;

							DefaultSearchData searchData = new DefaultSearchData();
							searchData.add("product",
									URLEncoder.encode(product, "UTF-8"));
							searchData.add("bug_status", status);
							Iterator<Issue> issueIter = session.searchBugs(
									searchData, this);

							int issueCounter = 0;
							while (issueIter.hasNext()) {
								Issue issue = issueIter.next();
								if (-1 == bugzillaDAO.getIssue(issue.getId())) {
									List<BugHistory> bugHistoryList = historyFetcher
											.fetchBugHistory(issue.getId());
									bugzillaDAO.addIssue(issue, projectId,
											bugHistoryList);
								}

								issueCounter++;
								if (issueCounter == 400) {
									issueCounter = 0;
									try {
										Thread.sleep(10000);
									} catch (InterruptedException e) {
										e.printStackTrace();
									}
								}
							}
							log.info("Product: " + prod + " Status: " + stat
									+ " DONE");
						}
					}
				}
			}

		} catch (IOException e) {
			log.error(e);
		} catch (ConfigurationException e) {
			log.error(e);
		} finally {

			// close the HTTP Session
			if (null != session) {
				session.close();
			}

			// delete the temporary configuration file
			if (null != tempFile) {
				if (tempFile.exists()) {
					tempFile.delete();
				}
			}
		}
	}

	private void writeIntoConfigFile(File file, String bugzillaURL,
			String proxyServer, int proxyPort) throws IOException {
		// if file doesnt exists, then create it
		if (!file.exists()) {
			file.createNewFile();
		}
		String filanString = CONFIG_FILE_CONTENT.replace("$BUGZILLA_URL$",
				bugzillaURL).replace("$PROXY_HOST$",
				proxyServer + ":" + proxyPort);
		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);
		bw.write(filanString);
		bw.close();
	}

	@Override
	public void setResultCount(int resultCount) {
		log.info("[" + stat + "][" + prod + "]Number of bugs:" + resultCount);
	}

	public static void main(String[] args) {
		BugzillaParser parser = new GenericBugzillaParser();
		parser.parseIssues("Linux Kernel", "https://bugzilla.kernel.org",
				"proxyfarm.3dns.netz.sbs.de", 84, new GenericStatusFetcher(),
				new GenericProductFetcher());
	}

}
