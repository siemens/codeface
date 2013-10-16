/*
 * This file is part of Codeface. Codeface is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, version 2.

 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 * Copyright 2013 by Siemens AG. All Rights Reserved.
 */

package de.siemens.quantarch.bugs.impl;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.log4j.Logger;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.session.HttpBugzillaSession;
import de.siemens.quantarch.bugs.IssueTracker;
import de.siemens.quantarch.bugs.dao.IssueTrackerDao;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.history.FetchHistory;
import de.siemens.quantarch.bugs.history.GenericHistoryFetcher;
import de.siemens.quantarch.bugs.scraper.GenericProductFetcher;
import de.siemens.quantarch.bugs.scraper.GenericStatusFetcher;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;
import de.siemens.quantarch.bugs.utils.StringUtils;

public class BugzillaTracker implements IssueTracker {

	private IssueTrackerDao dao;
	private static final String CONFIG_FILE_WITHOUT_PROXY = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><bugzilla-session class=\"b4j.core.session.HttpBugzillaSession\"><bugzilla-home>$BUGZILLA_URL$</bugzilla-home><BugzillaBug class=\"b4j.core.DefaultIssue\" /></bugzilla-session>";
	private static final String CONFIG_FILE_WITH_PROXY = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><bugzilla-session class=\"b4j.core.session.HttpBugzillaSession\"><proxy-host>$PROXY_HOST$</proxy-host><bugzilla-home>$BUGZILLA_URL$</bugzilla-home><BugzillaBug class=\"b4j.core.DefaultIssue\" /></bugzilla-session>";
	private static Logger log = Logger.getLogger(BugzillaTracker.class);

	// variables just to keep track
	private String stat = null;
	private String prod = null;

	private BugExtractorConfig config;

	@Override
	public void setDao(IssueTrackerDao dao) {
		this.dao = dao;
	}

	@Override
	public void setConfig(BugExtractorConfig config) {
		this.config = config;
	}

	@Override
	public void parseIssues() {
		GenericStatusFetcher statusFetcher = new GenericStatusFetcher();
		GenericProductFetcher productFetcher = new GenericProductFetcher();
		// get the projectId for the given name
		long projectId = dao.getProjectId(config.getProjectName());
		if (-1 == projectId) {
			log.error("Project with name: " + config.getProjectName()
					+ " does not exist in the database");
			return;
		}

		// history fetcher
		FetchHistory historyFetcher = new GenericHistoryFetcher(
				config.getIssueTrackerURL());

		// Step 1: create a temporary xml configuration file
		// for b4j using bugzillaURL, proxy server and proxy port
		File tempFile = null;
		HttpBugzillaSession session = null;
		try {
			tempFile = new File("tempConfig " + new Date().getTime() + ".xml");
			writeIntoConfigFile(tempFile, config.getIssueTrackerURL(),
					config.getProxyHost(), config.getProxyPort());

			// Step 2: Parse bugs based on Products and Statuses
			List<String> products = new ArrayList<String>();
			if (config.isProductAsProject()) {
				products.add(config.getBugsProjectName());
			} else {
				products.addAll(productFetcher.fetchProducts(config
						.getIssueTrackerURL()));
			}

			List<String> statuses = statusFetcher.fetchStatus(config
					.getIssueTrackerURL());

			XMLConfiguration myConfig;
			myConfig = new XMLConfiguration(tempFile);

			// Create the session
			session = new HttpBugzillaSession();
			session.configure(myConfig);

			if (session.open()) {
				for (String status : statuses) {
					for (String product : products) {
						// to maintain the names
						stat = status;
						prod = product;

						DefaultSearchData searchData = new DefaultSearchData();
						// Don't limit the search to 500 responses, but allow for an arbitrary amount
						searchData.add("limit", "0");
						searchData.add("product",
								URLEncoder.encode(product, "UTF-8"));
						searchData.add("bug_status", status);
						Iterator<Issue> issueIter = session.searchBugs(
								searchData, null);

						while (issueIter.hasNext()) {
							Issue issue = issueIter.next();
							List<BugHistory> bugHistoryList = historyFetcher
									.fetchBugHistory(issue.getId());
							dao.addIssue(issue, projectId, bugHistoryList);

							// sleep after parsing every bug (as the proxy
							// will block connections)
							// Even ISPs would block connections thinkg that
							// this is a DDoS attack
							try {
								Thread.sleep(config.getSleepTimeOut());
							} catch (InterruptedException e) {
								e.printStackTrace();
							}
						}
						log.info("Product: " + prod + " Status: " + stat
								+ " DONE");
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
		// if file doesn't exists, then create it
		if (!file.exists()) {
			file.createNewFile();
		}

		// check if proxy configuration is needed
		String finalString = null;
		if (StringUtils.isBlankOrNull(proxyServer)) {
			finalString = CONFIG_FILE_WITHOUT_PROXY.replace("$BUGZILLA_URL$",
					bugzillaURL);
		} else {
			finalString = CONFIG_FILE_WITH_PROXY.replace("$BUGZILLA_URL$",
					bugzillaURL).replace("$PROXY_HOST$",
					proxyServer + ":" + proxyPort);
		}
		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);
		bw.write(finalString);
		bw.close();
	}
}
