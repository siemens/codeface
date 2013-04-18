package de.siemens.quantarch.bugs.scraper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import de.siemens.quantarch.bugs.utils.StringUtils;

public class GenericStatusFetcher implements StatusFetcher {

	private static Logger log = Logger.getLogger(GenericStatusFetcher.class);

	// set the proxy server
	static {
		System.setProperty("http.proxySet", "true");
		System.setProperty("http.proxyHost", "proxyfarm.3dns.netz.sbs.de");
		System.setProperty("http.proxyPort", "84");

		System.setProperty("https.proxySet", "true");
		System.setProperty("https.proxyHost", "proxyfarm.3dns.netz.sbs.de");
		System.setProperty("https.proxyPort", "84");
	}

	@Override
	public List<String> fetchStatus(String bugzillaURL) {

		List<String> products = new ArrayList<String>();
		log.info("Fetching status details");
		String bugzillaProductURL = bugzillaURL + "/query.cgi?format=advanced";
		log.debug("Bugzilla URL to fetch details:" + bugzillaProductURL);
		try {

			// set the jsoup connection timeout from 3 seconds to 10 seconds.
			Document doc = Jsoup.connect(bugzillaProductURL).timeout(10 * 1000)
					.get();
			Elements options = doc.select("select[name=bug_status][id=bug_status] option");
			for (Element elem : options) {
				String value = elem.attr("value");
				if (!StringUtils.isBlankOrNull(value)) {
					products.add(value);
				}
			}

		} catch (IOException e) {
			log.error("Error occured while fetching product details for bug",e);
		}
		return products;
	}

	public static void main(String[] args) throws IOException {
		StatusFetcher statusFetcher = new GenericStatusFetcher();
		List<String> statuses = statusFetcher
				.fetchStatus("https://bugzilla.kernel.org");
		for (String status : statuses) {
			System.out.println(status);
		}
	}

}
