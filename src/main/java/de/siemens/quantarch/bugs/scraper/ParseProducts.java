package de.siemens.quantarch.bugs.scraper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import de.siemens.quantarch.bugs.history.GenericHistoryFetcher;

public class ParseProducts {

	private String bugzillaURL = null;

	public ParseProducts(String bugzillaURL) {
		this.bugzillaURL = bugzillaURL;
	}

	private static Logger log = Logger.getLogger(GenericHistoryFetcher.class);

	// set the proxy server
	static {
		System.setProperty("http.proxySet", "true");
		System.setProperty("http.proxyHost", "proxyfarm.3dns.netz.sbs.de");
		System.setProperty("http.proxyPort", "84");

		System.setProperty("https.proxySet", "true");
		System.setProperty("https.proxyHost", "proxyfarm.3dns.netz.sbs.de");
		System.setProperty("https.proxyPort", "84");
	}

	public List<String> fetchProducts() {
		List<String> products = new ArrayList<String>();
		log.info("Fetching product details");
		String bugzillaProductURL = bugzillaURL + "/describecomponents.cgi";
		log.debug("Bugzilla URL to fetch history:" + bugzillaProductURL);
		try {

			// set the jsoup connection timeout from 3 seconds to 10 seconds.
			Document doc = Jsoup.connect(bugzillaProductURL).timeout(10 * 1000)
					.get();
			Elements tableRows = doc.select("div#bugzilla-body")
					.select("table").select("tr th a");
			for (Element elem : tableRows) {
				String elementText = elem.text().replace("‑", "-")
						.replaceAll("\\xA0", " ");
				products.add(elementText);
			}

		} catch (IOException e) {
			log.error("Error occured while fetching product details for bug");
		}
		return products;
	}

	public static void main(String[] args) throws IOException {
		ParseProducts historyFetcher = new ParseProducts(
				"https://bugzilla.kernel.org");
		List<String> historyList = historyFetcher.fetchProducts();
		for (String history : historyList) {
			System.out.println(history);
		}
	}
}
