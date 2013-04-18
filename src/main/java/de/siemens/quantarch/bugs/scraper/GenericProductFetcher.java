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

public class GenericProductFetcher implements ProductFetcher {

	private static Logger log = Logger.getLogger(GenericProductFetcher.class);

	@Override
	public List<String> fetchProducts(String bugzillaURL) {

		List<String> products = new ArrayList<String>();
		log.info("Fetching product details");
		String bugzillaProductURL = bugzillaURL + "/query.cgi";
		log.debug("Bugzilla URL to fetch history:" + bugzillaProductURL);
		try {

			// set the jsoup connection timeout from 3 seconds to 10 seconds.
			Document doc = Jsoup.connect(bugzillaProductURL).timeout(10 * 1000)
					.get();
			Elements options = doc.select("#product option");
			for (Element elem : options) {
				String value = elem.attr("value");
				if (!StringUtils.isBlankOrNull(value)) {
					products.add(value);
				}
			}

		} catch (IOException e) {
			log.error("Error occured while fetching product details for bug");
		}
		return products;
	}
}
