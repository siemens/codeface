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
