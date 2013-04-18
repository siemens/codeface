package de.siemens.quantarch.bugs.history;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class GenericHistoryFetcher implements FetchHistory {

	private String bugzillaURL = null;

	public GenericHistoryFetcher(String bugzillaURL) {
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

	@Override
	public List<BugHistory> fetchBugHistory(String bugId) {
		List<BugHistory> bugHistoryList = new ArrayList<BugHistory>();
		log.info("Fetching history details for bug: " + bugId);
		String bugzillaHistoryURL = bugzillaURL + "/show_activity.cgi?id="
				+ bugId;
		log.debug("Bugzilla URL to fetch history:" + bugzillaHistoryURL);
		try {

			// set the jsoup connection timeout from 3 seconds to 10 seconds.
			Document doc = Jsoup.connect(bugzillaHistoryURL).timeout(10 * 1000)
					.get();
			String[] tags = { "Status", "Severity" };
			for (String tag : tags) {
				Elements tableRows = doc.select("div#bugzilla-body")
						.select("table").select("tbody")
						.select("tr td:contains(" + tag + ")");
				for (Element row : tableRows) {
					BugHistory history = new BugHistory();
					history.setField(tag);
					Element rowEle = row.parent();
					Element pickUpUserDetailsElement = null;
					if (rowEle.children().size() != 5) {
						Element elem = rowEle.previousElementSibling();
						if (!elem.children().first().hasAttr("rowspan")) {
							while (!elem.children().first().hasAttr("rowspan")) {
								elem = elem.previousElementSibling();
							}
						}
						pickUpUserDetailsElement = elem;
					} else {
						pickUpUserDetailsElement = rowEle;
					}

					String who = null;
					Element whoEle = pickUpUserDetailsElement.children()
							.first();
					who = whoEle.text();
					history.setWho(who);

					String when = null;
					Element whenEle = whoEle.nextElementSibling();
					when = whenEle.text().trim();
					history.setWhen(getDateFromString(when));

					Elements rowEleChildren = rowEle.children();
					for (Element content : rowEleChildren) {
						if (content.text().trim().equalsIgnoreCase(tag)) {
							history.setField(tag);
							history.setOldValue(content.nextElementSibling()
									.text());
							history.setNewValue(content.nextElementSibling()
									.nextElementSibling().text());
						}
					}
					bugHistoryList.add(history);
				}

				try {
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					log.error(e);
				}

			}
		} catch (IOException e) {
			log.error("Error occured while fetching history details for bug : "
					+ bugId, e);
		}
		return bugHistoryList;
	}

	private static Date getDateFromString(String dateString) {
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-d HH:mm:ss");
		Date date = null;
		try {
			date = dateFormat.parse(dateString);
		} catch (ParseException e) {
			e.printStackTrace();
		}
		return date;
	}

	public static void main(String[] args) throws IOException {
		FetchHistory historyFetcher = new GenericHistoryFetcher(
				"https://bugzilla.kernel.org");
		List<BugHistory> historyList = historyFetcher.fetchBugHistory("2082");
		for (BugHistory history : historyList) {
			System.out.println(history.getWho());
			System.out.println(history.getWhen());
			System.out.println(history.getField());
			System.out.println(history.getOldValue());
			System.out.println(history.getNewValue());
			System.out.println("------------------------------------");
		}
	}

}
