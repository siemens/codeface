package de.siemens.quantarch.bugs.scraper;

import java.util.List;

public interface ProductFetcher {

	public abstract List<String> fetchProducts(String bugzillaURL);

}
