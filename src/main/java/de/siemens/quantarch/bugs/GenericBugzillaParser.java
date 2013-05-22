package de.siemens.quantarch.bugs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.dbcp.BasicDataSource;
import org.apache.log4j.Logger;
import org.yaml.snakeyaml.Yaml;

import b4j.core.DefaultSearchData;
import b4j.core.Issue;
import b4j.core.session.HttpBugzillaSession;
import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAO;
import de.siemens.quantarch.bugs.dao.QuantArchBugzillaDAOImpl;
import de.siemens.quantarch.bugs.exceptions.CommandLineArgsException;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.history.FetchHistory;
import de.siemens.quantarch.bugs.history.GenericHistoryFetcher;
import de.siemens.quantarch.bugs.scraper.GenericProductFetcher;
import de.siemens.quantarch.bugs.scraper.GenericStatusFetcher;
import de.siemens.quantarch.bugs.scraper.ProductFetcher;
import de.siemens.quantarch.bugs.scraper.StatusFetcher;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;
import de.siemens.quantarch.bugs.utils.StringUtils;

public class GenericBugzillaParser implements BugzillaParser {

	private static Logger log = Logger.getLogger(GenericBugzillaParser.class);

	private static Options options = new Options();

	private static final String CONFIG_FILE_CONTENT = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><bugzilla-session class=\"b4j.core.session.HttpBugzillaSession\"><bugzilla-home>$BUGZILLA_URL$</bugzilla-home><proxy-host>$PROXY_HOST$</proxy-host><BugzillaBug class=\"b4j.core.DefaultIssue\" /></bugzilla-session>";

	// variables just to keep track
	private String stat = null;
	private String prod = null;

	// the project configuration
	private BugExtractorConfig projectConfig = null;

	public GenericBugzillaParser(BugExtractorConfig projectConfig) {
		super();
		this.projectConfig = projectConfig;
	}

	/**
	 * Build the database connection
	 * 
	 * @return
	 */
	private DataSource buildDataSource() {
		// create the data source for database
		BasicDataSource dataSource = new BasicDataSource();
		dataSource.setDriverClassName("com.mysql.jdbc.Driver");
		dataSource.setUrl("jdbc:mysql://" + projectConfig.getDbHost()
				+ ":3306/" + projectConfig.getDbName());
		dataSource.setUsername(projectConfig.getDbUser());
		dataSource.setPassword(projectConfig.getDbPassword());
		dataSource.setInitialSize(30);
		return dataSource;
	}

	@Override
	public void parseIssues(StatusFetcher statusFetcher,
			ProductFetcher productFetcher) {

		// database connection
		DataSource dataSource = buildDataSource();
		QuantArchBugzillaDAO bugzillaDAO = new QuantArchBugzillaDAOImpl(
				dataSource, projectConfig);

		// get the projectId for the given name
		long projectId = bugzillaDAO.getProjectId(projectConfig
				.getProjectName());
		if (-1 == projectId) {
			log.error("Project with name: " + projectConfig.getProjectName()
					+ " does not exist in the database");
			return;
		}

		// history fetcher
		FetchHistory historyFetcher = new GenericHistoryFetcher(
				projectConfig.getBugzillaURL());

		// Step 1: create a temporary xml configuration file
		// for b4j using bugzillaURL, proxy server and proxy port
		File tempFile = null;
		HttpBugzillaSession session = null;
		try {
			tempFile = new File("tempConfig " + new Date().getTime() + ".xml");
			writeIntoConfigFile(tempFile, projectConfig.getBugzillaURL(),
					projectConfig.getProxyHost(), projectConfig.getProxyPort());

			// Step 2: Parse bugs based on Products and Statuses
			List<String> products = new ArrayList<String>();
			if (projectConfig.isProductAsProject()) {
				products.add(projectConfig.getProjectName());
			} else {
				products.addAll(productFetcher.fetchProducts(projectConfig
						.getBugzillaURL()));
			}

			List<String> statuses = statusFetcher.fetchStatus(projectConfig
					.getBugzillaURL());

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
						searchData.add("product",
								URLEncoder.encode(product, "UTF-8"));
						searchData.add("bug_status", status);
						Iterator<Issue> issueIter = session.searchBugs(
								searchData, this);

						while (issueIter.hasNext()) {
							Issue issue = issueIter.next();
							if (-1 == bugzillaDAO.getIssue(issue.getId())) {
								List<BugHistory> bugHistoryList = historyFetcher
										.fetchBugHistory(issue.getId());
								bugzillaDAO.addIssue(issue, projectId,
										bugHistoryList);

								// sleep after parsing every bug (as the proxy
								// will block connections)
								// Even ISPs would block connections thinkg that
								// this is a DDoS attack
								try {
									Thread.sleep(projectConfig
											.getSleepTimeOut());
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
		// build project options
		buildOptions();

		// check if any arguments are given
		if (args.length < 1) {
			printUsage();
			return;
		}

		String globalConfig = null;
		String projectConfig = null;

		// ** now lets parse the input
		CommandLineParser cliParser = new BasicParser();
		CommandLine cmd = null;
		try {
			cmd = cliParser.parse(options, args);
		} catch (ParseException pe) {
			printUsage();
			return;
		}

		// check if the options have been provided
		if (cmd.hasOption("g") && cmd.hasOption("p")) {
			globalConfig = cmd.getOptionValue('g');
			projectConfig = cmd.getOptionValue('p');
		} else {
			printUsage();
			return;
		}

		BugExtractorConfig config = new BugExtractorConfig();
		// parse the global configuration file
		try {
			parseGlobalConfiguration(globalConfig, config);
		} catch (CommandLineArgsException e) {
			printUsage();
			return;
		}

		// Parse the project specific YAML file
		try {
			parseProjectConfiguration(projectConfig, config);
		} catch (CommandLineArgsException e) {
			printUsage();
			return;
		}

		BugzillaParser parser = new GenericBugzillaParser(config);
		parser.parseIssues(new GenericStatusFetcher(),
				new GenericProductFetcher());
	}

	/**
	 * Parse the project specific configuration file provided in the command
	 * line
	 * 
	 * @param projectConfigFile
	 * @param config
	 */
	private static void parseProjectConfiguration(String projectConfigFile,
			BugExtractorConfig config) throws CommandLineArgsException {
		Reader projectFileReader = null;
		try {
			Yaml projectConfigYaml = new Yaml();
			projectFileReader = new FileReader(projectConfigFile);
			@SuppressWarnings("unchecked")
			Map<String, Object> map = (Map<String, Object>) projectConfigYaml
					.load(projectFileReader);
			String project = (String) map.get("project");
			String bugzillaURL = (String) map.get("bugzillaURL");
			Boolean productAsProject = (Boolean) map.get("productAsProject");
			if (StringUtils.isBlankOrNull(project)
					|| StringUtils.isBlankOrNull(bugzillaURL)
					|| null == productAsProject) {
				throw new CommandLineArgsException(
						"Improper Project Configuration file supplied\n\n");
			}

			config.setBugzillaURL(bugzillaURL);
			config.setProjectName(project);
			config.setProductAsProject(productAsProject);
		} catch (FileNotFoundException e) {
			throw new CommandLineArgsException("The global configuration file "
					+ projectConfigFile + " does not exist");
		} finally {
			if (null != projectFileReader) {
				try {
					projectFileReader.close();
				} catch (final IOException ioe) {
					System.err
							.println("We got the following exception trying to clean up the reader: "
									+ ioe);
				}
			}
		}
	}

	/**
	 * Parse the global configuration file provided in the command line
	 * 
	 * @param globalConfigFile
	 * @param config
	 * @throws CommandLineArgsException
	 */
	private static void parseGlobalConfiguration(String globalConfigFile,
			BugExtractorConfig config) throws CommandLineArgsException {
		// parse the global configuration details
		Reader globalFileReader = null;
		try {
			Yaml globalConfigYaml = new Yaml();
			globalFileReader = new FileReader(globalConfigFile);
			@SuppressWarnings("unchecked")
			Map<String, Object> map = (Map<String, Object>) globalConfigYaml
					.load(globalFileReader);
			String dbHost = (String) map.get("dbhost");
			String dbUser = (String) map.get("dbuser");
			String dbPwd = (String) map.get("dbpwd");
			String dbName = (String) map.get("dbname");
			String proxyHost = (String) map.get("proxyHost");
			Integer proxyPort = (Integer) map.get("proxyPort");
			Integer sleepTime = (Integer) map.get("sleepTime");
			String personServiceUrl = (String) map.get("personServiceUrl");

			// Null and empty checks on all the necessary values
			if (StringUtils.isBlankOrNull(dbHost)
					|| StringUtils.isBlankOrNull(dbUser)
					|| StringUtils.isBlankOrNull(dbPwd)
					|| StringUtils.isBlankOrNull(dbName)
					|| StringUtils.isBlankOrNull(personServiceUrl)) {
				throw new CommandLineArgsException(
						"Improper Global Configuration file supplied\n\n");
			}

			// set configuration
			config.setDbHost(dbHost);
			config.setDbName(dbName);
			config.setDbPassword(dbPwd);
			config.setDbUser(dbUser);
			config.setSleepTimeOut(sleepTime);
			config.setPersonServiceURL(personServiceUrl);

			if (null != proxyHost) {
				config.setProxyHost(proxyHost);
				System.setProperty("http.proxyHost", proxyHost);
				System.setProperty("https.proxyHost", proxyHost);
			}
			if (null != proxyPort) {
				config.setProxyPort(proxyPort);
				System.setProperty("http.proxyPort",
						Integer.toString(proxyPort));
				System.setProperty("https.proxyPort",
						Integer.toString(proxyPort));
			}
		} catch (FileNotFoundException e) {
			throw new CommandLineArgsException("The global configuration file "
					+ globalConfigFile + " does not exist");
		} finally {
			if (null != globalFileReader) {
				try {
					globalFileReader.close();
				} catch (final IOException ioe) {
					System.err
							.println("We got the following exception trying to clean up the reader: "
									+ ioe);
				}
			}
		}
	}

	private static void buildOptions() {
		options.addOption("g", true, "global configuration file");
		options.addOption("p", true, "project configuration file");
	}

	private static void printUsage() {
		// Use the inbuilt formatter class
		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("java -jar BugExtractor.jar", options);
	}
}
