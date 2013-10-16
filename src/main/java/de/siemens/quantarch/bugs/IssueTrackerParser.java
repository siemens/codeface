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

package de.siemens.quantarch.bugs;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.dbcp.BasicDataSource;
import org.yaml.snakeyaml.Yaml;

import de.siemens.quantarch.bugs.dao.IssueTrackerDao;
import de.siemens.quantarch.bugs.dao.IssueTrackerDaoImpl;
import de.siemens.quantarch.bugs.exceptions.CommandLineArgsException;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;
import de.siemens.quantarch.bugs.utils.StringUtils;

public class IssueTrackerParser {

	private static Options options = new Options();

	// the project configuration
	private BugExtractorConfig projectConfig = null;

	public IssueTrackerParser(BugExtractorConfig projectConfig) {
		this.projectConfig = projectConfig;
	}

	/**
	 * Parse the issues
	 */
	public void parse() {
		IssueTrackerDao dao = new IssueTrackerDaoImpl(buildDataSource(),
				projectConfig);
		IssueTracker tracker = IssueTrackerFactory
				.getIssueTracker(projectConfig);
		tracker.setDao(dao);
		tracker.parseIssues();
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
		    System.out.println("Duh. Parse Exception: " + pe.getMessage());

			printUsage();
			return;
		}

		// check if the options have been provided
		if (cmd.hasOption("c") && cmd.hasOption("p")) {
			globalConfig = cmd.getOptionValue('c');
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
			System.out.println("Could not parse global config " +
					   "file: " + e.getMessage());
			return;
		}

		// Parse the project specific YAML file
		try {
			parseProjectConfiguration(projectConfig, config);
		} catch (CommandLineArgsException e) {
			System.out.println("Could not parse project config " +
					   "file: " + e.getMessage());
			return;
		}

		IssueTrackerParser parser = new IssueTrackerParser(config);
		parser.parse();
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
			// NOTE: project denotes the identifier name used in
			// the database, whereas bugsProject is the name
			// used for bugzilla.
			String project = (String) map.get("project");
			String bugsProject = (String) map.get("bugsProjectName");
			String issueTrackerType = (String) map.get("issueTrackerType");
			String issueTrackerURL = (String) map.get("issueTrackerURL");
			Boolean productAsProject = (Boolean) map.get("productAsProject");
			if (StringUtils.isBlankOrNull(project) ||
			    StringUtils.isBlankOrNull(bugsProject)
					|| StringUtils.isBlankOrNull(issueTrackerURL)
					|| StringUtils.isBlankOrNull(issueTrackerType)
					|| null == productAsProject) {
				throw new CommandLineArgsException(
						"Improper Project Configuration file supplied\n\n");
			}

			config.setIssueTrackerType(issueTrackerType);
			config.setIssueTrackerURL(issueTrackerURL);
			config.setProjectName(project);
			config.setBugsProjectName(bugsProject);
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
			String personServiceHostname = (String) map.get("idServiceHostname");
			Integer personServicePort = (Integer) map.get("idServicePort");

			// Null and empty checks on all the necessary values
			if (StringUtils.isBlankOrNull(dbHost)
					|| StringUtils.isBlankOrNull(dbUser)
					|| StringUtils.isBlankOrNull(dbPwd)
					|| StringUtils.isBlankOrNull(dbName)
					|| StringUtils.isBlankOrNull(personServiceHostname)) {
				throw new CommandLineArgsException(
						"Improper Global Configuration file supplied");
			}

			// set configuration
			config.setDbHost(dbHost);
			config.setDbName(dbName);
			config.setDbPassword(dbPwd);
			config.setDbUser(dbUser);
			config.setSleepTimeOut(sleepTime);
			config.setPersonServiceURL("http://" + personServiceHostname + ":" + personServicePort);

			// set proxy server configuration if specified
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
