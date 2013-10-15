package de.siemens.quantarch.bugs.utils;

public class BugExtractorConfig {
	// Obtained from Global properties
	private String dbHost = null;
	private String dbName = null;
	private String dbUser = null;
	private String dbPassword = null;
	private String proxyHost = null;
	private int proxyPort = 33333;
	private int sleepTimeOut = 2000;
	private String personServiceURL = null;

	// Obtained from project properties
	private String issueTrackerType = "bugzilla";
	private String issueTrackerURL = null;
	private String projectName = null;
	private String bugsProjectName = null;
	private long projectId = -1;
	private boolean productAsProject = false;

	public BugExtractorConfig() {
		super();
	}

	public BugExtractorConfig(String dbHost, String dbName, String dbUser,
			String dbPassword, String proxyHost, int proxyPort,
			int sleepTimeOut, String personServiceURL, String issueTrackerURL,
			String projectName, boolean productAsProject) {
		super();
		this.dbHost = dbHost;
		this.dbName = dbName;
		this.dbUser = dbUser;
		this.dbPassword = dbPassword;
		this.proxyHost = proxyHost;
		this.proxyPort = proxyPort;
		this.sleepTimeOut = sleepTimeOut;
		this.personServiceURL = personServiceURL;
		this.issueTrackerURL = issueTrackerURL;
		this.projectName = projectName;
		this.productAsProject = productAsProject;
	}

	/**
	 * @return the dbHost
	 */
	public String getDbHost() {
		return dbHost;
	}

	/**
	 * @param dbHost
	 *            the dbHost to set
	 */
	public void setDbHost(String dbHost) {
		this.dbHost = dbHost;
	}

	/**
	 * @return the dbName
	 */
	public String getDbName() {
		return dbName;
	}

	/**
	 * @param dbName
	 *            the dbName to set
	 */
	public void setDbName(String dbName) {
		this.dbName = dbName;
	}

	/**
	 * @return the dbUser
	 */
	public String getDbUser() {
		return dbUser;
	}

	/**
	 * @param dbUser
	 *            the dbUser to set
	 */
	public void setDbUser(String dbUser) {
		this.dbUser = dbUser;
	}

	/**
	 * @return the dbPassword
	 */
	public String getDbPassword() {
		return dbPassword;
	}

	/**
	 * @param dbPassword
	 *            the dbPassword to set
	 */
	public void setDbPassword(String dbPassword) {
		this.dbPassword = dbPassword;
	}

	/**
	 * @return the proxyHost
	 */
	public String getProxyHost() {
		return proxyHost;
	}

	/**
	 * @param proxyHost
	 *            the proxyHost to set
	 */
	public void setProxyHost(String proxyHost) {
		this.proxyHost = proxyHost;
	}

	/**
	 * @return the proxyPort
	 */
	public int getProxyPort() {
		return proxyPort;
	}

	/**
	 * @param proxyPort
	 *            the proxyPort to set
	 */
	public void setProxyPort(int proxyPort) {
		this.proxyPort = proxyPort;
	}

	/**
	 * @return the projectName
	 */
	public String getProjectName() {
		return projectName;
	}

	/**
	 * @param projectName
	 *            the projectName to set
	 */
	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	/**
	 * @return the projectName
	 */
	public String getBugsProjectName() {
		return bugsProjectName;
	}

	/**
	 * @param projectName
	 *            the projectName to set
	 */
	public void setBugsProjectName(String bugsProjectName) {
		this.bugsProjectName = bugsProjectName;
	}

	/**
	 * @return the productAsProject
	 */
	public boolean isProductAsProject() {
		return productAsProject;
	}

	/**
	 * @param productAsProject
	 *            the productAsProject to set
	 */
	public void setProductAsProject(boolean productAsProject) {
		this.productAsProject = productAsProject;
	}

	/**
	 * @return the sleepTimeOut
	 */
	public int getSleepTimeOut() {
		return sleepTimeOut;
	}

	/**
	 * @param sleepTimeOut
	 *            the sleepTimeOut to set
	 */
	public void setSleepTimeOut(int sleepTimeOut) {
		this.sleepTimeOut = sleepTimeOut;
	}

	/**
	 * @return the personServiceURL
	 */
	public String getPersonServiceURL() {
		return personServiceURL;
	}

	/**
	 * @param personServiceURL
	 *            the personServiceURL to set
	 */
	public void setPersonServiceURL(String personServiceURL) {
		this.personServiceURL = personServiceURL;
	}

	/**
	 * @return the issueTrackerURL
	 */
	public String getIssueTrackerURL() {
		return issueTrackerURL;
	}

	/**
	 * @param issueTrackerURL
	 *            the issueTrackerURL to set
	 */
	public void setIssueTrackerURL(String issueTrackerURL) {
		this.issueTrackerURL = issueTrackerURL;
	}

	/**
	 * @return the projectId
	 */
	public long getProjectId() {
		return projectId;
	}

	/**
	 * @param projectId
	 *            the projectId to set
	 */
	public void setProjectId(long projectId) {
		this.projectId = projectId;
	}

	/**
	 * @return the issueTrackerType
	 */
	public String getIssueTrackerType() {
		return issueTrackerType;
	}

	/**
	 * @param issueTrackerType
	 *            the issueTrackerType to set
	 */
	public void setIssueTrackerType(String issueTrackerType) {
		this.issueTrackerType = issueTrackerType;
	}

}
