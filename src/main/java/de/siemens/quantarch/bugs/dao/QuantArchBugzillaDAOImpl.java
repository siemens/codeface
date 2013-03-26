package de.siemens.quantarch.bugs.dao;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.simple.SimpleJdbcInsert;
import org.springframework.jdbc.core.support.JdbcDaoSupport;

import de.siemens.quantarch.bugs.history.BugHistory;

import b4j.core.Issue;
import b4j.core.LongDescription;

public class QuantArchBugzillaDAOImpl extends JdbcDaoSupport implements
		QuantArchBugzillaDAO {

	@Override
	public long addIssue(Issue issue, long projectId,
			List<BugHistory> bugHistoryList) {

		// if the issue already exists in the database do not add it again.
		// TODO : In the update scenario, we could delete the issue, cascade the
		// effect and then update the details
		long existingIssueId = checkIfIssueExists(issue.getId(), projectId);
		if (-1 != existingIssueId) {
			return -1;
		}

		long productId = -1;
		long componentId = -1;
		long versionId = -1;
		long createdByUserId = -1;
		long assigneeUserId = -1;

		// Step 1: Add the product if it does not already exist
		String productName = issue.getProduct();

		if (null != productName) {
			productId = addProduct(productName, projectId);
			System.out.println("Product : " + productName + "id: " + productId);
		}

		// Step 2: Add the component and associate it with the product
		String componentName = issue.getComponent();
		if (-1 != productId && null != componentName) {
			componentId = addComponent(componentName, productId);
		}

		// Step 3: Add the version number / revision number
		String version = issue.getVersion();
		if (-1 != productId && null != version) {
			versionId = addVersion(version, productId);
		}

		// Step 4: Add the users
		// a> Created By
		String createdByName = issue.getReporter();
		if (null != createdByName) {
			createdByUserId = addUser(createdByName, projectId);
		}

		// b> Assigned to
		String assigneeName = issue.getAssignee();
		if (null != assigneeName) {
			assigneeUserId = addUser(assigneeName, projectId);
		}

		// TODO : Can remove the below code once it is decided to store status,
		// priority, resolution and severity as strings and not as integers
		/*
		 * IssueStatus status = getIssueStatus(issue.getStatus()); IssuePriority
		 * priority = getIssuePriority(issue.getPriority()); IssueResolution
		 * resolution = getIssueResolution(issue.getResolution());
		 */

		// Step 5: Add the issue
		// now add the issue record
		SimpleJdbcInsert insertPerson = new SimpleJdbcInsert(getDataSource())
				.withTableName("issue").usingGeneratedKeyColumns("id");
		Map<String, Object> parameters = new HashMap<String, Object>(2);
		parameters.put("bugId", issue.getId());
		parameters.put("creationDate", issue.getCreationTimestamp());
		parameters.put("modifiedDate", issue.getDeltaTimestamp());
		parameters.put("url", null);
		parameters.put("isRegression", 0);
		parameters.put("status", issue.getStatus());
		parameters.put("resolution", issue.getResolution());
		parameters.put("severity", issue.getSeverity());
		parameters.put("priority", issue.getPriority());
		parameters.put("createdBy", createdByUserId);
		parameters.put("assignedTo", assigneeUserId);
		parameters.put("projectId", projectId);
		parameters.put("productId", productId);
		parameters.put("componentId", componentId);
		parameters.put("versionId", versionId);

		Number newId = insertPerson.executeAndReturnKey(parameters);
		long issueId = newId.longValue();

		// Step 6: Populate cc list of the issue
		Iterator<String> iter = issue.getCcIterator();
		while (iter.hasNext()) {
			String cc = iter.next();
			long userId = addUser(cc, projectId);
			getJdbcTemplate().update(
					"INSERT INTO cc_list (issueId,who) VALUES(?,?)", issueId,
					userId);
		}

		// Step 7: Populate issue communication / comments
		Iterator<LongDescription> longDescIter = issue
				.getLongDescriptionIterator();
		while (longDescIter.hasNext()) {
			LongDescription desc = longDescIter.next();
			Date date = desc.getWhen();
			String who = desc.getWho();
			long userId = addUser(who, projectId);
			getJdbcTemplate()
					.update("INSERT INTO issue_comment (who,fk_issueId,commentDate) VALUES(?,?,?)",
							userId, issueId, date);
		}

		// Step 8: add the bug history
		for (BugHistory record : bugHistoryList) {
			addBugHistory(record, issueId, projectId);
		}

		return issueId;
	}

	/**
	 * Populate the issue_history table with the history data of the issue
	 * 
	 * @param history
	 * @param issueId
	 * @param projectId
	 */
	private void addBugHistory(BugHistory history, long issueId, long projectId) {
		SimpleJdbcInsert insertPerson = new SimpleJdbcInsert(getDataSource())
				.withTableName("issue_history").usingGeneratedKeyColumns("id");
		Map<String, Object> parameters = new HashMap<String, Object>(2);
		parameters.put("field", history.getField());
		parameters.put("changeDate", history.getWhen());
		parameters.put("oldValue", history.getOldValue());
		parameters.put("newValue", history.getNewValue());
		parameters.put("issueId", issueId);

		// get the person who changed the history
		long userId = addUser(history.getWho(), projectId);
		parameters.put("who", userId);

		insertPerson.executeAndReturnKey(parameters);
	}

	/**
	 * Check if the issue already exists before adding, needed in case we are
	 * running it again and again
	 * 
	 * @param bugId
	 * @param projectId
	 * @return
	 */
	private long checkIfIssueExists(String bugId, long projectId) {
		long issueId = -1;
		if (null != bugId) {
			try {
				issueId = getJdbcTemplate()
						.queryForLong(
								"SELECT id FROM issue where projectId = ? and bugId = ?",
								new Object[] { projectId, bugId });
			} catch (EmptyResultDataAccessException e) {

			}
		}
		return issueId;
	}

	/**
	 * Check if the user with the given username or emailid exists in the
	 * database and return the id of the same
	 * 
	 * @param userName
	 * @param email
	 * @return
	 */
	private long checkIfUserExists(String userName, long projectId) {
		long userID = -1;
		// if name is null, check for only email
		// if name is not null, check for both
		if (null != userName) {
			try {
				userID = getJdbcTemplate()
						.queryForLong(
								"SELECT id FROM person where projectId = ? and name = ?",
								new Object[] { projectId, userName });
			} catch (EmptyResultDataAccessException e) {
				System.out.println("User with the name " + userName
						+ " not found");
			}
		}
		return userID;
	}

	/**
	 * Add User record into the database. For now we consider name to store
	 * email id (in case of Linux kernel bugzilla) TODO: This needs to change as
	 * we come up with a user addition strategy.
	 * 
	 * @param name
	 * @param emailIds
	 * @param projectId
	 * @return
	 */
	private long addUser(String name, long projectId) {
		long userId = -1;
		userId = checkIfUserExists(name, projectId);
		if (-1 == userId) {
			SimpleJdbcInsert insertPerson = new SimpleJdbcInsert(
					getDataSource()).withTableName("person")
					.usingGeneratedKeyColumns("id");
			Map<String, Object> parameters = new HashMap<String, Object>(2);
			parameters.put("name", name);
			parameters.put("projectId", projectId);
			Number newId = insertPerson.executeAndReturnKey(parameters);
			userId = newId.longValue();
		}
		return userId;
	}

	/**
	 * Check if the version number record is already present
	 * 
	 * @param version
	 * @param productId
	 * @return
	 */
	private long checkIfVersionExists(String version, long productId) {
		long versionId = -1;
		try {
			versionId = getJdbcTemplate()
					.queryForLong(
							"SELECT id FROM version where productId = ? and version = ?",
							productId, version);
		} catch (EmptyResultDataAccessException e) {
			System.out.println("Version with the value " + version
					+ " not found");
		}
		return versionId;
	}

	/**
	 * This method adds version details into the tables. It first checks if the
	 * data already exists and adds it only if it does not exists. If it does
	 * exists, it returns the id of the row (primary key)
	 * 
	 * @param version
	 * @param productId
	 * @return ID of the inserted row (Primary key) or the existsing row
	 */
	private long addVersion(String version, long productId) {
		long versionId = checkIfVersionExists(version, productId);
		if (versionId == -1) {
			SimpleJdbcInsert insertVersion = new SimpleJdbcInsert(
					getDataSource()).withTableName("version")
					.usingGeneratedKeyColumns("id");
			Map<String, Object> parameters = new HashMap<String, Object>(2);
			parameters.put("version", version);
			parameters.put("productId", productId);
			Number newId = insertVersion.executeAndReturnKey(parameters);
			versionId = newId.longValue();
		}
		return versionId;
	}

	/**
	 * Check if the component details exists in the table
	 * 
	 * @param name
	 * @param productId
	 * @return
	 */
	private long checkIfComponentExists(String name, long productId) {
		long componentId = -1;
		try {
			componentId = getJdbcTemplate()
					.queryForLong(
							"SELECT id FROM component where productId = ? and name = ?",
							productId, name);
		} catch (EmptyResultDataAccessException e) {
			System.out
					.println("Component with the name " + name + " not found");
		}
		return componentId;
	}

	/**
	 * This method adds component details into the tables. It first checks if
	 * the record is already present and adds only if its not present
	 * 
	 * @param name
	 * @param productId
	 * @return The Component ID of the inserted / existing record
	 */
	private long addComponent(String name, long productId) {
		long componentId = checkIfComponentExists(name, productId);
		if (componentId == -1) {
			SimpleJdbcInsert insertComponent = new SimpleJdbcInsert(
					getDataSource()).withTableName("component")
					.usingGeneratedKeyColumns("id");
			Map<String, Object> parameters = new HashMap<String, Object>(2);
			parameters.put("name", name);
			parameters.put("productId", productId);
			Number newId = insertComponent.executeAndReturnKey(parameters);
			componentId = newId.longValue();
		}
		return componentId;
	}

	/**
	 * Check if the product details already exists in the tables
	 * 
	 * @param name
	 * @param projectId
	 * @return
	 */
	private long checkIfProductExists(String name, long projectId) {
		long productId = -1;
		try {
			productId = getJdbcTemplate().queryForLong(
					"SELECT id from product where projectId = ? and name = ?",
					projectId, name);
		} catch (EmptyResultDataAccessException e) {
			System.out.println("Product with the name " + name + " not found");
		}
		return productId;
	}

	/**
	 * This method adds Product details into the Product table. It first checks
	 * if the record already exists and then adds only in case the record does
	 * not exist
	 * 
	 * @param name
	 * @param projectId
	 * @return The Project Id of the inserted / found record
	 */
	private long addProduct(String name, long projectId) {
		long productId = checkIfProductExists(name, projectId);
		if (productId == -1) {
			SimpleJdbcInsert insertProduct = new SimpleJdbcInsert(
					getDataSource()).withTableName("product")
					.usingGeneratedKeyColumns("id");
			Map<String, Object> parameters = new HashMap<String, Object>(2);
			parameters.put("name", name);
			parameters.put("projectId", projectId);
			Number newId = insertProduct.executeAndReturnKey(parameters);
			productId = newId.longValue();
		}
		return productId;
	}

	@Override
	/**
	 * Add Project details into the database
	 */
	public long addProject(String name, String issueTrackerURL,
			String mailingListURL) {
		return 0;
	}

}