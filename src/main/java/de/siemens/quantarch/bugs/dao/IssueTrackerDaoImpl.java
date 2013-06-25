package de.siemens.quantarch.bugs.dao;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.simple.SimpleJdbcInsert;
import org.springframework.jdbc.core.support.JdbcDaoSupport;

import b4j.core.Issue;
import b4j.core.LongDescription;
import de.siemens.quantarch.bugs.history.BugHistory;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;
import de.siemens.quantarch.personService.PersonServiceClient;

public class IssueTrackerDaoImpl extends JdbcDaoSupport implements
		IssueTrackerDao {

	private BugExtractorConfig projectConfig = null;

	public IssueTrackerDaoImpl(DataSource dataSource,
			BugExtractorConfig projectConfig) {
		setDataSource(dataSource);
		this.projectConfig = projectConfig;
	}

	@Override
	public long addIssue(Issue issue, long projectId,
			List<BugHistory> bugHistoryList) {

		// Step 4: Add the users
		// a> Created By
		long createdBy = PersonServiceClient.getPerson(issue.getReporterName(),
				issue.getReporter(), projectId,
				projectConfig.getPersonServiceURL());
		long assignee = PersonServiceClient.getPerson(issue.getAssigneeName(),
				issue.getAssignee(), projectId,
				projectConfig.getPersonServiceURL());

		// Step 5: Add the issue
		// now add the issue record
		SimpleJdbcInsert insertPerson = new SimpleJdbcInsert(getDataSource())
				.withTableName("issue").usingGeneratedKeyColumns("id");
		Map<String, Object> parameters = new HashMap<String, Object>();
		parameters.put("bugId", issue.getId());
		parameters.put("creationDate", issue.getCreationTimestamp());
		parameters.put("modifiedDate", issue.getDeltaTimestamp());
		parameters.put("url", null);
		parameters.put("isRegression", 0);
		parameters.put("status", issue.getStatus());
		parameters.put("resolution", issue.getResolution());
		parameters.put("severity", issue.getSeverity());
		parameters.put("priority", issue.getPriority());
		parameters.put("createdBy", createdBy);
		parameters.put("assignedTo", assignee);
		parameters.put("projectId", projectId);

		if (projectConfig.isProductAsProject()) {
			// when product is parsed as a project
			// component becomes subComponent and subSubComponent is null
			parameters.put("subComponent", issue.getComponent());
		} else {
			parameters.put("subComponent", issue.getProduct());
			parameters.put("subSubComponent", issue.getComponent());
		}

		parameters.put("version", issue.getVersion());

		Number newId = insertPerson.executeAndReturnKey(parameters);
		long issueId = newId.longValue();

		// Step 6: Populate cc list of the issue
		Iterator<String> iter = issue.getCcIterator();
		while (iter.hasNext()) {
			String cc = iter.next();
			long ccUser = PersonServiceClient.getPerson(null, cc, projectId,
					projectConfig.getPersonServiceURL());
			getJdbcTemplate().update(
					"INSERT INTO cc_list (issueId,who) VALUES(?,?)", issueId,
					ccUser);
		}

		// Step 7: Populate issue communication / comments
		Iterator<LongDescription> longDescIter = issue
				.getLongDescriptionIterator();
		while (longDescIter.hasNext()) {
			LongDescription desc = longDescIter.next();
			Date date = desc.getWhen();
			String who = desc.getWho();
			long commentUser = PersonServiceClient.getPerson(
					desc.getAuthorName(), who, projectId,
					projectConfig.getPersonServiceURL());
			getJdbcTemplate()
					.update("INSERT INTO issue_comment (who,fk_issueId,commentDate) VALUES(?,?,?)",
							commentUser, issueId, date);
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
		long commentUser = PersonServiceClient.getPerson(null,
				history.getWho(), projectId,
				projectConfig.getPersonServiceURL());
		parameters.put("who", commentUser);
		insertPerson.executeAndReturnKey(parameters);
	}

	@Override
	public long getProjectId(String name) {
		long projectId = -1;
		try {
			projectId = getJdbcTemplate().queryForObject(
					"SELECT id FROM project WHERE name = ?",
					new Object[] { name }, Long.class);
		} catch (EmptyResultDataAccessException e) {
			return -1;
		}
		return projectId;
	}
}