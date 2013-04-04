package de.siemens.quantarch.bugs.dao.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import de.siemens.quantarch.bugs.models.CustomIssue;

public class IssueRowMapper implements RowMapper<CustomIssue> {

	@Override
	public CustomIssue mapRow(ResultSet rs, int rowNum) throws SQLException {
		CustomIssue issue = new CustomIssue();
		issue.id = rs.getLong(1);
		issue.assignee = rs.getString(2);
		issue.reporter = rs.getString(3);
		return issue;
	}

}
