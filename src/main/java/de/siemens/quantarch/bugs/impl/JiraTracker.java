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

package de.siemens.quantarch.bugs.impl;

import de.siemens.quantarch.bugs.IssueTracker;
import de.siemens.quantarch.bugs.dao.IssueTrackerDao;
import de.siemens.quantarch.bugs.utils.BugExtractorConfig;

@SuppressWarnings("unused")
public class JiraTracker implements IssueTracker {

	private IssueTrackerDao dao;
	private BugExtractorConfig config;

	@Override
	public void setDao(IssueTrackerDao dao) {
		this.dao = dao;
	}

	@Override
	public void setConfig(BugExtractorConfig config) {
		this.config = config;
	}

	@Override
	public void parseIssues() {
		System.out.println("Not yet implemented");
	}

}
