CREATE VIEW `quantarch`.`revisions` AS
SELECT 
	p.id as projectId,
	rt_s.date as date_start, 
	rt_e.date as date_end, 
	rt_rs.date as date_rc_start, 
	rt_s.tag as tag, 
	concat(rt_s.tag,'-',rt_e.tag) as cycle
FROM 
	release_range rr JOIN release_timeline rt_s ON rr.releaseStartId = rt_s.id
	JOIN release_timeline rt_e ON rr.releaseEndId = rt_e.id
	LEFT JOIN release_timeline rt_rs ON rr.releaseRCStartId = rt_rs.id
	JOIN project p ON rr.projectId = p.id
order by rr.id asc

CREATE VIEW `quantarch`.`author_commit_stats_view` AS 
SELECT person.name as Name, author_commit_stats.author as ID, author_commit_stats.releaseRangeId, author_commit_stats.added, author_commit_stats.deleted, author_commit_stats.total, author_commit_stats.numcommits
FROM author_commit_stats,person
WHERE author IN (select distinct(author) FROM author_commit_stats) 
      AND person.id=author_commit_stats.author
GROUP BY author, releaseRangeId;
