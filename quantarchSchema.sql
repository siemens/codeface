SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

DROP SCHEMA IF EXISTS `quantarch` ;
CREATE SCHEMA IF NOT EXISTS `quantarch` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `quantarch` ;

-- -----------------------------------------------------
-- Table `quantarch`.`project`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`project` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`project` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NOT NULL ,
  `analysisMethod` VARCHAR(45) NOT NULL ,
  `analysisTime` DATETIME NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;

CREATE UNIQUE INDEX `name_UNIQUE` ON `quantarch`.`project` (`name` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`person`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`person` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`person` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NULL ,
  `projectId` BIGINT NOT NULL ,
  `email1` VARCHAR(255) NOT NULL ,
  `email2` VARCHAR(255) NULL ,
  `email3` VARCHAR(255) NULL ,
  `email4` VARCHAR(255) NULL ,
  `email5` VARCHAR(255) NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `person_projectId`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `person_projectId_idx` ON `quantarch`.`person` (`projectId` ASC) ;

CREATE UNIQUE INDEX `person_email_project_idx` ON `quantarch`.`person` (`projectId` ASC, `email1` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `bugId` VARCHAR(45) NOT NULL ,
  `creationDate` DATETIME NOT NULL ,
  `modifiedDate` DATETIME NULL ,
  `url` VARCHAR(255) NULL ,
  `isRegression` INT(1) NULL DEFAULT 0 ,
  `status` VARCHAR(45) NOT NULL ,
  `resolution` VARCHAR(45) NULL ,
  `priority` VARCHAR(45) NOT NULL ,
  `severity` VARCHAR(45) NOT NULL ,
  `createdBy` BIGINT NOT NULL ,
  `assignedTo` BIGINT NULL ,
  `projectId` BIGINT NOT NULL ,
  `subComponent` VARCHAR(45) NULL ,
  `subSubComponent` VARCHAR(45) NULL ,
  `version` VARCHAR(45) NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `issue_createdBy`
    FOREIGN KEY (`createdBy` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `issue_assignedTo`
    FOREIGN KEY (`assignedTo` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE SET NULL
    ON UPDATE CASCADE,
  CONSTRAINT `issue_projectId`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `issue_createdBy_idx` ON `quantarch`.`issue` (`createdBy` ASC) ;

CREATE INDEX `issue_assignedTo_idx` ON `quantarch`.`issue` (`assignedTo` ASC) ;

CREATE INDEX `issue_projectId_idx` ON `quantarch`.`issue` (`projectId` ASC) ;

CREATE UNIQUE INDEX `bugId_UNIQUE` ON `quantarch`.`issue` (`bugId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue_comment`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue_comment` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue_comment` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `who` BIGINT NOT NULL ,
  `fk_issueId` BIGINT NOT NULL ,
  `commentDate` DATETIME NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `fk_issueId`
    FOREIGN KEY (`fk_issueId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `issue_comment_who`
    FOREIGN KEY (`who` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `fk_issueId_idx` ON `quantarch`.`issue_comment` (`fk_issueId` ASC) ;

CREATE INDEX `issue_comment_who_idx` ON `quantarch`.`issue_comment` (`who` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`mail_thread`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`mail_thread` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`mail_thread` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `subject` VARCHAR(255) NULL ,
  `createdBy` BIGINT NOT NULL ,
  `projectId` BIGINT NOT NULL ,
  `creationDate` DATETIME NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `mail_createdBy`
    FOREIGN KEY (`createdBy` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `mail_projectId`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `mail_createdBy_idx` ON `quantarch`.`mail_thread` (`createdBy` ASC) ;

CREATE INDEX `mail_projectId_idx` ON `quantarch`.`mail_thread` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`mail_comments`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`mail_comments` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`mail_comments` (
  `who` BIGINT NOT NULL ,
  `mailThreadId` BIGINT NOT NULL ,
  `commentDate` DATETIME NULL ,
  CONSTRAINT `mail_comment_who`
    FOREIGN KEY (`who` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `mailThreadId`
    FOREIGN KEY (`mailThreadId` )
    REFERENCES `quantarch`.`mail_thread` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `mail_comment_who_idx` ON `quantarch`.`mail_comments` (`who` ASC) ;

CREATE INDEX `mailThreadId_idx` ON `quantarch`.`mail_comments` (`mailThreadId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`release_timeline`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`release_timeline` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`release_timeline` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `type` VARCHAR(45) NOT NULL ,
  `tag` VARCHAR(45) NOT NULL ,
  `date` DATETIME NULL ,
  `projectId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `release_project_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `release_project_ref_idx` ON `quantarch`.`release_timeline` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`release_range`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`release_range` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`release_range` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `releaseStartId` BIGINT NOT NULL ,
  `releaseEndId` BIGINT NOT NULL ,
  `projectId` BIGINT NOT NULL ,
  `releaseRCStartId` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `releaseRange_releaseStartId`
    FOREIGN KEY (`releaseStartId` )
    REFERENCES `quantarch`.`release_timeline` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `releaseRange_releaseEndId`
    FOREIGN KEY (`releaseEndId` )
    REFERENCES `quantarch`.`release_timeline` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `releaseRange_projectId`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `releaseRange_RCStartId`
    FOREIGN KEY (`releaseRCStartId` )
    REFERENCES `quantarch`.`release_timeline` (`id` )
    ON DELETE SET NULL
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `releaseRange_releaseStartId_idx` ON `quantarch`.`release_range` (`releaseStartId` ASC) ;

CREATE INDEX `releaseRange_releaseEndId_idx` ON `quantarch`.`release_range` (`releaseEndId` ASC) ;

CREATE INDEX `releaseRange_projectId_idx` ON `quantarch`.`release_range` (`projectId` ASC) ;

CREATE INDEX `releaseRange_RCStartId_idx` ON `quantarch`.`release_range` (`releaseRCStartId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`cc_list`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`cc_list` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`cc_list` (
  `issueId` BIGINT NOT NULL ,
  `who` BIGINT NOT NULL ,
  CONSTRAINT `cclist_issueId`
    FOREIGN KEY (`issueId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `cclist_who`
    FOREIGN KEY (`who` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `cclist_issueId_idx` ON `quantarch`.`cc_list` (`issueId` ASC) ;

CREATE INDEX `cclist_who_idx` ON `quantarch`.`cc_list` (`who` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`commit`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`commit` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`commit` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `commitHash` VARCHAR(255) NOT NULL ,
  `commitDate` DATETIME NOT NULL ,
  `author` BIGINT NOT NULL ,
  `projectId` BIGINT NOT NULL ,
  `ChangedFiles` INT NULL ,
  `AddedLines` INT NULL ,
  `DeletedLines` INT NULL ,
  `DiffSize` INT NULL ,
  `CmtMsgLines` INT NULL ,
  `CmtMsgBytes` INT NULL ,
  `NumSignedOffs` INT NULL ,
  `NumTags` INT NULL ,
  `general` INT NULL ,
  `TotalSubsys` INT NULL ,
  `Subsys` VARCHAR(45) NULL ,
  `inRC` INT NULL ,
  `AuthorSubsysSimilarity` FLOAT NULL ,
  `AuthorTaggersSimilarity` FLOAT NULL ,
  `TaggersSubsysSimilarity` FLOAT NULL ,
  `releaseRangeId` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `commit_person`
    FOREIGN KEY (`author` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `commit_project`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `commit_release_range`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE SET NULL
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `commit_person_idx` ON `quantarch`.`commit` (`author` ASC) ;

CREATE INDEX `commit_project_idx` ON `quantarch`.`commit` (`projectId` ASC) ;

CREATE INDEX `commit_release_end_idx` ON `quantarch`.`commit` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`commit_communication`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`commit_communication` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`commit_communication` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `commitId` BIGINT NOT NULL ,
  `who` BIGINT NOT NULL ,
  `communicationType` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `commitcom_commit`
    FOREIGN KEY (`commitId` )
    REFERENCES `quantarch`.`commit` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `commitcom_person`
    FOREIGN KEY (`who` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `commtcom_commit_idx` ON `quantarch`.`commit_communication` (`commitId` ASC) ;

CREATE INDEX `commitcom_person_idx` ON `quantarch`.`commit_communication` (`who` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue_duplicates`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue_duplicates` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue_duplicates` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `originalBugId` BIGINT NOT NULL ,
  `duplicateBugId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `original_issue_duplicate`
    FOREIGN KEY (`originalBugId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `duplicate_issue_duplicate`
    FOREIGN KEY (`duplicateBugId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `original_issue_duplicate_idx` ON `quantarch`.`issue_duplicates` (`originalBugId` ASC) ;

CREATE INDEX `duplicate_issue_duplicate_idx` ON `quantarch`.`issue_duplicates` (`duplicateBugId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue_dependencies`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue_dependencies` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue_dependencies` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `originalIssueId` BIGINT NOT NULL ,
  `dependentIssueId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `dependent_original_issue`
    FOREIGN KEY (`originalIssueId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dependent_dependent_issue`
    FOREIGN KEY (`dependentIssueId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `dependent_original_issue_idx` ON `quantarch`.`issue_dependencies` (`originalIssueId` ASC) ;

CREATE INDEX `dependent_dependent_issue_idx` ON `quantarch`.`issue_dependencies` (`dependentIssueId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`author_commit_stats`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`author_commit_stats` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`author_commit_stats` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `authorId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `added` INT NULL ,
  `deleted` INT NULL ,
  `total` INT NULL ,
  `numcommits` INT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `author_person_key`
    FOREIGN KEY (`authorId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `releaseRangeId_key`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `author_person_key_idx` ON `quantarch`.`author_commit_stats` (`authorId` ASC) ;

CREATE INDEX `releaseRangeId_key_idx` ON `quantarch`.`author_commit_stats` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`plots`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`plots` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`plots` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(45) NOT NULL ,
  `projectId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `plot_project_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `plot_project_ref_idx` ON `quantarch`.`plots` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`plot_bin`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`plot_bin` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`plot_bin` (
  `plotID` BIGINT NOT NULL ,
  `type` VARCHAR(45) NOT NULL ,
  `data` LONGBLOB NOT NULL ,
  CONSTRAINT `plot_bin_plot_ref`
    FOREIGN KEY (`plotID` )
    REFERENCES `quantarch`.`plots` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `plot_bin_plot_ref_idx` ON `quantarch`.`plot_bin` (`plotID` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`cluster`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`cluster` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`cluster` (
  `clusterId` BIGINT NOT NULL AUTO_INCREMENT ,
  `projectId` BIGINT NOT NULL ,
  `clusterNumber` INT NULL ,
  `clusterMethod` VARCHAR(45) NULL ,
  `dot` BIGINT NULL ,
  `svg` BIGINT NULL ,
  PRIMARY KEY (`clusterId`) ,
  CONSTRAINT `project_cluster_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dot_plot_bin_data`
    FOREIGN KEY (`dot` )
    REFERENCES `quantarch`.`plot_bin` (`plotID` )
    ON DELETE SET NULL
    ON UPDATE CASCADE,
  CONSTRAINT `svg_plot_bin_data_ref`
    FOREIGN KEY (`svg` )
    REFERENCES `quantarch`.`plot_bin` (`plotID` )
    ON DELETE SET NULL
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `project_cluster_ref_idx` ON `quantarch`.`cluster` (`projectId` ASC) ;

CREATE INDEX `dot_plot_bin_data_idx` ON `quantarch`.`cluster` (`dot` ASC) ;

CREATE INDEX `svg_plot_bin_data_ref_idx` ON `quantarch`.`cluster` (`svg` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`cluster_user_mapping`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`cluster_user_mapping` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`cluster_user_mapping` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `person` BIGINT NOT NULL ,
  `clusterId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `cluster_cluster_user_ref`
    FOREIGN KEY (`clusterId` )
    REFERENCES `quantarch`.`cluster` (`clusterId` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `person_cluster_user_ref`
    FOREIGN KEY (`person` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `cluster_cluster_user_ref_idx` ON `quantarch`.`cluster_user_mapping` (`clusterId` ASC) ;

CREATE INDEX `person_cluster_user_ref_idx` ON `quantarch`.`cluster_user_mapping` (`person` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue_history`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue_history` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue_history` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `changeDate` DATETIME NOT NULL ,
  `field` VARCHAR(45) NOT NULL ,
  `oldValue` VARCHAR(45) NULL ,
  `newValue` VARCHAR(45) NULL ,
  `who` BIGINT NOT NULL ,
  `issueId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `issue_history_issue_map`
    FOREIGN KEY (`issueId` )
    REFERENCES `quantarch`.`issue` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `issue_history_person_map`
    FOREIGN KEY (`who` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `issue_history_issue_map_idx` ON `quantarch`.`issue_history` (`issueId` ASC) ;

CREATE INDEX `issue_history_person_map_idx` ON `quantarch`.`issue_history` (`who` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`url_info`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`url_info` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`url_info` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `projectId` BIGINT NOT NULL ,
  `type` VARCHAR(45) NOT NULL ,
  `url` TEXT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `url_info_project`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `url_info_project_idx` ON `quantarch`.`url_info` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`timeseries`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`timeseries` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`timeseries` (
  `plotId` BIGINT NOT NULL ,
  `time` DATETIME NOT NULL ,
  `value` DOUBLE NOT NULL ,
  `value_scaled` DOUBLE NULL ,
  CONSTRAINT `plot_time_double_plot_ref`
    FOREIGN KEY (`plotId` )
    REFERENCES `quantarch`.`plots` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `plot_time_double_plot_ref_idx` ON `quantarch`.`timeseries` (`plotId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`freq_subjects`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`freq_subjects` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`freq_subjects` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `projectId` BIGINT NOT NULL ,
  `subject` TEXT NOT NULL ,
  `count` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `freq_subects_project_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `freq_subects_project_ref_idx` ON `quantarch`.`freq_subjects` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`thread_info`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`thread_info` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`thread_info` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `numberOfAuthors` INT NOT NULL ,
  `numberOfMessages` INT NOT NULL ,
  `mailThreadId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `mailThread_thread_info_ref`
    FOREIGN KEY (`mailThreadId` )
    REFERENCES `quantarch`.`mail_thread` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `mailThread_thread_info_ref_idx` ON `quantarch`.`thread_info` (`mailThreadId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`thread_density`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`thread_density` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`thread_density` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `num` DOUBLE NOT NULL ,
  `density` DOUBLE NOT NULL ,
  `type` VARCHAR(45) NOT NULL ,
  `projectId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `project_thread_density_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `project_thread_density_ref_idx` ON `quantarch`.`thread_density` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`pagerank`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`pagerank` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`pagerank` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `clusterId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `technique` VARCHAR(45) NULL ,
  `name` VARCHAR(45) NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `pagerank_releaserange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `pagerank_cluster`
    FOREIGN KEY (`clusterId` )
    REFERENCES `quantarch`.`cluster` (`clusterId` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `pagerank_releaserange_idx` ON `quantarch`.`pagerank` (`releaseRangeId` ASC) ;

CREATE INDEX `pagerank_cluster_idx` ON `quantarch`.`pagerank` (`clusterId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`pagerank_matrix`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`pagerank_matrix` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`pagerank_matrix` (
  `pageRankId` BIGINT NOT NULL ,
  `personID` BIGINT NOT NULL ,
  `rankValue` DOUBLE NOT NULL ,
  CONSTRAINT `pagerankMatrix_pagerank`
    FOREIGN KEY (`pageRankId` )
    REFERENCES `quantarch`.`pagerank` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `pagerankMatrix_person`
    FOREIGN KEY (`personID` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `pagerankMatrix_pagerank_idx` ON `quantarch`.`pagerank_matrix` (`pageRankId` ASC) ;

CREATE INDEX `pagerankMatrix_person_idx` ON `quantarch`.`pagerank_matrix` (`personID` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`graph`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`graph` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`graph` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `clusterId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `technique` VARCHAR(45) NULL ,
  `name` VARCHAR(45) NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `graph_releaserange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `graph_cluster`
    FOREIGN KEY (`clusterId` )
    REFERENCES `quantarch`.`cluster` (`clusterId` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `graph_releaserange_idx` ON `quantarch`.`graph` (`releaseRangeId` ASC) ;

CREATE INDEX `graph_cluster_idx` ON `quantarch`.`graph` (`clusterId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`edgelist`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`edgelist` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`edgelist` (
  `graphId` BIGINT NOT NULL ,
  `fromId` BIGINT NOT NULL ,
  `toId` BIGINT NOT NULL ,
  `weight` DOUBLE NOT NULL ,
  CONSTRAINT `edgelist_graph`
    FOREIGN KEY (`graphId` )
    REFERENCES `quantarch`.`graph` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `edgelist_person_from`
    FOREIGN KEY (`fromId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `edgeList_person_to`
    FOREIGN KEY (`toId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `edgelist_person_from_idx` ON `quantarch`.`edgelist` (`fromId` ASC) ;

CREATE INDEX `edgelist_graph_idx` ON `quantarch`.`edgelist` (`graphId` ASC) ;

CREATE INDEX `edgelist_person_to_idx` ON `quantarch`.`edgelist` (`toId` ASC) ;

USE `quantarch` ;

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`revisions`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`revisions` (`projectId` INT, `date_start` INT, `date_end` INT, `date_rc_start` INT, `tag` INT, `cycle` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`author_commit_stats_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`author_commit_stats_view` (`Name` INT, `ID` INT, `releaseRangeId` INT, `added` INT, `deleted` INT, `total` INT, `numcommits` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`per_cluster_statistics`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`per_cluster_statistics` (`'projectId'` INT, `'releaseRangeId'` INT, `'group'` INT, `'personId'` INT, `'added'` INT, `'deleted'` INT, `'total'` INT, `'numcommits'` INT, `'prank'` INT);

-- -----------------------------------------------------
-- View `quantarch`.`revisions`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`revisions` ;
DROP TABLE IF EXISTS `quantarch`.`revisions`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`revisions` AS
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
order by rr.id asc;

-- -----------------------------------------------------
-- View `quantarch`.`author_commit_stats_view`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`author_commit_stats_view` ;
DROP TABLE IF EXISTS `quantarch`.`author_commit_stats_view`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`author_commit_stats_view` AS

SELECT 
	p.name as Name, 
	s.authorId as ID, 
	s.releaseRangeId, 
	sum(s.added) as added, 
	sum(s.deleted) as deleted, 
	sum(s.total) as total, 
	sum(s.numcommits) as numcommits
FROM author_commit_stats s join person p on p.id = s.authorId
WHERE 
s.authorId IN 
	(	select distinct(authorId) 
		FROM author_commit_stats) 
GROUP BY s.authorId, p.name, s.releaseRangeId;

-- -----------------------------------------------------
-- View `quantarch`.`per_cluster_statistics`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`per_cluster_statistics` ;
DROP TABLE IF EXISTS `quantarch`.`per_cluster_statistics`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`per_cluster_statistics` AS
select
	rr.projectId as 'projectId',
	rr.id as 'releaseRangeId',
	c.clusterId as 'group',
	p.id as 'personId',
	sum(acs.added) as 'added',
	sum(acs.deleted) as 'deleted',
	sum(acs.total) as 'total',
	sum(acs.numcommits) as 'numcommits',
	avg(prm.rankValue) as 'prank'
from (((((release_range rr join author_commit_stats acs on rr.id = acs.releaseRangeId)
		join Person p on  p.id = acs.authorId)
		join cluster_user_mapping cum on cum.person = p.id)
		join cluster c on cum.clusterId = c.clusterId)
		join pagerank pr on pr.clusterId = c.clusterId AND pr.releaseRangeId = rr.id)
		join pagerank_matrix prm on prm.pageRankId = pr.id AND p.id = prm.personId
group by rr.projectId, rr.id, c.clusterId, p.id;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
