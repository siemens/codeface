SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

DROP SCHEMA IF EXISTS `quantarch` ;
CREATE SCHEMA IF NOT EXISTS `quantarch` DEFAULT CHARACTER SET utf8 ;
USE `quantarch` ;

-- -----------------------------------------------------
-- Table `quantarch`.`project`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`project` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`project` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NOT NULL ,
  `analysisMethod` VARCHAR(45) NOT NULL ,
  `analysisTime` DATETIME NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;

CREATE UNIQUE INDEX `name_UNIQUE` ON `quantarch`.`project` (`name` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`person`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`person` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`person` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NULL DEFAULT NULL ,
  `projectId` BIGINT NOT NULL ,
  `email1` VARCHAR(255) NOT NULL ,
  `email2` VARCHAR(255) NULL DEFAULT NULL ,
  `email3` VARCHAR(255) NULL DEFAULT NULL ,
  `email4` VARCHAR(255) NULL DEFAULT NULL ,
  `email5` VARCHAR(255) NULL DEFAULT NULL ,
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
  `modifiedDate` DATETIME NULL DEFAULT NULL ,
  `url` VARCHAR(255) NULL DEFAULT NULL ,
  `isRegression` INT(1) NULL DEFAULT 0 ,
  `status` VARCHAR(45) NOT NULL ,
  `resolution` VARCHAR(45) NULL DEFAULT NULL ,
  `priority` VARCHAR(45) NOT NULL ,
  `severity` VARCHAR(45) NOT NULL ,
  `createdBy` BIGINT NOT NULL ,
  `assignedTo` BIGINT NULL DEFAULT NULL ,
  `projectId` BIGINT NOT NULL ,
  `subComponent` VARCHAR(45) NULL DEFAULT NULL ,
  `subSubComponent` VARCHAR(45) NULL DEFAULT NULL ,
  `version` VARCHAR(45) NULL DEFAULT NULL ,
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
  `commentDate` DATETIME NULL DEFAULT NULL ,
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
-- Table `quantarch`.`release_timeline`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`release_timeline` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`release_timeline` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `type` VARCHAR(45) NOT NULL ,
  `tag` VARCHAR(45) NOT NULL ,
  `date` DATETIME NULL DEFAULT NULL ,
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
  `releaseRCStartId` BIGINT NULL DEFAULT NULL ,
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
-- Table `quantarch`.`mail_thread`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`mail_thread` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`mail_thread` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `subject` VARCHAR(255) NULL DEFAULT NULL ,
  `createdBy` BIGINT NULL DEFAULT NULL ,
  `projectId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `ml` VARCHAR(255) NOT NULL ,
  `mailThreadId` BIGINT NOT NULL ,
  `creationDate` DATETIME NULL DEFAULT NULL ,
  `numberOfAuthors` INT NOT NULL ,
  `numberOfMessages` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `mail_createdBy`
    FOREIGN KEY (`createdBy` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `mail_release_range_key`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
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

CREATE INDEX `mail_release_range_key_idx` ON `quantarch`.`mail_thread` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`mail_comments`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`mail_comments` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`mail_comments` (
  `who` BIGINT NOT NULL ,
  `mailThreadId` BIGINT NOT NULL ,
  `commentDate` DATETIME NULL DEFAULT NULL ,
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
  `ChangedFiles` INT NULL DEFAULT NULL ,
  `AddedLines` INT NULL DEFAULT NULL ,
  `DeletedLines` INT NULL DEFAULT NULL ,
  `DiffSize` INT NULL DEFAULT NULL ,
  `CmtMsgLines` INT NULL DEFAULT NULL ,
  `CmtMsgBytes` INT NULL DEFAULT NULL ,
  `NumSignedOffs` INT NULL DEFAULT NULL ,
  `NumTags` INT NULL DEFAULT NULL ,
  `general` INT NULL DEFAULT NULL ,
  `TotalSubsys` INT NULL DEFAULT NULL ,
  `Subsys` VARCHAR(45) NULL DEFAULT NULL ,
  `inRC` INT NULL DEFAULT NULL ,
  `AuthorSubsysSimilarity` FLOAT NULL DEFAULT NULL ,
  `AuthorTaggersSimilarity` FLOAT NULL DEFAULT NULL ,
  `TaggersSubsysSimilarity` FLOAT NULL DEFAULT NULL ,
  `releaseRangeId` BIGINT NULL DEFAULT NULL ,
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
  `added` INT NULL DEFAULT NULL ,
  `deleted` INT NULL DEFAULT NULL ,
  `total` INT NULL DEFAULT NULL ,
  `numcommits` INT NULL DEFAULT NULL ,
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
  `releaseRangeId` BIGINT NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `plot_project_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `plot_releaseRangeId_ref`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `plot_project_ref_idx` ON `quantarch`.`plots` (`projectId` ASC) ;

CREATE INDEX `plot_releaseRangeId_ref_idx` ON `quantarch`.`plots` (`releaseRangeId` ASC) ;


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
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `projectId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `clusterNumber` INT NULL DEFAULT NULL ,
  `clusterMethod` VARCHAR(45) NULL DEFAULT NULL ,
  `dot` BIGINT NULL DEFAULT NULL ,
  `svg` BIGINT NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) ,
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
    ON UPDATE CASCADE,
  CONSTRAINT `cluster_releaseRange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `project_cluster_ref_idx` ON `quantarch`.`cluster` (`projectId` ASC) ;

CREATE INDEX `dot_plot_bin_data_idx` ON `quantarch`.`cluster` (`dot` ASC) ;

CREATE INDEX `svg_plot_bin_data_ref_idx` ON `quantarch`.`cluster` (`svg` ASC) ;

CREATE INDEX `cluster_releaseRange_idx` ON `quantarch`.`cluster` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`cluster_user_mapping`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`cluster_user_mapping` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`cluster_user_mapping` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `personId` BIGINT NOT NULL ,
  `clusterId` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `cluster_cluster_user_ref`
    FOREIGN KEY (`clusterId` )
    REFERENCES `quantarch`.`cluster` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `person_cluster_user_ref`
    FOREIGN KEY (`personId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `cluster_cluster_user_ref_idx` ON `quantarch`.`cluster_user_mapping` (`clusterId` ASC) ;

CREATE INDEX `person_cluster_user_ref_idx` ON `quantarch`.`cluster_user_mapping` (`personId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`issue_history`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`issue_history` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`issue_history` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `changeDate` DATETIME NOT NULL ,
  `field` VARCHAR(45) NOT NULL ,
  `oldValue` VARCHAR(45) NULL DEFAULT NULL ,
  `newValue` VARCHAR(45) NULL DEFAULT NULL ,
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
  `value_scaled` DOUBLE NULL DEFAULT NULL ,
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
  `releaseRangeId` BIGINT NOT NULL ,
  `subject` TEXT NOT NULL ,
  `count` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `freq_subects_project_ref`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `freq_subjects_release_range_ref`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `freq_subects_project_ref_idx` ON `quantarch`.`freq_subjects` (`projectId` ASC) ;

CREATE INDEX `freq_subjects_release_range_ref_idx` ON `quantarch`.`freq_subjects` (`releaseRangeId` ASC) ;


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
  `releaseRangeId` BIGINT NOT NULL ,
  `technique` TINYINT NOT NULL ,
  `name` VARCHAR(45) NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `pagerank_releaserange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `pagerank_releaserange_idx` ON `quantarch`.`pagerank` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`pagerank_matrix`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`pagerank_matrix` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`pagerank_matrix` (
  `pageRankId` BIGINT NOT NULL ,
  `personId` BIGINT NOT NULL ,
  `rankValue` DOUBLE NOT NULL ,
  PRIMARY KEY (`pageRankId`, `personId`) ,
  CONSTRAINT `pagerankMatrix_pagerank`
    FOREIGN KEY (`pageRankId` )
    REFERENCES `quantarch`.`pagerank` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `pagerankMatrix_person`
    FOREIGN KEY (`personId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `pagerankMatrix_pagerank_idx` ON `quantarch`.`pagerank_matrix` (`pageRankId` ASC) ;

CREATE INDEX `pagerankMatrix_person_idx` ON `quantarch`.`pagerank_matrix` (`personId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`edgelist`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`edgelist` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`edgelist` (
  `clusterId` BIGINT NOT NULL ,
  `fromId` BIGINT NOT NULL ,
  `toId` BIGINT NOT NULL ,
  `weight` DOUBLE NOT NULL ,
  CONSTRAINT `edgelist_person_from`
    FOREIGN KEY (`fromId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `edgeList_person_to`
    FOREIGN KEY (`toId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `edgeList_cluster`
    FOREIGN KEY (`clusterId` )
    REFERENCES `quantarch`.`cluster` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `edgelist_person_from_idx` ON `quantarch`.`edgelist` (`fromId` ASC) ;

CREATE INDEX `edgelist_person_to_idx` ON `quantarch`.`edgelist` (`toId` ASC) ;

CREATE INDEX `edgeList_cluster_idx` ON `quantarch`.`edgelist` (`clusterId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`twomode_edgelist`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`twomode_edgelist` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`twomode_edgelist` (
  `releaseRangeId` BIGINT NOT NULL ,
  `source` CHAR(7) NOT NULL ,
  `ml` VARCHAR(255) NOT NULL ,
  `fromVert` BIGINT NOT NULL ,
  `toVert` VARCHAR(255) NOT NULL ,
  `weight` DOUBLE NOT NULL ,
  CONSTRAINT `twomode_edgelist_releaseRange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `twomode_edgelist_person`
    FOREIGN KEY (`fromVert` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `twomode_edgelist_releaseRange_idx` ON `quantarch`.`twomode_edgelist` (`releaseRangeId` ASC) ;

CREATE INDEX `twomode_edgelist_person_idx` ON `quantarch`.`twomode_edgelist` (`fromVert` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`twomode_vertices`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`twomode_vertices` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`twomode_vertices` (
  `releaseRangeId` BIGINT NOT NULL ,
  `source` CHAR(7) NOT NULL ,
  `ml` VARCHAR(255) NOT NULL ,
  `name` VARCHAR(255) NOT NULL ,
  `degree` DOUBLE NOT NULL ,
  `type` SMALLINT NOT NULL ,
  CONSTRAINT `twomode_vertices_releaseRange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `twomode_vertices_releaseRange_idx` ON `quantarch`.`twomode_vertices` (`releaseRangeId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`initiate_response`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`initiate_response` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`initiate_response` (
  `releaseRangeId` BIGINT NOT NULL ,
  `ml` VARCHAR(255) NOT NULL ,
  `personId` BIGINT NOT NULL ,
  `source` TINYINT NOT NULL ,
  `responses` INT NULL DEFAULT NULL ,
  `initiations` INT NULL DEFAULT NULL ,
  `responses_received` INT NULL DEFAULT NULL ,
  `deg` DOUBLE NULL DEFAULT NULL ,
  CONSTRAINT `initiate_response_releaseRange`
    FOREIGN KEY (`releaseRangeId` )
    REFERENCES `quantarch`.`release_range` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `initiate_response_person`
    FOREIGN KEY (`personId` )
    REFERENCES `quantarch`.`person` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `initiate_response_releaseRange_idx` ON `quantarch`.`initiate_response` (`releaseRangeId` ASC) ;

CREATE INDEX `initiate_response_person_idx` ON `quantarch`.`initiate_response` (`personId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`mailing_list`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`mailing_list` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`mailing_list` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `projectId` BIGINT NOT NULL ,
  `name` VARCHAR(128) NOT NULL ,
  `description` VARCHAR(255) NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `mailing_lists_projectid`
    FOREIGN KEY (`projectId` )
    REFERENCES `quantarch`.`project` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

CREATE INDEX `mailing_lists_projectid_idx` ON `quantarch`.`mailing_list` (`projectId` ASC) ;


-- -----------------------------------------------------
-- Table `quantarch`.`per_cluster_statistics`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `quantarch`.`per_cluster_statistics` ;

CREATE  TABLE IF NOT EXISTS `quantarch`.`per_cluster_statistics` (
  `projectId` BIGINT NOT NULL ,
  `releaseRangeId` BIGINT NOT NULL ,
  `clusterId` BIGINT NOT NULL ,
  `technique` TINYINT NOT NULL ,
  `num_members` INT(11) NOT NULL ,
  `added` INT(11) NOT NULL ,
  `deleted` INT(11) NOT NULL ,
  `total` INT(11) NOT NULL ,
  `numcommits` INT(11) NOT NULL ,
  `prank_avg` DOUBLE NOT NULL )
ENGINE = InnoDB;

USE `quantarch` ;

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`revisions_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`revisions_view` (`projectId` INT, `releaseRangeID` INT, `date_start` INT, `date_end` INT, `date_rc_start` INT, `tag` INT, `cycle` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`author_commit_stats_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`author_commit_stats_view` (`Name` INT, `ID` INT, `releaseRangeId` INT, `added` INT, `deleted` INT, `total` INT, `numcommits` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`per_person_cluster_statistics_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`per_person_cluster_statistics_view` (`'projectId'` INT, `'releaseRangeId'` INT, `'clusterId'` INT, `'personId'` INT, `'technique'` INT, `'rankValue'` INT, `'added'` INT, `'deleted'` INT, `'total'` INT, `'numcommits'` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`cluster_user_pagerank_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`cluster_user_pagerank_view` (`id` INT, `personId` INT, `clusterId` INT, `technique` INT, `rankValue` INT);

-- -----------------------------------------------------
-- Placeholder table for view `quantarch`.`per_cluster_statistics_view`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `quantarch`.`per_cluster_statistics_view` (`'projectId'` INT, `'releaseRangeId'` INT, `'clusterId'` INT, `technique` INT, `'num_members'` INT, `'added'` INT, `'deleted'` INT, `'total'` INT, `'numcommits'` INT, `'prank_avg'` INT);

-- -----------------------------------------------------
-- procedure update_per_cluster_statistics
-- -----------------------------------------------------

USE `quantarch`;
DROP procedure IF EXISTS `quantarch`.`update_per_cluster_statistics`;

DELIMITER $$
USE `quantarch`$$
CREATE PROCEDURE `quantarch`.`update_per_cluster_statistics` ()
BEGIN
	TRUNCATE per_cluster_statistics;
	INSERT INTO per_cluster_statistics SELECT * FROM per_cluster_statistics_view;
END$$

DELIMITER ;

-- -----------------------------------------------------
-- View `quantarch`.`revisions_view`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`revisions_view` ;
DROP TABLE IF EXISTS `quantarch`.`revisions_view`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`revisions_view` AS
SELECT 
	p.id as projectId,
	rr.id as releaseRangeID,
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
-- View `quantarch`.`per_person_cluster_statistics_view`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`per_person_cluster_statistics_view` ;
DROP TABLE IF EXISTS `quantarch`.`per_person_cluster_statistics_view`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`per_person_cluster_statistics_view` AS
select 
    rr.projectId as 'projectId',
    rr.id as 'releaseRangeId',
    c.id as 'clusterId',
    p.id as 'personId',
	pr.technique as 'technique',
	prm.rankValue as 'rankValue',
    sum(acs.added) as 'added',
    sum(acs.deleted) as 'deleted',
    sum(acs.total) as 'total',
    sum(acs.numcommits) as 'numcommits'
from release_range rr INNER JOIN (cluster c, cluster_user_mapping cum, person p, author_commit_stats acs, pagerank pr, pagerank_matrix prm)
	ON (rr.id = c.releaseRangeId
		AND c.id = cum.clusterId
        AND cum.personId = p.id
		AND rr.id = acs.releaseRangeId
		AND p.id = acs.authorId
		AND rr.id = pr.releaseRangeID
		AND pr.id = prm.pageRankId
		AND p.id = prm.personId)
group by rr.projectId , rr.id , c.id , p.id, pr.technique, prm.rankValue;

-- -----------------------------------------------------
-- View `quantarch`.`cluster_user_pagerank_view`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`cluster_user_pagerank_view` ;
DROP TABLE IF EXISTS `quantarch`.`cluster_user_pagerank_view`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`cluster_user_pagerank_view` AS
SELECT
	cum.id, 
	cum.personId,
	cum.clusterId AS clusterId,
	pr.technique,
	prm.rankValue
FROM
	cluster_user_mapping cum
	INNER JOIN (cluster c, pagerank_matrix prm, pagerank pr)
	ON (cum.personId = prm.personId AND
	    cum.clusterId = c.id AND
	    prm.pageRankId = pr.id AND
	    c.releaseRangeId = pr.releaseRangeId);

-- -----------------------------------------------------
-- View `quantarch`.`per_cluster_statistics_view`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `quantarch`.`per_cluster_statistics_view` ;
DROP TABLE IF EXISTS `quantarch`.`per_cluster_statistics_view`;
USE `quantarch`;
CREATE  OR REPLACE VIEW `quantarch`.`per_cluster_statistics_view` AS
select 
    rr.projectId as 'projectId',
    rr.id as 'releaseRangeId',
    c.id as 'clusterId',
	pr.technique,
    count(p.id) as 'num_members',
    sum(acs.added) as 'added',
    sum(acs.deleted) as 'deleted',
    sum(acs.total) as 'total',
    sum(acs.numcommits) as 'numcommits',
	avg(prm.rankValue) as 'prank_avg'
from release_range rr INNER JOIN (cluster c, cluster_user_mapping cum, person p, author_commit_stats acs, pagerank pr, pagerank_matrix prm)
	ON (rr.id = c.releaseRangeId
		AND c.id = cum.clusterId
        AND cum.personId = p.id
		AND rr.id = acs.releaseRangeId
		AND p.id = acs.authorId
		AND rr.id = pr.releaseRangeID
		AND pr.id = prm.pageRankId
		AND p.id = prm.personId)
group by rr.projectId , rr.id , c.id, pr.technique;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
