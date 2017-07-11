#! /usr/bin/env Rscript

## This file is part of Codeface. Codeface is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2017 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

s <- suppressPackageStartupMessages
s(library(ggplot2))
s(library(lubridate))
s(library(dplyr))
s(library(gtable))
s(library(GGally))
s(library(corrplot))

source("query.r")
source("utils.r")
source("config.r")
source("dependency_analysis.r")
source("quality_analysis.r")
source("conway_data.r")

make.title <- function(conf, motif.type) {
    return(str_c(conf$description, " (window: ", conf$windowSize, " months, motif: ",
                 motif.type, ", comm: ", conf$communicationType, ")"))
}

## Visualise the multi-source conway data, and compute statistical correlations and
## their significance
create.conway.correlation.plots <- function(artifacts.dat, quality.type, corr.plot.path,
                                            motif.type, communication.type, lag.type) {
    artifacts.dat.aug <- augment.artifact.data(artifacts.dat, quality.type)

    corr.elements <- gen.conway.artifact.columns(quality.type)
    artifacts.subset <- artifacts.dat.aug[, corr.elements$names]
    colnames(artifacts.subset) <- corr.elements$labels

    ## NOTE: We deliberately use artifacts.dat.aug and then subset the columns because
    ## working directly with artifacts.subset does not work -- the column names
    ## are not in a suitable format for ggpairs()
    correlation.plot <- ggpairs(artifacts.dat.aug,
                                columns=corr.elements$names,
                                lower=list(continuous=wrap("points",
                                               alpha=0.33, size=0.5)),
                                upper=list(continuous=wrap('cor',
                                               method='spearman'))) +
                                theme_bw() +
                  theme(axis.title.x = element_text(angle = 90, vjust = 1,
                            color = "black"))

    corr.mat <- cor(artifacts.subset, use="pairwise.complete.obs",
                    method="spearman")
    if (nrow(artifacts.subset) == 0) {
        loginfo(str_c("Conway analysis: No correlation data available, exiting early", sep=""),
                startlogger="conway")
        return()
    }

    ## Compute the p values for each correlation tests
    corr.test <- cor.mtest(artifacts.subset)

    ## Provide plots to visualise the data
    pdf(file.path(corr.plot.path, str_c("correlation_plot_", lag.type, ".pdf")),
        width=10, height=10)
    print(correlation.plot)
    dev.off()

    if (!is.null(corr.test)) {
        pdf(file.path(corr.plot.path, str_c("correlation_plot_color_",
                                            lag.type, ".pdf")),
            width=7, height=7)
        corrplot(corr.mat, p.mat=corr.test[[1]],
                 insig = "p-value", sig.level=0.05, method="pie", type="lower")
        dev.off()
    }
}

do.quality.analysis <- function(conf, cycles, i, motif.type, global.resdir) {
    ## Combine quality (defect, corrections) information with motif/anti-motif counts
    ## and data obtained from the VCS analysis
    artifact.type <- conf$artifactType
    quality.type <- conf$qualityType
    communication.type <- conf$communicationType
    range.resdir <- file.path(global.resdir, gen.range.path(i, cycles[i,]$cycle))
    corr.path <- file.path(range.resdir, "quality_analysis", motif.type,
                           communication.type)
    networks.dir <- file.path(range.resdir, "motif_analysis", motif.type,
                              conf$communicationType)
    
    ## If the isochronous quality file for the cycle is not available
    ## (because of missing data), retarded and advanced correlations cannot
    ## be computed
    if (!file.exists(file.path(corr.path, "quality_data_isochronous.csv"))) {
        loginfo(str_c("Isochronous quality data for range ", i, " unavailable, exitting early"),
                logger="conway")
        return()
    }

    artifacts.dat.list <- merge.conway.data(conf, cycles, i, motif.type,
                                        communication.type, quality.type,
                                        global.resdir, networks.dir, do.variants=TRUE)

    null <- lapply(1:length(artifacts.dat.list), function(j) {
        lag.type <- names(artifacts.dat.list)[j]
        create.conway.correlation.plots(artifacts.dat.list[[j]], quality.type,
                                        corr.path, motif.type,
                                        communication.type, lag.type)
    })
}

dispatch.all <- function(conf, resdir, motif.type) {
    cycles <- get.cycles(conf)

    if (is.null(conf$windowSize)) {
        conf$windowSize <- 3
    }

    for (i in 1:nrow(cycles)) {
        do.quality.analysis(conf, cycles, i, motif.type, resdir)
    }

    ## Compute correlation values time series and plot the result
    corr.dat <- get.correlations.ts(conf, resdir, motif.type)
    if (is.null(corr.dat)) {
        logerror(str_c("No conway results available for ", motif.type, " motifs -- exitting early ",
                       "(did no range contain communication relations?)", sep=""), logger="conway")
        return()
    }

    plot.file <- file.path(resdir, str_c("correlations_ts_", motif.type, "_",
                                        conf$communicationType, ".pdf"))
    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")

    corr.label <- "Correlation"
    ## To avoid plotting too many lines into a single graph, split
    ## the available correlation data in chunks of 6 lines per panel.
    series.per.panel <- 6    # This is the maximal is symbols in ggplot2
    num.panels <- ceiling(length(unique(corr.dat$combination))/series.per.panel)
    map.panels <- data.frame(combination=unique(corr.dat$combination),
                             panel=ceiling(seq_along(unique(corr.dat$combination))/series.per.panel))
    corr.dat$panel <- corr.dat$combination
    corr.dat$panel <- mapvalues(corr.dat$panel, map.panels$combination, map.panels$panel)
    plots <- lapply(1:num.panels, function(i) {
        g <- ggplot(corr.dat[corr.dat$panel==i,], aes(x=date, y=value,
                                                      colour=combination, shape=combination)) +
             geom_point() + geom_line() + scale_x_date("", date_labels="%m-%Y") +
             ylab("Correlation") +  theme_bw() +
             expand_limits(y=c(-1,1))

        shape.colour.label <- ""
        if (i==1) {
            g <- g + ggtitle(make.title(conf, motif.type))
            shape.colour.label <- corr.label
        } else if (i==num.panels) {
            g <- g + xlab("Date")
        }

        g <- g + scale_colour_discrete(shape.colour.label) +
                 scale_shape_discrete(shape.colour.label)
        return(g)
    })

    ## Combine all plots into a column panel with some ggplot2 and gtable magic
    grobs <- lapply(plots, ggplotGrob)
    g <- gtable_col("plots",grobs, unit(7, "in"), unit(rep(3, num.panels), c("in")))
    ggsave(plot.file, g, width=7, height=3*num.panels)

    ## ###############################################################
    ## Compute a time series with absolute data counts
    res <- get.conway.artifact.data.ts(conf, resdir, motif.type)

    plot.file <- file.path(resdir, str_c("abs_ts_", motif.type, "_",
                                         conf$communicationType, ".pdf"))
    labels <- c(motif.count = "Motifs", motif.anti.count = "Anti-Motifs",
                motif.ratio="Motif Ratio")
    dat <- prepare.abs.ts(res)

    g <- ggplot(dat, aes(x=dev.count, y=value)) + geom_point(size=0.5) +
        facet_grid(variable~date, labeller=labeller(variable=labels), scale="free_y") +
            scale_x_sqrt("# Devs contributing to artifact [sqrt]") +
    scale_y_sqrt("Count or Ratio [sqrt]") + geom_smooth(method=lm) + theme_bw() +
        ggtitle(make.title(conf, motif.type))
    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
    ggsave(plot.file, g, width=2*length(unique(dat$date)), height=5, limitsize=FALSE)


    ## ######################################
    ## Plot time series with absolute data counts for the previous correlation computations
    if (conf$communicationType == "jira") {
        labels <- c(motif.count = "Motifs", motif.anti.count = "Anti-Motifs",
                    motif.ratio="Motif Ratio")
        dat <- prepare.abs.bug.ts(res)

        plot.file <- file.path(resdir, str_c("abs_bug_ts1_", motif.type, "_",
                                             conf$communicationType, ".pdf"))
        g <- ggplot(dat, aes(x=Churn, y=value)) + geom_point(size=0.5) +
            facet_grid(variable~date, labeller=labeller(variable=labels), scale="free_y") +
            scale_x_log10("Churn [log]") + scale_y_continuous("Count or Ratio") +
            geom_smooth(method=lm) + theme_bw() +
            ggtitle(make.title(conf, motif.type))
        logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
        ggsave(plot.file, g, width=2*length(unique(dat$date)), height=5, limitsize=FALSE)

        ## #################################################################
        plot.file <- file.path(resdir, str_c("abs_bug_ts2_", motif.type, "_",
                                             conf$communicationType, ".pdf"))
        g <- ggplot(dat, aes(x=BugIssueCount, y=value)) + geom_point(size=0.5) +
            facet_grid(variable~date, labeller=labeller(variable=labels), scale="free_y") +
            scale_x_continuous("Bug Issue Count") + scale_y_continuous("Count or Ratio") +
            geom_smooth(method=lm) + theme_bw() + ggtitle(make.title(conf, motif.type))
        logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
        ggsave(plot.file, g, width=2*length(unique(dat$date)), height=5, limitsize=FALSE)


        ## #################################################################
        plot.file <- file.path(resdir, str_c("jira_ts_abs_", motif.type, "_",
                                             conf$communicationType, ".pdf"))
        dat <- res[,c("CountLineCode", "bug.density", "Churn",
                      "BugIssueCount", "date", "range")]
        dat$Churn <- log(dat$Churn+1)
        dat$CountLineCode <- log(dat$CountLineCode+1)
        labels <- c(CountLineCode="LoC [log]", BugIssueCount="Bug Issue Count",
                    Churn="Code Churn [log]", bug.density="Bug Density")

        dat.molten <- melt(dat, id.vars=c("date", "range"))
        g <- ggplot(dat.molten, aes(x=date, y=value)) + geom_boxplot(aes(x=date, group=date)) +
            facet_grid(variable~., scales="free_y", labeller=labeller(variable=labels)) +
            scale_x_date("Date", date_labels="%m-%Y") + expand_limits(y=0) +
            ylab("") + theme_bw() + ggtitle(make.title(conf, motif.type))

        logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
        ggsave(plot.file, g, width=7, height=8)
    }

    ## ############

    plot.file <- file.path(resdir, str_c("norm_ts_", motif.type, "_",
                                         conf$communicationType, ".pdf"))
    labels.norm <- c(motif.count.norm = "Motifs", motif.anti.count.norm = "Anti-Motifs",
                     motif.ratio="Motif Ratio")
    dat <- res[,c("motif.count.norm", "motif.anti.count.norm", "motif.ratio",
                  "dev.count", "date", "range")]
    dat.molten <- melt(dat, measure.vars=c("motif.count.norm", "motif.anti.count.norm",
                                           "motif.ratio"))

    g <- ggplot(dat.molten, aes(x=dev.count, y=value)) + geom_point(size=0.5) +
        facet_grid(variable~date, labeller=labeller(variable=labels.norm), scale="free_y") +
            scale_x_sqrt("# Devs contributing to artifact [sqrt]") +
    scale_y_continuous("Normalised count or ratio") + geom_smooth(method=lm) + theme_bw() +
        ggtitle(make.title(conf, motif.type))
    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
    ggsave(plot.file, g, width=2*length(unique(dat$date)), height=5, limitsize=FALSE)


    plot.file <- file.path(resdir, str_c("motif_count_ts_", motif.type, "_",
                                         conf$communicationType, ".pdf"))
    labels.norm <- c(motif.count = "Motifs", motif.anti.count = "Anti-Motifs")
    dat <- res[,c("motif.count", "motif.anti.count", "dev.count", "date", "range")]

    g <- ggplot(dat, aes(x=motif.count, y=motif.anti.count)) +
        geom_point(size=0.75) + facet_grid(~date) + scale_x_continuous("# Motifs") +
        scale_y_continuous("# Anti-Motifs") + geom_smooth(method=lm) + theme_bw() +
        ggtitle(make.title(conf, motif.type))
    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
    ggsave(plot.file, g, width=2*length(unique(dat$date)), height=4, limitsize=FALSE)


    ## ###########################################################
    ## Prepare a global "timeseries" plot of the null model tests
    res <- read.motif.results(conf, resdir, motif.type)

    labels <- c(negative = "Anti-Motif", positive = "Motif", ratio = "Ratio")
    g <- ggplot(data=res, aes(x=count)) +
        geom_point(aes(x=empirical.count), y=0, color="red", size=2.5) +
        geom_density(aes(x=count, y=..scaled..), alpha=.2, fill="#AAD4FF") +
        facet_wrap(count.type~date, nrow=3, scales="free_x",
                   labeller=labeller(count.type=labels)) +
        xlab("Count or Ratio") + ylab("Density [a.u.]") +
        ggtitle(make.title(conf, motif.type)) + theme_bw()

    plot.file <- file.path(resdir, str_c("motif_null_model_ts_", motif.type, "_",
                                         conf$communicationType, ".pdf"))
    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
    ggsave(plot.file, g, width=2*length(unique(dat$date)), height=6, limitsize=FALSE)

    ## #####################################################
    ## Plot a time series with absolute empirical motif counts
    plot.file <- file.path(resdir, str_c("motif_ts_abs_", motif.type, "_",
                                         conf$communicationType, ".pdf"))
    g <- ggplot(res, aes(x=as.Date(date), y=empirical.count)) + geom_point() + geom_line() +
        facet_grid(count.type~., scales="free_y", labeller=labeller(count.type=labels)) +
        scale_x_date("Date", date_labels="%m-%Y") + expand_limits(y=0) +
        ylab("Count or Ratio") + theme_bw() + ggtitle(make.title(conf, motif.type))

    logdevinfo(str_c("Saving plot to ", plot.file), logger="conway")
    ggsave(plot.file, g, width=7, height=4)
}

config.script.run({
    conf <- config.from.args(positional.args=list("resdir"), require.project=TRUE)
    for (motif.type in c("triangle", "square")) {
        dispatch.all(conf, conf$resdir, motif.type)
    }
})
