#!/usr/bin/Rscript

library(dplyr,    quietly=TRUE, warn.conflicts = FALSE)
library(ggplot2,  quietly=TRUE, warn.conflicts = FALSE)
library(reshape2, quietly=TRUE, warn.conflicts = FALSE)
library(ggmap,    quietly=TRUE, warn.conflicts = FALSE)

barFactor = "size"
gridFacet = "art_ifreq"

args <- commandArgs(trailingOnly=TRUE)
file = args[2]
tors = args[1]
df = read.csv(file)
df.space <- df[, !(colnames(df) %in% c("time"))]
df.time  <- df[, !(colnames(df) %in% c("space"))]
df.space.long <- melt(df.space, id.vars = c("type", "min_depth", "art_ifreq", "size"))
df.time.long  <- melt(df.time,  id.vars = c("type", "min_depth", "art_ifreq", "size"))

X11()

if (tors == "space") {
    ggplot(df.space.long, aes(variable, value, fill=as.factor(df.space.long[,barFactor]))) +
      geom_bar(position="dodge", stat="identity") +
      facet_wrap(as.formula(paste("~", gridFacet, sep='')), nrow=2)
} else {
    ggplot(df.time.long, aes(variable, value, fill=as.factor(df.time.long[,barFactor]))) +
      geom_bar(position="dodge", stat="identity") +
      facet_wrap(as.formula(paste("~", gridFacet, sep='')), nrow=2)
}

gglocator(1)

#message("Press [Return] to close.")
#invisible(readLines("stdin", n=1))
