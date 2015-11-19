#!/usr/bin/Rscript

library(dplyr,    quietly=TRUE, warn.conflicts = FALSE)
library(ggplot2,  quietly=TRUE, warn.conflicts = FALSE)
library(reshape2, quietly=TRUE, warn.conflicts = FALSE)
#library(ggmap,    quietly=TRUE, warn.conflicts = FALSE)
library(cowplot,  quietly=TRUE, warn.conflicts = FALSE)

barFactor = "size"
gridFacet = "art_ifreq"

args <- commandArgs(trailingOnly=TRUE)
file = args[1]
df = read.csv(file)

df.summary <- df %>%
    group_by(type, min_depth, art_ifreq, size) %>%
    summarize(  time = median(time)
             , space =   mean(space)) %>%
    ungroup() %>%
    filter(type != "")

df.space <- df.summary[, !(colnames(df.summary) %in% c("time"))]
df.time  <- df.summary[, !(colnames(df.summary) %in% c("space"))]

df.long <- melt(df, id.vars = c("type", "min_depth", "art_ifreq", "size"))
df.space.long <- melt(df.space, id.vars = c("type", "min_depth", "art_ifreq", "size"))
df.time.long  <- melt(df.time,  id.vars = c("type", "min_depth", "art_ifreq", "size"))

df.long
#print.data.frame(df.time.long)

X11()

space <- ggplot(df.space.long, aes(variable, value, fill=as.factor(size))) +
    geom_bar(position="dodge", stat="identity") +
    facet_grid(~ art_ifreq + min_depth)

time <- ggplot(df.time.long, aes(variable, value, fill=as.factor(size))) +
    geom_bar(position="dodge", stat="identity") +
    facet_grid(~ art_ifreq + min_depth)

plot_grid(space, time, nrow=2, ncol=1)

gglocator(1)

message("Press [Return] to close.")
invisible(readLines("stdin", n=1))
