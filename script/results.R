#!/usr/bin/env Rscript

library(dplyr, quietly=TRUE, warn.conflicts = FALSE)

usage <- "Usage: ./results-list.R ../out/some-test-name.csv (one | all | NA)"
demandAll <- "all"
demandOne <- "one"
demandNA  <- "NA"
argsExpected <- 2

args    <- commandArgs(trailingOnly = TRUE)
if (length(args) != argsExpected) { stop(usage) }
path    <- args[1]
demand  <- args[2]
results <- read.csv(path)

# Table 1 is broken into two pieces:
# a: Demand all (Demand.. = 100.0)
# b: Demand one (Demand.. = 0.0)
# Note: "Demand.." column is "Demand %"
if (demand == demandOne) {
    results <- filter(results, Demand.. == 0.0)
} else if (demand == demandAll) {
    results <- filter(results, Demand.. > 0.0)
} else if (demand == demandNA) {
    demand <- "all" # For display purposes
} else {
    stop(usage)
}

# Remove rows that are not reported.
results <- filter(results,
                  !(Test == "initial" |
                    Test == "final" |
                    Test == "rr-replace2"))

# Remove columns that are not reported.
results <- results[c("Seed", "Version", "Test", "Time")]

# Rename the internal values to nicer display values.
results$Test <- as.character(results$Test)
results$Test[results$Test == "id-delete"]   <- "Delete"
results$Test[results$Test == "id-insert"]   <- "Insert"
results$Test[results$Test == "rr-replace1"] <- "Replace"
results$Version <- as.character(results$Version)
results$Version[grepl(".*_name$", results$Version)] <-
    "Nominal Adapton"
results$Version[grepl(".*_arggen$", results$Version)] <-
    "Structural Adapton"
results$Version[grepl(".*_(lazyrecalc|eagernoninc)$", results$Version)] <-
    "From Scratch"

# It's possible to insert/delete/replace from different locations
# inside a list, so we tested many different locations. We take the
# average of the different locations when reporting the results.
results <- results %>%
    group_by(Seed, Test, Version) %>%
    summarize(Time = mean(Time)) %>%
    ungroup()

# We run each test a number of times, and take the median
# of those runs as our final result.
results <- results %>%
    group_by(Test, Version) %>%
    summarize(Time = median(Time)) %>%
    ungroup()

# We report the speedup of Nominal Adapton and Structural Adapton wrt.
# the from-scratch runtime, so we need to add a speedup column. The
# speedup is the From Scratch runtime divided by the Adapton runtime.
get.value <- function (df, test, artlib, col) {
  return(as.numeric(df[df$Version == artlib & df$Test == test,][col]))
}
results <- results %>%
    rowwise() %>%
    mutate(Speedup = get.value(results, Test, "From Scratch", "Time") /
                     get.value(results, Test, Version,        "Time"))

cat(paste0("Results for ", path, " (demand ", demand, ")\n"))
print.data.frame(results)
