#!/usr/bin/env Rscript

install <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
}

