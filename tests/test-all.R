# Script for running all unit tests
library(testthat)
source('cachematrix.R')

test_dir('tests/testthat', reporter = 'Summary')

