#!/bin/sh
# Runs tests with coverage, and uploads final report to codacy.
#
# Dependencies:
#   $CODACY_PROJECT_TOKEN

# run the tests and create coverage files
sbt clean coverage test
# export scoverage report files into cobertura compatible files
sbt coverageReport
# if you have sub-projects, you will want to aggregate all reports into one
sbt coverageAggregate
# upload results to Codacy (TODO: RE-ENABLE, CURRRENTLY DISABLED)
# sbt codacyCoverage
