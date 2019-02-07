#!/bin/sh
# Runs tests with coverage, and uploads final report to codacy.
#
# Dependencies:
#   $CODACY_PROJECT_TOKEN
sbt coverage test
sbt coverageReport
sbt coverageAggregate
