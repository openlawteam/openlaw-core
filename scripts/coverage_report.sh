#!/bin/sh
# Runs tests with coverage, and uploads final report to codacy.
#
# This takes significantly longer than just running the tests on their own, so
# we tend to run this in a separate concurrent phase to enable fast test
# failure.

# run the tests and create coverage files
sbt clean coverage test
# export scoverage report files into cobertura compatible files
sbt coverageReport
# if you have sub-projects, you will want to aggregate all reports into one
sbt coverageAggregate

# upload results to Codacy
UPLOAD_RESULTS=${UPLOAD_RESULTS:-0}
if [ "$UPLOAD_RESULTS" -eq "1" ]; then
    if [ -z "$CODACY_PROJECT_TOKEN" ]; then
        echo "No CODACY_PROJECT_TOKEN env variable! Can't upload results."
        exit 1
    else
        sbt codacyCoverage
    fi
fi
