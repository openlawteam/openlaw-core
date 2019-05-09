# SHARED: actions used as dependencies in multiple workflows.
action "Docker Registry Login" {
  uses = "actions/docker/login@master"
  secrets = ["DOCKER_USERNAME", "DOCKER_PASSWORD"]
}

action "Cached Build" {
  uses = "actions/docker/cli@master"
  runs = ["sh", "-c", "BRANCH=$GITHUB_REF ID=gh ci/build.sh"]
  needs = ["Docker Registry Login"]
  env = {
    PUSH_CACHE = "0"
  }
}

action "Caching Build" {
  uses = "actions/docker/cli@master"
  runs = ["sh", "-c", "BRANCH=$GITHUB_REF ID=gh ci/build.sh"]
  needs = ["Docker Registry Login"]
  env = {
    PUSH_CACHE = "1"
  }
}

# WORKFLOW: Build and test on all pushes.
workflow "Build and Test" {
  on = "push"
  resolves = ["Test"]
}

action "Test" {
  uses = "docker://openlaw/core"
  runs = "scripts/test.sh"
  needs = ["Caching Build"]
}

# WORKFLOW: Run linters on all pushes.
#
# For now, this just lints the shell scripts used for CI themselves. In the
# future, we will likely want to introduce some more linters (scalafmt?).
#
# This is done as a separate workflow from the build&test since we may want to
# handle linters here in the future for convenience even if we move build and
# test somewhere else for performance reasons.
workflow "Linters" {
  on = "push"
  resolves = ["Shellcheck Lint"]
}

action "Shellcheck Lint" {
  uses = "actions/bin/shellcheck@master"
  args = ["scripts/*.sh", "ci/*.sh"]
}


# WORKFLOW: Run coverage reporter on all pushes.
# 
# Coverage reports are much slower than normal tests, so run them in a separate
# workflow from normal tests, which should be on a different machine in GitHub
# Actions land. Note the future, we may wish to disable coverage reporting of
# this type due to Codacy costs(?).
workflow "Coverage Reporter" {
  resolves = ["Coverage Report"]
  on = "push"
}

action "Coverage Report" {
  uses = "docker://openlaw/core"
  runs = "scripts/coverage_report.sh"
  secrets = ["CODACY_PROJECT_TOKEN"]
  env = {
    UPLOAD_RESULTS = "1"
  }
  needs = ["Cached Build"]
}


# WORKFLOW: Publish to Bintray on Release
#
# TODO: Currently this is fired 3x on release due to GH API particulars, may 
# need to convert to being a tag filter at end of push like other platforms or
# use an action filter.
workflow "Publish on Release" {
  on = "release"
  resolves = ["Release"]
}

# verify the release tag began with a semver indicator
action "SemVer Tag Filter" {
  uses = "actions/bin/filter@master"
  args = "tag v*"
}

action "Release" {
  uses = "docker://openlaw/core"
  runs = "scripts/release.sh"
  secrets = ["BINTRAY_USER", "BINTRAY_PASS"]
  env = {
    RELEASE_TRIGGER = "false"
  }
  needs = ["SemVer Tag Filter", "Caching Build"]
}
