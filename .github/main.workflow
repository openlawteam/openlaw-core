workflow "Build and Test" {
  on = "push"
  resolves = ["Test", "LintersOK"]
}

action "Docker Registry Login" {
  uses = "actions/docker/login@master"
  secrets = ["DOCKER_USERNAME", "DOCKER_PASSWORD"]
}

# Currently adding a gh-* in front of branch name to avoid collisions since
# running on so many different CIs right now.**
action "Cached Build" {
  uses = "actions/docker/cli@master"
  runs = ["sh", "-c", "BRANCH=gh-${GITHUB_REF##*/} ci/build.sh"]
  needs = ["Docker Registry Login"]
  env = {
    PUSH_CACHE = "0"
  }
}

action "Caching Build" {
  uses = "actions/docker/cli@master"
  runs = ["sh", "-c", "BRANCH=gh-${GITHUB_REF##*/} ci/build.sh"]
  needs = ["Docker Registry Login"]
  env = {
    PUSH_CACHE = "1"
  }
}

action "Test" {
  uses = "docker://openlaw/core"
  runs = "scripts/test.sh"
  needs = ["Caching Build"]
}

# as an example, let's add the shellcheck linter for linting our shell scripts
action "Shellcheck Lint" {
  uses = "actions/bin/shellcheck@master"
  args = ["scripts/*.sh", "ci/*.sh"]
}

action "LintersOK" {
  uses = "actions/bin/sh@master"
  needs = ["Shellcheck Lint"]
  args = ["echo linters OK"]
}

# Coverage reports are much slower than normal tests, so run them in another
# workflow for now
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


# Release to BinTray flow
workflow "Publish on Release" {
  on = "release"
  resolves = ["Release"]
}

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
