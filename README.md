# Shared Scala libraries for the OpenLaw project

[![CodeFactor](https://www.codefactor.io/repository/github/openlawteam/openlaw-core/badge)](https://www.codefactor.io/repository/github/openlawteam/openlaw-core)

## Getting started

If you want to learn more about this library, please read our [OpenLaw core overview](https://docs.openlaw.io/openlaw-core/).

If you want to use OpenLaw core in your Scala project, here is how to add it to your sbt project:

```scala
// First add our repository
resolvers += "Openlaw core" at "https://dl.bintray.com/openlawos/openlaw-core"

//add the dependency
libraryDependencies += "org.openlaw" %% "openlaw-core" % "<last version>"
```

## Contributing

Want to get involved? See detailed information about contributing [here](docs/CONTRIBUTING.md)!

### Versioning

Project versioning is automatically maintained from [Semantic Versioning](https://semver.org/spec/v2.0.0.html) formatted tags via `sbt-git`.

<small>If the most recent release was versioned `v1.2.3`, you may notice your local version is something like `1.2.3-7-a1b2c3d`. This means you are `7` commits past release `1.2.3`, and the latest commit was SHA `a1b2c3d`.</small>

### Code Formatting

We adhere to standardized code formatting via
[scalafmt](https://scalameta.org/scalafmt/). All PRs will be automatically
checked for adherence. If they do not adhere to standardized formatting, they
should be corrected prior to being merged.

The best way is let your editor handle everything for you everytime you hit
save, see the [Scalafmt Installation
Docs](https://scalameta.org/scalafmt/docs/installation.html) or the OpenLaw
developer setup documentation.

To manually verify, can run `make lint-style` to check and `make lint-style-fix`
to automatically repair all files.

### Release Process

For project maintainers, our current release process is documented [here](docs/ReleaseProcess.md).

## License

Copyright 2019 Aaron Wright, David Roon, and ConsenSys AG.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
