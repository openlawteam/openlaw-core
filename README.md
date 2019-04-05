# Shared Scala libraries for the OpenLaw project.

[![CircleCI](https://circleci.com/gh/openlawteam/openlaw-core.svg?style=svg)](https://circleci.com/gh/openlawteam/openlaw-core)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/4fe8f703ef3546738530fdcc585ccd2d)](https://www.codacy.com?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=openlawteam/openlaw-core&amp;utm_campaign=Badge_Grade)

## Getting started
If you want to learn more about this library, please read our [OpenLaw core overview](https://docs.openlaw.io/openlaw-core/).

If you want to use OpenLaw core in your Scala project, here is how to add it to your sbt project:

```
// First add our repository 
resolvers += "Openlaw core" at "https://dl.bintray.com/openlawos/openlaw-core"

//add the dependency
libraryDependencies += "org.openlaw" %% "openlaw-core" % "<last version>"

```

## Contributing 

See information about contributing [here](CONTRIBUTING.md).

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
