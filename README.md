# Shared libraries for the OpenLaw project.

[![Waffle.io - Columns and their card count](https://badge.waffle.io/openlawteam/openlaw-core.svg?columns=all)](https://waffle.io/openlawteam/openlaw-core)

## Getting started
If you want to learn more about this library, please read our [OpenLaw core overview](https://docs.openlaw.io/openlaw-core/), which describes the Scala portion of the code.

To use OpenLaw core in your JavaScript project, you can use our npm package with 
`npm install openlaw --save`. You can find further instructions for how to use the library [here](npm.README.md) and in our [docs](https://docs.openlaw.io).

If you want to use OpenLaw core in your Scala project, here is how to add it to your sbt project:

```scala
// First add our repository 
resolvers += "https://openlaw.bintray.com/openlaw-core"

//add the dependency
libraryDependencies += "org.openlaw" % "openlaw-core_2.12" & "<last version>"

```

## Troubleshooting 

You need to do `sbt "project openlawCoreJs" fullOptJS` before the first time you run `npm run build`. Otherwise, you will see an error like the below:

```
ERROR in Entry module not found: Error: Can't resolve '/$YOUR_DIR/openlaw-core/client/target/scala-2.12/client.js' in '/$YOUR_DIR/openlaw-core'.
```

## Contributing 

See information about contributing [here](CONTRIBUTING.md).

## License

Copyright 2018 Aaron Wright, David Roon, and ConsenSys AG.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
