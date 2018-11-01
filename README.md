Shared libraries for the OpenLaw project.

## Getting started
If you want to know more about this library, please read [this](CORE.md).

To use Openlaw core in your javascript project, you can use our npm package with 
`npm install openlaw --save`. You can find further instructions for how to use the library [here](npm.README.md).

If you want to use Openlaw core in your scala project, here is how to add it to your sbt project:

```scala
// First add our repository 
resolvers += "https://openlaw.bintray.com/openlaw-core"

//add the dependency
libraryDependencies += "org.openlaw" % "openlaw-core" & "<last version>"

```


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
