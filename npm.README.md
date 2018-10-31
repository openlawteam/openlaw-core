# openlaw

[![npm version](https://badge.fury.io/js/openlaw.svg)](https://badge.fury.io/js/openlaw)

The OpenLaw JavaScript APIs allow you to integrate and build on the OpenLaw protocol.

The class `APIClient` library is an interface for querying, saving, and changing data in an OpenLaw instance as well as for user authentication. The interface serves as a convenient wrapper to the OpenLaw REST API.

The `Openlaw` object library is an interface to interact directly with an OpenLaw legal agreement and its contents, including its various variable types.

## Documentation

Check out the guide for getting started, the complete reference for using the OpenLaw JavaScript APIs, and other information about how to use the OpenLaw application at our [docs](https://docs.openlaw.io/).

## Installation

```sh
$ npm install openlaw --save
```

## Example Usage

### `APIClient`

```js
const {APIClient} = require('openlaw');
// ES2015 modules
import {APIClient} from 'openlaw';

// Include the root URL for the OpenLaw instance.
apiClient = new APIClient('https://app.openlaw.io');

/*
Most of the APIClient method calls can only be made by a logged in
user with a StandardUser role or an Admin role. Log in before making
those calls.
*/
apiClient.login('openlawuser+1@gmail.com', 'password');
const templateDetails = apiClient.getTemplate('Advisor Agreement');

console.log(templateDetails);
/*
{
  "id": "d76ede8ca437f6da06b1e09f115393318faf29fdc5bdaaf0b2e889886136edf4",
  "title": "Advisor Agreement",
  "content": "This Advisor Agreement is entered into between [[Company Name: Text]] (\"Corporation\") and [[Advisor Name]] (\"Advisor\") as of [[Effective Date: Date]] (\"Effective Date\"). Company and Advisor agree as follows:  \n\n^ **Services**. Advisor agrees to consult with and advise Company from time to time, at Company's request (the \"Services\"). {{No Services \"Do you want to limit the advisor's services?\"  While this Agreement is is effect, Advisor will not provide services to any company active in the field of [[Noncompete Field \"What field should the advisor not participate in?\"]].}}\n\n...**COMPANY:**\n[[Company Signatory Email: Identity]]\n\n___________________\nName:  [[Company Signatory]]\nAddress:  [[Company Address: Address]]\n\n\n**ADVISOR:**\n[[Advisor Email: Identity]]\n\n___________________\nName [[Advisor Name]]      \nAddress: [[Advisor Address: Address]]\n",
  "templateType": "agreement"
}
*/
```

### `Openlaw`

```js
const {Openlaw} = require('openlaw');
// ES2015 modules
import {Openlaw} from 'openlaw';

const compiledTemplate = Openlaw.compileTemplate(
  'This Advisor Agreement is entered into between [[Company Name]] ("Corporation") and [[Advisor Name]] ("Advisor") as of [[Effective Date: Date]] ("Effective Date"). Company and Advisor agree as follows: \n\n^**Services**. Advisor agrees to consult with and advise Company from time to time, at Company\'s request (the "Services").'
);

console.log(compiledTemplate);
/*
{
  isError: false,
  errorMessage: "",
  compiledTemplate: CompiledTemplate
}
*/
```

### Import both exports from module

```js
// ES2015 modules
import {APIClient, Openlaw} from 'openlaw';
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