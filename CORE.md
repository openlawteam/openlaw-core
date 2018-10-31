# OpenLaw Core

The Scala core of the OpenLaw project contains two sub-projects, `shared` and `client`.

The `shared` sub-project contains four key components: the oracles (in the `oracles` folder), a set of custom value types related to contracts and templates (in the `values` folder), the OpenLaw Markup Language parser and associated types and rules (in the `parser` folder), and the OpenLaw Virtual Machine (hereafter called "the VM") (in the `vm` folder).

## General Concepts

In order to understand these components, we need to understand some fundamental concepts within the OpenLaw protocol. The lifecycle of an agreement on the OpenLaw platform is as follows. It starts as a template: a legal agreement, marked up using [the OpenLaw Markup Language](https://docs.openlaw.io/markup-language), with empty fields corresponding to various provisions. When the user fills out some, but not all, of the fields, it becomes a draft. Once all fields have been filled out, the draft becomes a contract. A contract can then be sent to signatories and its signing will be registered and verified on the Ethereum blockchain. Signing by all parties will also cause any smart contracts embedded in the document to execute according to the provisions which the user has specified.

## VM

Under the hood, much of the workflow above is managed and moderated by the OpenLaw VM. The VM is a secure execution environment for contracts. It has several functions. First, it keeps track of the series of events over the contract's lifetime using the custom `OpenlawVmEvent` type. The VM uses `Oracles` to verify event requests. They will be discussed in greater detail [#Oracles](below).

Second, the VM keeps track of the state of the contract itself at any given time using the `OpenlawVmState` private variable. One important aspect of state involves the status of smart contracts embedded in the contract. The VM uses OpenLaw's Action API to query and store actions in the life of smart contracts. These actions may include starting, stopping, and resuming smart contract executions at time intervals which the user has specified.

In order for the legal agreement to be interpreted properly, the OpenLaw Markup Language, like many other computer languages, needs to know how to interpret and compile custom language types. The `VariableExecutionEngine` and its sub-type the `OpenlawExecutionEngine` help handle some of the logic for this. For example, the `processExecutedElement` function in the `OpenlawExecutionEngine` pattern matches on the [type of the variable](https://docs.openlaw.io/markup-language/#variables) to provide specific execution instructions.

The `OpenlawVmProvider` class contains just one method, `create`, which returns an `OpenlawVm` type. This is useful for integrating the `OpenlawVm` into server and other applications.

## Oracles

OpenLaw's oracles are designed in a modular way so that they can be integrated with mainnet, testnet, or other backends without much extra customization. In general, OpenLaw prefers to develop high-level components which are broadly functional, so that they can be plugged into a wide variety of environments and use cases, and the oracles are an example of that.

All oracles share a similar workflow:

  -Validate the event being passed to the VM. This is typically done using a cryptographic proof.

  -If the validation check passes, allow the event to occur.

Currently, the following oracles exist:

`OpenlawOracle`: This is a trait which `EthereumSmartContractOracle`, `ResumeContractOracle`, `StopContractOracle`, and `TemplateLoadOracle` extend. It requires passing in the type of the event to be executed if the validation is successful. For example, `EthereumSmartContractOracle` executes an `EthereumSmartContractCallEvent` if successful.

`EthereumSmartContractOracle`: This handles validation of events of type `EthereumSmartContractCallEvent` to process calls to execute embedded smart contracts.

`StopContractOracle`: This handles validation of events of type `StopExecutionEvent` to stop calls to execute embedded smart contracts.

`ResumeContractOracle`: This handles validation of events of type `ResumeExecutionEvent` to resume calls to execute embedded smart contracts which had previously been stopped with a `StopExecutionEvent`.

`TemplateLoadOracle`: This handles validation of events of type `LoadTemplate` to load a new/altered version of a template into the VM.

`OpenlawIdentityOracle`: This is a trait which `OpenlawSignatureOracle` extends. It contains helper functions and values such as `isSignatureValid` (for checking the validity of an incoming signature) and `providerId` (for storing the identity provider).

`OpenlawSignatureOracle`:  This handles validation of events of type `SignatureEvent` to process a signature on the Ethereum blockchain once the signature and Ethereum address of the signee have been verified.

In addition to the seven oracles mentioned above, the `oracles` folder also contains a `CryptoService` with helper functions for sha256 checksums and validating ECS signatures.

## Parser

The parser stores information regarding custom types in the OpenLaw Markup Language and how they should be processed. This folder has three sub-folders: `contract`, `field`, and `template`.

The `contract` folder is the simplest, containing just the `ParagraphEdits` type, which is used to represent paragraphs added to templates by the user.

The `fields` folder contains various `case objects` which extend the base type `ContractField`. They provide mappings between custom Scala types and field names in the `TemplateEditor` for an agreement. For example, a `Name` field in the editor corresponds to a custom `FullName` type internally.

The `template` folder is the most complex of the three. Definitions of features of the OpenLaw Markup Language and their expected behavior can be found here. In general, [the Markup Language docs](https://docs.openlaw.io/markup-language) are a solid companion to keep open while perusing this part of the codebase.

For example, the `formatters` folder contains all classes extending the `Formatter` trait, which corresponds to the formatting features of the markup language as discussed [in the documentation](https://docs.openlaw.io/markup-language/#formatting). The `EthereumSmartContractCall` type declared in `variableTypes` defines the logic for the [smart contracts](https://docs.openlaw.io/markup-language/#smart-contracts) described in the docs. (The eagle-eyed may also notice the correspondence with the `EthereumSmartContractCallEvent` we mentioned in our `EthereumSmartContractOracle` above). The custom `IdentityType`, also in `variableTypes`, corresponds to the `Identity` type [which must be added to any agreement before it can be sent for signature](https://docs.openlaw.io/markup-language/#identity-and-signatures).

Another key utility here is the `printers` sub-folder. This contains case classes extending the `HTMLAgreementPrinter` trait. These custom types specify various means of outputting a marked-up legal agreement in HTML, which may be useful if integrating an OpenLaw legal agreement into a custom web application.

Finally, but perhaps most crucially, the `OpenlawTemplateLanguageParserService` provides many essential methods for interacting with marked-up legal agreements, including compiling templates, parsing expressions, and rendering output based on variable type.

## Values
