# OpenLaw Core

The core of the OpenLaw project is the OpenLaw Virtual Machine (OpenLaw VM).

In order to understand the VM, we need to understand some fundamental concepts within OpenLaw. The OpenLaw VM is an execution environment for OpenLaw smart contracts. But this raises the question - how is a smart contract represented in OpenLaw under the hood?

The contract is represented by a custom type, `ContractDefinition`, which contains the following parameters:

1) `creatorId`: The creator of the contract with type `UserId`. 
2) `mainTemplate`: The main template of the contract with type `TemplateId`, analagous to the main class in a program.
3) `templates`: Other templates which may be included in the contract with type `Map[String, TemplateId]`. 
4) `parameters`: The parameters of the contract, as input by the user in the form in the OpenLaw UI, with type `TemplateParameters`.
5) `paragraphs`: Paragraphs within the contract which have been edited by the user in the template editor in the OpenLaw UI, with type `Map[TemplateId, ParagraphEdits]`.

On its own, the `ContractDefinition` type simply holds information. It needs the VM in order to be executed. 

Each `Contract` has an `id` which is a hash representing the contract, based on the creator ID, ID of the main template,
all the parameters, and the template scope. This hash gives significant security advantages. First, as with any hash, since its id will change if any of the above changes, it serves as a useful, unique means of identifying the contract,
as well as determining if it has been tampered with. Second, though the hash will be stored on the mainnet, it will be difficult to derive any useful information from the hash alone. 
Finally, since a contract has multiple signatories, any attempts at tampering from one signatory's side will not get very far, since the versions between the two signatories have diverged, as indicated by the conflicting hashes,
and thus the consensus will fail. 

The VM keeps track of the series of events over the life of the contract. This is done by loading `Oracles`, which are used for processing and verifying information about what happens to the contract. The oracles are designed in a modular way so that they can be integrated with mainnet, testnet, or other backends without much extra customization. In general, OpenLaw prefers to develop high-level components which are broadly functional, so that they can be plugged into a wide variety of environments and use cases, and the oracles are an example of that.

The `OpenlawVmService` class creates the VM. When the VM is created, it contains the `ContractDefinition` discussed above; the `CryptoService` used for cryptographic verification (see more below); the `OpenlawTemplateLanguageParserService`, which is used for parsing the template using the OpenLaw Markup Language; and all the oracles. 

Digging a bit deeper into the structure of the `Oracles`, we find that they implement two methods:
 
* `incoming`, which takes as its parameters the `OpenlawVm` and an event. The event may be a custom type (for example, `OpenlawSignatureOracle` has a `SignatureEvent`), but it must implement the `OpenlawVmEvent` parent class. The `incoming` method returns a `Either[String, OpenlawVm]` and is the place where cryptographic validation of the event occurs. Cryptographic proof is part of the process for validating the event in each oracle as it enters the execution environment. The nature of this proof differs depending on the oracle in question. For example, for the `TemplateLoadOracle`, it needs to be proven that the template in question is part of the contract. 
                                                                                                                                                                                                                                                                                                                                                                          For the `OpenlawSignatureOracle`, the identity of the signing party is what needs to be checked. 
* `shouldExecute`, which takes as its parameter an `OpenlawVmEvent` and returns a `Boolean`. It will return true if `OpenlawVmEvent` is a superclass of the custom `Event` in the `Oracle`. 

In some (`OpenlawSignatureOracle` and `StopContractOracle`) but not all (`LoadTemplateOracle` or `IdentityUpdateOracle`) oracles, a third method is also found:

* `processEvent`, which takes the `OpenlawVM`, the `Event`, and other parameters and returns an `Either(String, OpenlawVM)`. This method is used because there is a desire to separate the cryptographic proof in `incoming` from the event which should occur if the proof is successful. `incoming` needs to occur immediately; however, `processEvent` returns a `Future` of some kind, so it needs to be separated because the result is not yet known until the `Future` is completed. 

Regardless of whether it occurs in `processEvent` or `incoming`, the last action for the `Oracle` is typically to run an `OpenlawVmCommand`. The case class for the specific command must implement the `OpenlawVmCommand` type. The `apply` method in `OpenlawVm` takes an `OpenlawVmCommand` as parameter and matches actions based on the name of the command. The action must return either an `Either[String, OpenlawVm]` or the `OpenlawVm`: the VM has to be part of the result because the command changes the VM state as a result of the execution. The point of the commands is to separate the verification which occured in the cryptographic proof from the execution. Once the verification is complete, it is acceptable for the command to run and do its job.

Any changes to the OpenLaw VM state are stored in an additional case class in the `OpenlawVm`, `OpenlawVmState`. Most of the parameters of this class are the objects in the VM whose state can be altered by an `OpenlawVmCommand`, such as the templates, the execution state, and the template parameters. The final parameter is `events`: a list of type `OpenlawVmEvent` which stores all events which have occurred in the `OpenlawVm` for the particular contract.




