# Contributing to OpenLaw

:+1::tada: First off, thanks for being interested in contributing to OpenLaw! :tada::+1:

The following is a set of guidelines for contributing to OpenLaw and its packages, which are hosted in the [OpenLaw Organization](https://github.com/openlawteam) on GitHub.

#### Table Of Contents

[Code of Conduct](#code-of-conduct)

[Questions](#questions)

[How Can I Contribute?](#how-can-i-contribute)
  * [Reporting Bugs](#reporting-bugs)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Pull Requests](#pull-requests)

[Styleguides](#styleguides)
  * [Git Commit Messages](#git-commit-messages)
  * [JavaScript Styleguide](#javascript-styleguide)
  * [CoffeeScript Styleguide](#coffeescript-styleguide)
  * [Specs Styleguide](#specs-styleguide)
  * [Documentation Styleguide](#documentation-styleguide)

[Additional Notes](#additional-notes)
  * [Issue and Pull Request Labels](#issue-and-pull-request-labels)

## Code of Conduct

This project and everyone participating in it is governed by the [OpenLaw Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to [hello@openlaw.io](mailto:hello@openlaw.io).

## Questions

> **Note:** If you have a question, please don't file an issue. You'll get faster results by using the resource below.

We have a detailed FAQ on our website.

* [OpenLaw FAQ](https://openlaw.io/faq)

We also have an active OpenLaw Community Slack:

* [Join the Openlaw Community Slack Team](https://join.slack.com/t/openlaw-community/shared_invite/enQtMzY1MTA2ODY3ODg5LTc0ZGQ4OTEwMDEyMGUxMzJmMDVmNzM1ODRmNTdkNDIyNDkyOGU0NmRkMmRlMmY3ZTMwYzNlOTFiMzUwZjJkOTk)
    * Even though Slack is a chat service, sometimes it takes several hours for OpenLaw team members to respond &mdash; however, we'll get to your query as soon as we can! Our team is distributed across timezones and wants to help you.
    * Use the `#general` channel for general questions or discussion about OpenLaw
    * Use the `#bugs` channel for reporting bugs with the OpenLaw application (as distinct from bugs you encounter in running one of our core libraries - for those, you should open an issue as discussed [below](#reporting-bugs).

## How Can I Contribute?

### Reporting Bugs

This section guides you through submitting a bug report for OpenLaw Core. Following these guidelines helps maintainers and the community understand your report, reproduce the behavior, and find related reports.

Before creating bug reports, please check [this list](#before-submitting-a-bug-report) as you might find out that you don't need to create one. When you are creating a bug report, please [include as many details as possible](#how-do-i-submit-a-good-bug-report).

> **Note:** If you find a **Closed** issue that seems like it is the same thing that you're experiencing, open a new issue and include a link to the original issue in the body of your new one.

#### Before Submitting A Bug Report

* **Reach out to our team on Slack in the `#bugs` channel.
* **Perform a [cursory search](https://github.com/search?q=+is%3Aissue+user%3Aopenlaw-core)** to see if the problem has already been reported. If it has **and the issue is still open**, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Bug Report?

Bugs are tracked as [GitHub issues](https://guides.github.com/features/issues/).

When creating an issue, explain the problem and include additional details to help maintainers reproduce the problem:

* **Use a clear and descriptive title** for the issue to identify the problem.
* **Specify if your issue relates to the Scala or JavaScript library** with a `scala` or `javascript` tag.
* **Describe the exact steps which reproduce the problem** in as many details as possible. For example, start by explaining what you are trying to do with OpenLaw Core. When listing steps, **don't just say what you did, but explain how you did it**.
* **Provide specific examples to demonstrate the steps**. Include links to files or GitHub projects, or copy/pasteable snippets, which you use in those examples. If you're providing snippets in the issue, use [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the behavior you observed after following the steps** and point out what exactly is the problem with that behavior.
* **Explain which behavior you expected to see instead and why.**
* **Include screenshots and videos** which show you following the described steps and clearly demonstrate the problem. We recommend [Loom](useloom.com).
* **If the problem wasn't triggered by a specific action**, describe what you were doing before the problem happened and share more information using the guidelines below.

Provide more context by answering these questions:

* **Did the problem start happening recently** (e.g. after updating to a new version of `openlaw-core`) or was this always a problem?
* If the problem started happening recently, **can you reproduce the problem in an older version of openlaw-core?** What's the most recent version in which the problem doesn't happen?
* **Can you reliably reproduce the issue?** If not, provide details about how often the problem happens and under which conditions it normally happens.

Include details about your configuration and environment:

* **Which version of the OpenLaw Core library are you using?**
* **What's the name and version of the OS you're using**?
* **Are you running openlaw-core in a virtual machine or Docker container?** Please provide more information about your environment in that case.

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for OpenLaw Core, including completely new features and minor improvements to existing functionality. Following these guidelines helps maintainers and the community understand your suggestion and find related suggestions.

Before creating enhancement suggestions, please check [this list](#before-submitting-an-enhancement-suggestion) as you might find out that you don't need to create one. When you are creating an enhancement suggestion, please [include as many details as possible](#how-do-i-submit-a-good-enhancement-suggestion).

#### Before Submitting An Enhancement Suggestion

* **Perform a [cursory search](https://github.com/search?q=+is%3Aissue+user%3Aopenlaw-core)** to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://guides.github.com/features/issues/). Provide the following information in your issue:

* **Use a clear and descriptive title** for the issue to identify the suggestion.
* **Specify if your issue relates to the Scala or JavaScript library** with a `scala` or `javascript` tag.
* **Provide a step-by-step description of the suggested enhancement** in as many details as possible.
* **Provide specific examples to demonstrate the steps**. Include copy/pasteable snippets which you use in those examples, as [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the current behavior** and **explain which behavior you expected to see instead** and why.
* * **Include screenshots and videos** which show you following the described steps and clearly demonstrate the problem. We recommend [Loom](useloom.com).
* **Explain why this enhancement would be useful** to users of OpenLaw.
