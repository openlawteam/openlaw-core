# Release Process: openlaw-core

Note this documentation is for core project maintainers and all steps may not apply to open source contributors. If in doubt, just ask! We're friendly folks. :rainbow:

## Submitting Changes :sparkles:

New code changes should always be introduced via a Pull Request branch:

1. Pull latest `master` branch then create new branch off from `master` and make the changes there.

2. If added any new sbt tests, make sure they are all passing.

3. Submit PR for merging that new branch into `master`. GitHub Actions will automatically attempt a build and run tests, which you can see here: https://github.com/openlawteam/openlaw-core/actions.

4. If the build and all checks are passing, the PR can be merged to master.

## Releasing to BinTray :rocket:

The `master` branch represents the current stable set of the code, and should be considered "release-able" at any point in time.

New _releases_ are cut from the `master` branch, and managed via GitHub Releases with [Semantic Versioning](https://semver.org/spec/v2.0.0.html) formatted tags. <!-- As a developer, this process is largely automated for you. -->

To make a release of the current status of master, follow these steps:

### Drafting the release

1. On GitHub, navigate to the main page of the repository. Under your repository name, click **Releases**:

   ![screenshot](https://help.github.com/assets/images/help/releases/release-link.png)

   Then click the **Draft a new release** button:
   ![screenshot](https://help.github.com/assets/images/help/releases/draft_release_button.png)


    You can also click and/or bookmark [this link](https://github.com/openlawteam/openlaw-core/releases/new) to navigate there directly.

2. **Fill out the version tag, target and release title fields**:

   ![release_populated](https://user-images.githubusercontent.com/40650/57199414-9d5fa700-6f4c-11e9-8ee3-2aa34356e8ac.png)

   **Please note that:**

   - The _target_ **must** be `master`.
   - The _version number_ **must** adhere to Semantic Version specification.
   - The _version tag_ **must** begin with a `v` in order to indicate it is a version. Thus the full tag should be of the format `v1.2.3` for version.
   - For _release title_ please also put the identical version tag as seen above. (If left blank GitHub auto-generates very poorly.)

   SBT is configured to use these version tags as the source of truth, so treat them accordingly. :smile: In the future, we'll have a script to help out with pre-populating these values.

   > **How to pick a version:** In most cases, you'll be doing a "patch" increment, which means just incrementing the last digit of the version. If the current version tag is `v0.1.29`, the next patch would be `v0.1.30`. If you think the release requires a larger Semantic Version bump (API changes for example), talk to others on `#dev` in Slack to confirm.</small>

3. When you're ready to publish your release, click **Publish release**. To work on the release later, click **Save draft**.
   ![screenshot](https://help.github.com/assets/images/help/releases/release_buttons.png)

<!--
Steps for open-law client below. Preserving in comments here since hopefully we can have something similar in future.

1. Switch to `master` and pull the latest.

1. Run `npm run release` to start the release process.

1. A prompt will appear in your console allowing you to pick what sort of Semantic Version increment to pick. In most cases, you will be selecting a `patch` release. _(Note: you can skip the interactive prompt for a patch by substituting `npm run release:patch` in the step above.)_

1. You'll see the release steps going through in your console.<sup>§</sup> When that's done the `openlaw-client` repo should open in your web browser with a draft release. All you have to do there is verify the release notes and click publish.
-->

### Publishing the release to BinTray

#### Automatically (coming soon!)

1. Once a GitHub Release is published, our CI system will automatically start the process to publish the package to BinTray. You can [monitor the progress for that process](#TODO_URL) and see when it completes.

2. Confirm that the package was successfully published with the latest version number by [checking the BinTray listing](https://bintray.com/openlawos/openlaw-core/openlaw-core).

#### Manually

<!-- If for some reason, the publish to BinTray must be done manually in your local environment, you can do it via the following steps in an emergency: -->

In the very near future, the following steps will be automated and done in CI, but for _right now_ we're doing them manually while we test:

1. Switch to `master` and pull the latest. You may wish to also run `git fetch` to ensure you have the latest tags.

2. Run `sbt version` and verify the version matches what you expect. If the release you just published was tagged `v1.2.3`, you should see `1.2.3`.

   <small>It's unlikely, but if you see a version like `1.2.3-4-gbd90214` it means the master branch has now advanced past the tagged release in commits in the time since you published the GitHub Release. You shouldn't publish these pre-release versions to BinTray. Ping @outkaj and/or @mroth and we can help resolve (most likely by manually `git checkout` the tag corresponding to what we want to release.).</small>

3. Access the BinTray credentials in our keystore (if you don't have access, ask someone who does).

4. Run `BINTRAY_USER=foo BINTRAY_PASS=bar sbt releaseBoth`. <small>_(NOTE: Please never ever put these credentials in your .bash_profile or similar auto-sourced file! You are giving them away to every single process that ever executes on your workstation.)_</small>
