# Contributing to Hylo

So you want to contribute to Hylo?
Thanks, that's amazing!
Let's get you started.

There are several ways you may contribute to the Hylo project (see below) and we greatly appreciate all of them.
We'd like to ask every contributor to read and follow our [Code of Conduct](CODE_OF_CONDUCT.md) and expect every member of our community to be welcoming, helpful, and respectful.

Please file an issue if you have any questions.

## Suggesting feature requests

Feature ideas from the community should be pitched [here](https://github.com/val-lang/val-lang.github.io/discussions/categories/feature-pitches) before being formally proposed as a pull request.

First, please search existing pitches to make sure your feature has not already been requested.
You may then create a new discussion to present your idea.
At this time, we only request that you provide a description of the requested feature and motivate it with a small example.

If the pitch gathers momentum, the new feature should be formally proposed as a pull request on that repository, tagged with the label `enhancement`.

## Reporting a bug

First, please search existing issues on this repository to make sure your bug has not already been reported.

If it hasn't, you can create a new issue with the label `bug` with the following details:
- a description of what you're trying to achieve;
- a description of the bug that you encountered;
- the steps to reproduce it (typically in the form of a program);
- the expected result;
- the actual result;
- the version of your operating system; and
- the commit hash and public repository of the code you are using.

We greatly appreciate it if you can isolate the problem and provide a minimal reproducer.

## Contributing code

Please get familiar with our [project conventions](CONVENTIONS.md). See
[CompilerArchitecture.md] for an overview on the design of the compiler.
We use the standard GitHub workflow to merge code contributions:

1. Fork this repository.
2. Create a new branch on your own repository that is named after your contribution.
3. Commit your contribution to that branch.
4. Push your changes to your repository.
5. Create a pull request from GitHub.

Keep in mind that your fork will not automatically stay up to date with this repository.
You will need to manually [synchronize it](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork).

Make sure your changes do not break anything by running all existing tests.
We also ask that you write tests for the code you want to contribute.

Ensure your changes do not introduce any spelling errors. We use [typos-action]
to ensure no mistakes creep in. To run locally, [install typos] and run `typos`
to run a check and `typos -w` to automatically apply suggestions. If you run
into any false positives see the [typos false positives documentation].

Before issuing a pull request we ask that you squash all your commits into a single logical commit that describes your contribution.
We may ask you to commit additional changes while your pull request is under review.
Once everything looks good, squash those additional commits before merging.

Do not hesitate to reach out if you are lost in the process.


## Join the community
You can also get in touch with the community by joining our [Slack][join-slack] or one of our [Zoom Meetings][join-zoom]
(ID: 633 2146 3694, Passcode: 409180) on Tuesdays and Thursdays, 13:00-13:30 Pacific time or 22:00-22:30 Central
European time.

[join-slack]: https://join.slack.com/t/val-qs97696/shared_invite/zt-1z3dsblrq-y4qXfEE6wr6uMEJSN9uFyg
[join-zoom]: https://unige.zoom.us/j/66489601602?pwd=z8jMbhAcbxeZBy9js6YTKOIyfSeRrd.1

[typos-action]: https://github.com/marketplace/actions/typos-action
[install typos]: https://github.com/crate-ci/typos#install
[typos false positives documentation]: https://github.com/crate-ci/typos#false-positives
[CompilerArchitecture.md]: Docs/Compiler/CompilerArchitecture.md
