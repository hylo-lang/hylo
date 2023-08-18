# Code Formatting

## Overview
The Swift source code in this repository is meant to be checked with `swift-format`, based on a custom style.
Contributors are asked to format all the code they submit to the repo.
There is a CI job that checks the formatting; this job will not auto-correct the formatting, it will just ensure that the formatting is correct.

We use the version `swift-format` that is forked in our group: https://github.com/val-lang/swift-format.


## Manually Running `swift-format`

To check the style for the Swift code in the repository, run from root directory:
```bash
Tools/run-swift-format.sh lint
```

(Add all targets for which you want to check formatting)

To automatically format the code, please run
```bash
Tools/run-swift-format.sh fix
```

Please note that this may not bring all the code to be properly formatted; some code needs to be formatted by hand.

To attempt an automatic fix, then to check the formatting, run from root directory:
```bash
Tools/run-swift-format.sh fix_lint
```

## Configuration in IDE

### VisualStudio Code

There are multiple ways of setting this up.
One simple way is to install [`apple-swift-format`](https://marketplace.visualstudio.com/items?itemName=vknabel.vscode-apple-swift-format) extension.
Once the extension is installed and configured, one can run `Format Document` or `Format Selection` commands to format the code.
The default keyboard shortcut for running the formatter for the open document is `Alt+Shift+F`.

### XCode

The `lint-source-code` and `format-source-code` commands can be directly applied from XCode.
Right-click on the `hc` package in the "Project navigator" tab, and select "Lint Source Code" or "Format Source Code".
Ensure to add `--configuration .swift-format.json` inside "Arguments" expansion area, select the targets to run on, and then hit "Run".
