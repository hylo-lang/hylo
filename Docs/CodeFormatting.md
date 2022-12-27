# Code Formatting

## Overview
The Swift source code in this repository is meant to be checked with `swift-format`, based on a custom style.
Contributors are asked to format all the code they submit to the repo.
There is a CI job that checks the formatting; this job will not auto-correct the formatting, it will just ensure that the formatting is correct.

At this point we use release 5.7 of `swift-format`.

## Manually Running `swift-format`

To check the style for the Swift code in the repository, run from root directory:
```bash
Tools/run-swift-format.sh lint
```

To automatically format the code, please run
```bash
Tools/run-swift-format.sh fix
```

Please note that this may not bring all the code to be properly formatted; some code needs to be formatted by hand.

This tool requires for `swift-format` to be in PATH, or to be found at `/usr/local/bin/swift-format`.

## Obtaining `swift-format`
The best way to get `swift-format` is to obtain it from the [official repository](https://github.com/apple/swift-format) and compile it by hand.

## Configuration in IDE

### VisualStudio Code

There are multiple ways of setting this up.
One simple way is to install [`apple-swift-format`](https://marketplace.visualstudio.com/items?itemName=vknabel.vscode-apple-swift-format) extension.
Once the extension is installed and configured, one can run `Format Document` or `Format Selection` commands to format the code.
The default keyboard shortcut for running the formatter for the open document is `Alt+Shift+F`.

### XCode

XCode doesn't provide a good way to install custom code formatters.
Use the following respources to use `swift-format` in different ways with XCode:
* [How to use Apple's swift-format with Xcode?](https://stackoverflow.com/questions/64304613/how-to-use-apples-swift-format-with-xcode)
* [How to start working with swift-format](https://exyte.com/blog/how-to-start-working-with-swift-format)

Here is one of the methods to add `swift-format` run each time the project is built:
* Go to `Product` → `Scheme` → `Edit Scheme...` in the XCode menu
* Under `Build`, select `Post-build actions`
* Click on `+` button, then select `New Run Script Action`
* In the box corresponding to the script, paste the following:
```bash
${WORKSPACE_PATH}/../../../Tools/run-swift-format.sh fix
```