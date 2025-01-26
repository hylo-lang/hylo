# Code formatting

Contributors are asked to format all the code they submit to the repository using `swift-format`.

We use `swift-format` tagged with the commit [0bc2f03](https://github.com/apple/swift-format/commit/0bc2f0381c72d66a949254af22208a81377cf717).
The best way to get this tool is to obtain it from its [official repository](https://github.com/apple/swift-format) and compile it manually.

**Warning:** please make sure to get the version specified above.
Different versions of `swift-format` may produce different results.

The script `Tools/run-swift-format.sh` can be used to check and format code.
It will search for `swift-format` in your path `PATH` or at `/usr/local/bin/swift-format`.

To check the style for the Swift code in the repository, run the following command from the root directory:

```bash
Tools/run-swift-format.sh lint
```

To automatically format the code, run the following command:

```bash
Tools/run-swift-format.sh fix
```

Please note that `swift-format` might not be able to fix all issues raised by the `lint` command.
Hence, you may have to manually format your code to comply, even after using the `fix` command.

### Configuring your IDE with `swift-format`

#### Visual Studio Code

The simplest way to configure `swift-format` with VS Code is to install the [`apple-swift-format`](https://marketplace.visualstudio.com/items?itemName=vknabel.vscode-apple-swift-format) extension.
Once it's installed and configured, you can run the `Format Document` or `Format Selection` commands to format your code.
The default keyboard shortcut for running the formatter on the current document is `Alt+Shift+F`.

#### XCode

As of this writing, there isn't any XCode extension to integrate `swift-format`.
One workaround is to configure formatting as a post-build action in your project:
- Go to `Product` → `Scheme` → `Edit Scheme...` in the XCode menu
- Under `Build`, select `Post-build actions`
- Click on the `+` button and select `New Run Script Action`
- In the box corresponding to the script, paste the following:

```bash
${WORKSPACE_PATH}/../../../Tools/run-swift-format.sh fix
```
