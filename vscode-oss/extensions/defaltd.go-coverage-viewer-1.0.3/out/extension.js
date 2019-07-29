"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const fs = require("fs");
const os = require("os");
const path = require("path");
const goMode_1 = require("./goMode");
const goStatus_1 = require("./goStatus");
const goTest_1 = require("./goTest");
function activate(context) {
    let disposable = vscode.commands.registerCommand('extension.generateCoverage', () => __awaiter(this, void 0, void 0, function* () {
        try {
            let fileUri;
            goStatus_1.outputChannel.clear();
            if (vscode.window.activeTextEditor) {
                fileUri = vscode.window.activeTextEditor.document.uri;
                if (!vscode.languages.match(goMode_1.GO_MODE, vscode.window.activeTextEditor.document)) {
                    vscode.window.showWarningMessage('The current filetype is not supported');
                    return;
                }
                const contents = fs.readFileSync(fileUri.fsPath, { encoding: 'utf8' });
                let packageLine = contents.split('\n').shift();
                let packageName = packageLine ? packageLine.split(' ').pop() : null;
                if (packageName === 'main') {
                    vscode.window.showErrorMessage('Cannot generate coverage results for main package');
                }
                const cwd = path.dirname(fileUri.fsPath);
                const tmpPath = path.normalize(path.join(os.tmpdir(), 'go-coverage'));
                goStatus_1.outputChannel.appendLine(`Generating coverage results for package ${packageName}...`);
                if (!fs.existsSync(`${tmpPath}`)) {
                    fs.mkdirSync(`${tmpPath}`);
                }
                else {
                    if (fs.existsSync(`${tmpPath}/coverage.html`)) {
                        fs.unlinkSync(`${tmpPath}/coverage.html`);
                    }
                    if (fs.existsSync(`${tmpPath}/c.out`)) {
                        fs.unlinkSync(`${tmpPath}/c.out`);
                    }
                }
                let testFailed = yield goTest_1.createTestCoverage('c.out', tmpPath, cwd);
                if (testFailed) {
                    goStatus_1.outputChannel.appendLine(testFailed.message.toString());
                    vscode.window.showErrorMessage(`Failed to generate test coverage for package named ${packageName}`);
                    goStatus_1.outputChannel.show();
                    return;
                }
                let coverageFailed = yield goTest_1.generateHtmlCoverage(tmpPath, cwd);
                if (coverageFailed) {
                    goStatus_1.outputChannel.appendLine(coverageFailed.message.toString());
                    vscode.window.showErrorMessage(`Failed to covert test coverage to HTML for package named ${packageName}`);
                    goStatus_1.outputChannel.show();
                    return;
                }
                goStatus_1.outputChannel.appendLine(`Displaying Package Coverage from ${tmpPath}/coverage.html`);
                const coverageHTML = fs.readFileSync(`${tmpPath}/coverage.html`, { encoding: 'utf8' });
                const viewPanel = vscode.window.createWebviewPanel('goCoverage', `Package [${packageName}]: Coverage Results`, vscode.ViewColumn.Two, { enableScripts: true });
                viewPanel.webview.html = coverageHTML;
            }
            else {
                vscode.window.showErrorMessage('No active editor was detected');
            }
        }
        catch (error) {
            goStatus_1.outputChannel.appendLine(error);
            goStatus_1.outputChannel.show();
        }
    }));
    context.subscriptions.push(disposable);
}
exports.activate = activate;
function deactivate() { }
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map