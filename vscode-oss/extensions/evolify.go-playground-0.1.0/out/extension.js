'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require("vscode");
const axios_1 = require("axios");
const { window, workspace, Disposable } = vscode;
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    let textSaveHandler = new TextSaveHandler();
    context.subscriptions.push(textSaveHandler);
}
exports.activate = activate;
class TextSaveHandler {
    constructor() {
        let subscriptions = [];
        workspace.onDidSaveTextDocument(this.onTextSave, this, subscriptions);
        this.disposable = Disposable.from(...subscriptions);
        this.logger = window.createOutputChannel('output');
    }
    onTextSave() {
        let code = window.activeTextEditor.document.getText();
        axios_1.default.post('https://play.golang.org/compile', { version: 2, body: code })
            .then(res => res.data)
            .then(({ Errors, Events }) => {
            if (!this.logger) {
                this.logger = window.createOutputChannel('output');
            }
            this.logger.clear();
            if (Errors) {
                this.logger.append(Errors + '');
            }
            else {
                this.logger.append(Events[0].Message);
            }
            this.logger.show();
            window.activeTextEditor.show();
        });
    }
    dispose() {
        this.disposable.dispose();
        this.logger.dispose();
    }
}
// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map