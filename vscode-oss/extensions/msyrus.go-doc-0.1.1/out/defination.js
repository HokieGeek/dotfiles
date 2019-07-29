'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const cp = require("child_process");
const path = require("path");
const util_1 = require("./util");
const outputChannel_1 = require("./outputChannel");
function printDefination() {
    let editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showInformationMessage('No editor is active');
        return;
    }
    if (editor.document.languageId !== 'go') {
        vscode.window.showInformationMessage('File in the active editor is not a Go file');
        return;
    }
    outputChannel_1.showOutput('Generating documentation...', true);
    getDefinition(editor.document, editor.selection.active).then(def => {
        if (!def) {
            return;
        }
        outputChannel_1.showOutput(def, true, true);
    }, (err) => {
        outputChannel_1.showOutput(err, true);
    });
}
exports.printDefination = printDefination;
function getDefinition(doc, pos) {
    let gogetdoc = util_1.getBinPath('gogetdoc');
    if (!path.isAbsolute(gogetdoc)) {
        return Promise.reject('gogetdoc is missing');
    }
    let offset = Buffer.byteLength(doc.getText().substr(0, doc.offsetAt(pos)));
    return new Promise((resolve, reject) => {
        let opts = {
            cwd: path.dirname(doc.fileName),
        };
        let p = cp.execFile(gogetdoc, ['-json', '-modified', '-u', '-pos', doc.fileName + ':#' + offset.toString()], opts, (err, stdout, stderr) => {
            try {
                if (err && err.code === 'ENOENT') {
                    return reject('gogetdoc is missing');
                }
                if (err) {
                    return reject(stderr);
                }
                let def = JSON.parse(stdout);
                let out = '';
                if (def.import) {
                    out = def.import + '\n';
                }
                if (def.decl) {
                    out += (out ? '\n' : '') + def.decl + '\n';
                }
                if (def.doc) {
                    out += (out ? '\n' : '') + def.doc;
                }
                return resolve(out);
            }
            catch (e) {
                return reject(e);
            }
        });
        let arch = util_1.getFileArchive(doc);
        p.stdin.end(arch);
    });
}
//# sourceMappingURL=defination.js.map