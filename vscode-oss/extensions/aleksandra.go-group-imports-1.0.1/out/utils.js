"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
exports.multilineImportsGroupRegex = /import \(([^)]+)\)/;
exports.resolveRootPackage = () => {
    const gopath = process.env.GOPATH;
    if (gopath === '') {
        vscode_1.window.showErrorMessage('No gopath configured.');
        return '';
    }
    const pwd = vscode_1.window.activeTextEditor.document.fileName;
    if (!pwd.includes(gopath)) {
        vscode_1.window.showErrorMessage('File is not in gopath.');
        return '';
    }
    const trimmedPwd = pwd.replace(path.join(gopath, 'src/', ''), '');
    const rootPkg = trimmedPwd.split(path.sep).slice(0, 2).join(path.sep);
    return rootPkg;
};
exports.getImports = (documentText) => {
    const importsMatch = documentText.match(exports.multilineImportsGroupRegex);
    if (!importsMatch || importsMatch.length < 2) {
        return [];
    }
    return importsMatch[1]
        .split("\n")
        .filter(line => line.trim() != "");
};
exports.getImportsRange = (documentText) => {
    let start = 1; // lines in vs code are numereted from 1
    for (var line of documentText.split('\n')) {
        if (line.includes('import (')) {
            break;
        }
        start++;
    }
    let end = start;
    for (var line of documentText.split('\n').slice(start)) {
        if (line.includes(')')) {
            break;
        }
        end++;
    }
    return {
        end,
        start,
    };
};
//# sourceMappingURL=utils.js.map