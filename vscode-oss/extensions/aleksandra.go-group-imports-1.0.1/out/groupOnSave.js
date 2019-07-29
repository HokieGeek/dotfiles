"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const group_1 = require("./group");
exports.groupImportsOnSave = () => {
    if (!vscode_1.window.activeTextEditor.document.languageId.includes('go')) {
        return;
    }
    return group_1.goGroupImports();
};
//# sourceMappingURL=groupOnSave.js.map