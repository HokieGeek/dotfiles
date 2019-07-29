"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const groupOnSave_1 = require("./groupOnSave");
let saveRegistration;
const unregisterWillSaveTextDocument = () => {
    if (!saveRegistration) {
        return;
    }
    saveRegistration.dispose();
    saveRegistration = null;
};
const registerWillSaveTextDocument = () => {
    if (saveRegistration) {
        return;
    }
    saveRegistration = vscode_1.workspace.onWillSaveTextDocument(groupOnSave_1.groupImportsOnSave);
};
exports.getOnSaveSetting = () => {
    return vscode_1.workspace.getConfiguration('groupImports').get('onSave');
};
exports.updateSaveRegistration = () => exports.getOnSaveSetting() ? registerWillSaveTextDocument() : unregisterWillSaveTextDocument();
//# sourceMappingURL=register.js.map