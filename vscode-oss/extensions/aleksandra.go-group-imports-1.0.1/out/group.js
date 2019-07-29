"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const utils_1 = require("./utils");
exports.goGroupImports = () => {
    const { activeTextEditor: editor, activeTextEditor: { document } } = vscode_1.window;
    const documentText = document.getText();
    if (document.languageId !== 'go')
        return;
    const rootPkg = utils_1.resolveRootPackage();
    if (rootPkg === '')
        return;
    const imports = utils_1.getImports(documentText);
    if (!imports.length)
        return;
    const groupedList = exports.group(imports, rootPkg);
    const importsRange = utils_1.getImportsRange(documentText);
    editor.edit(edit => {
        edit.replace(new vscode_1.Range(importsRange.start, 0, importsRange.end - 1, Number.MAX_VALUE), importGroupsToString(groupedList));
    });
    document.save();
};
const isStdlibImport = (imp) => {
    return !imp.includes('.');
};
const isOwnImport = (imp, root) => {
    return imp.includes(root);
};
exports.group = (imports, rootPkg) => {
    const importGroups = {
        stdlib: [],
        thirdParty: [],
        own: [],
    };
    imports.forEach((imp) => {
        if (isOwnImport(imp, rootPkg)) {
            importGroups.own.push(imp);
        }
        else if (isStdlibImport(imp)) {
            importGroups.stdlib.push(imp);
        }
        else {
            importGroups.thirdParty.push(imp);
        }
    });
    return importGroups;
};
const importGroupsToString = (importGroups) => Object.keys(importGroups)
    .filter(key => importGroups[key].length)
    .map(key => importGroups[key].join('\n'))
    .join('\n\n');
//# sourceMappingURL=group.js.map