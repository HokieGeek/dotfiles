"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const cp = require("child_process");
const goStatus_1 = require("./goStatus");
exports.createTestCoverage = (fname, tmpPath, cwd) => {
    return new Promise((resolve, reject) => {
        try {
            const output = cp.execSync(`go test -coverprofile=${tmpPath}/${fname}`, { cwd });
            const lines = output.toString().split('\n');
            goStatus_1.outputChannel.appendLine(`${cwd}> finished generating coverage results:`);
            for (const line of lines) {
                goStatus_1.outputChannel.appendLine(line);
            }
            return resolve();
        }
        catch (err) {
            goStatus_1.outputChannel.appendLine('Error while generating coverage output:');
            goStatus_1.outputChannel.appendLine(err.toString());
            reject(err);
        }
    });
};
exports.generateHtmlCoverage = (tmpPath, cwd) => {
    return new Promise((resolve, reject) => {
        try {
            const output = cp.execSync(`go tool cover -html=${tmpPath}/c.out -o ${tmpPath}/coverage.html`, { cwd });
            let lines = output.toString().split('\n');
            goStatus_1.outputChannel.appendLine(`${cwd}> finished converting coverage results to HTML:`);
            for (const line of lines) {
                goStatus_1.outputChannel.appendLine(line);
            }
            resolve();
        }
        catch (err) {
            goStatus_1.outputChannel.appendLine('Error while converting coverage results to HTML:');
            goStatus_1.outputChannel.appendLine(err.toString());
            reject(err);
        }
    });
};
//# sourceMappingURL=goTest.js.map