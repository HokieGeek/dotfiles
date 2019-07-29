"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
exports.envPath = process.env['PATH'] || (process.platform === 'win32' ? process.env['Path'] : null);
function fileExists(filePath) {
    try {
        return fs.statSync(filePath).isFile();
    }
    catch (e) {
        return false;
    }
}
exports.fileExists = fileExists;
function byteOffsetAt(doc, pos) {
    return Buffer.byteLength(doc.getText().substr(0, doc.offsetAt(pos)));
}
exports.byteOffsetAt = byteOffsetAt;
function getFileArchive(doc) {
    let text = doc.getText();
    return doc.fileName + '\n' + Buffer.byteLength(text, 'utf8') + '\n' + text;
}
exports.getFileArchive = getFileArchive;
function getBinPath(name) {
    if (!name) {
        return "";
    }
    if (binPathCache[name]) {
        return binPathCache[name];
    }
    let binName = correctBinName(name);
    let goRoot = process.env["GOROOT"];
    if (goRoot) {
        let binPath = path.join(goRoot, "bin", binName);
        if (fileExists(binPath)) {
            binPathCache[name] = binPath;
            return binPathCache[name];
        }
    }
    let goPath = process.env["GOPATH"];
    if (goPath) {
        let binPath = path.join(goPath, "bin", binName);
        if (fileExists(binPath)) {
            binPathCache[name] = binPath;
            return binPathCache[name];
        }
    }
    if (exports.envPath) {
        let binPath = exports.envPath.split(path.delimiter)
            .map(dir => path.join(dir, binName))
            .filter(fp => fileExists(fp))[0];
        if (fileExists(binPath)) {
            binPathCache[name] = binPath;
            return binPathCache[name];
        }
    }
    return "";
}
exports.getBinPath = getBinPath;
let binPathCache = {};
let defaultPathForGo = process.platform === 'win32' ? 'C:\\Go\\bin\\go.exe' : '/usr/local/go/bin/go';
function getGoBinPath() {
    if (binPathCache["go"]) {
        return binPathCache["go"];
    }
    let binName = correctBinName("go");
    let goRoot = process.env["GOROOT"];
    if (goRoot) {
        let binPath = path.join(goRoot, "bin", binName);
        if (fileExists(binPath)) {
            binPathCache["go"] = binPath;
            return binPathCache["go"];
        }
    }
    if (exports.envPath) {
        let binPath = exports.envPath.split(path.delimiter)
            .map(dir => path.join(dir, binName))
            .filter(fp => fileExists(fp))[0];
        if (fileExists(binPath)) {
            binPathCache["go"] = binPath;
            return binPathCache["go"];
        }
    }
    if (fileExists(defaultPathForGo)) {
        binPathCache["go"] = defaultPathForGo;
    }
    return binPathCache["go"];
}
exports.getGoBinPath = getGoBinPath;
function correctBinName(name) {
    return name && process.platform === 'win32' ? name + '.exe' : name;
}
//# sourceMappingURL=util.js.map