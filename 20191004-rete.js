function isVar(str) {
    return str.charAt(0) == '?';
}

function duplicates(xs, ys) {
    let d = [];
    for (let x of xs) {
        let flgExistSame = false;
        for (let y of ys) {
            if (x == y) flgExistSame = flgExistSame || true;
        }
        if (flgExistSame) d.push(x);
    }
    return d;
}

// Construct RETE net
// filter
function isSameAssertion(a, f) {
    if (a.length != f.length) return false;
    let flgSame = true;
    for (let i = 0; i < a.length; i++) {
        flgSame = flgSame && (f[i] == a[i]);
    }
    return flgSame;
}
function addFilter(af, fs) {
    for (let f of fs) {
        if (isSameAssertion(af, fs)) return f;
    }

    let newF = [];
    for (let t of af) newF.push(t);
    fs.push(newF);
    newF["listeners"] = [];
    return newF;
}
// alpha node
function makeAlphaNode(f, name) {
    let vars = [];
    for (let t of f) {
        if (isVar(t)) {
            if (vars.indexOf(t) < 0) vars.push(t);
        }
    }
    return { "name": name, "attrs": vars, "tupples": [], "listeners": [], "newComming": [], "isUpdated": false };
}
// beta node from two alpha nodes
function makeBetaNode(a1, a2) {
    let attrs = [];
    for (let t of a1["attrs"]) {
        attrs.push(t);
    }
    for (let t of a2["attrs"]) {
        if (attrs.indexOf(t) < 0) attrs.push(t);
    }
    let dattrs = duplicates(a1["attrs"], a2["attrs"]);
    return { "name": "(" + a1["name"] + "-" + a2["name"] + ")", "attrs": attrs, "dattrs": dattrs, "tupples": [], "newComming": [], "listeners": [], "isUpdated": false };
}
function clearNode(n) {
    n["tupples"] = [];
    n["newComming"] = [];
    n["isUpdated"] = false;
}

function constructRETE(rb) {
    let filters = [];
    let alphaNodes = [];
    let betaNodes = [];

    for (let r of rb["rules"]) {

        let localAlpha = [];

        for (let i = 0; i < r["if"].length; i++) {
            //for (let a of r["if"]) {
            let a = r["if"][i];
            let f = addFilter(a, filters);
            let alpha = makeAlphaNode(f, r["name"] + i);
            f["listeners"].push(alpha);
            localAlpha.push(alpha);
            alphaNodes.push(alpha)
        }

        let localBeta = [];

        let b = makeBetaNode(localAlpha[0], localAlpha[0]);
        b["left"] = localAlpha[0];
        b["right"] = localAlpha[0];
        localAlpha[0]["listeners"].push(b);
        localBeta.push(b);
        betaNodes.push(b);

        for (let i = 1; i < localAlpha.length; i++) {
            let b = localBeta[localBeta.length - 1];
            let newB = makeBetaNode(b, localAlpha[i]);
            localBeta.push(newB);
            betaNodes.push(newB);
            localAlpha[i]["listeners"].push(newB);
            b["listeners"].push(newB);

            newB["left"] = b;
            newB["right"] = localAlpha[i];
        }
        localBeta[localBeta.length - 1]["triggerRule"] = r;
    }
    return { "filters": filters, "alphaNodes": alphaNodes, "betaNodes": betaNodes };
}

// Working RETE
// a : one assertion, not assertions
function throwAssertionToFilter(a, f, added) {
    if (a.length != f.length) return false;
    let bindings = {};
    for (let i = 0; i < a.length; i++) {
        if (isVar(f[i])) {
            if (bindings[f[i]]) {
                if (bindings[f[i]] != a[i]) return false;
            }
            else {
                bindings[f[i]] = a[i];
            }
        }
        else {
            if (f[i] != a[i]) return false;
        }
    }

    if (Object.keys(bindings).length <= 0) bindings["NO-VAR"] = "NO-VAR";

    for (let alpha of f["listeners"]) {
        throwTuppleToAlpha(bindings, alpha, added);
    }
}

// b: one binding. b = {varName1: val, varName2:val2, ...}.
function throwTuppleToAlpha(b, a, added) {

    if (existsSameTupple(b, a["tupples"])) return false;

    a["tupples"].push(b);
    a["newComming"] = [b];

    a["isUpdated"] = true;

    for (let beta of a["listeners"]) {
        throwToBeta(beta, added);
    }
    a["isUpdated"] = false;
}

// add binding to table
// exist same tupple in Binding DB
function existsSameTupple(newT, tupples) {
    for (let t of tupples) {
        let flgSame = true;
        for (let k of Object.keys(newT)) {
            if (!t[k]) {
                flgSame = false;
            }
            else {
                flgSame = flgSame && (t[k] == newT[k]);
            }
        }
        if (flgSame) return true;
    }
    return false;
}
function addTuppleUnlessSame(newT, tupples) {
    if (existsSameTupple(newT, tupples)) return false;
    tupples.push(newT);
    return true;
}

function throwToBeta(b, added) {
    let l; // updated alpha
    let r; // another alpha

    if (b["left"]["isUpdated"]) { l = b["left"]; r = b["right"]; }
    else { l = b["right"]; r = b["left"]; }

    if (!(l && r)) return false;

    let newComming = []; // tuples added

    for (let lt of l["newComming"]) {  // for each tupple
        for (let rt of r["tupples"]) {  // for each tupple
            let flgSame = true;
            for (let da of b["dattrs"]) { // for each term
                flgSame = flgSame && (lt[da] == rt[da]);
            }
            if (flgSame) {
                let tupple = {};
                for (let la of l["attrs"]) {
                    tupple[la] = lt[la];
                }
                for (let ra of r["attrs"]) {
                    tupple[ra] = rt[ra];
                }
                let flgAdded = addTuppleUnlessSame(tupple, b["tupples"]);
                if (flgAdded) {
                    newComming.push(tupple);
                    b["isUpdated"] = true;
                }
            }
        }
    }


    b["newComming"] = newComming;

    if (b["isUpdated"]) {
        if (b["triggerRule"]) {
            let acts = makeActions(b["tupples"], b["triggerRule"]);
            added.push(acts);
        }
        else {
            for (let newB of b["listeners"]) {
                throwToBeta(newB, added);
            }
        }
    }
    b["isUpdated"] = false;
}

function isBuiltIn(a) {
    if (a.length == 3) {
        return a[1] == '>';
    }
    return false;
}
function getIntVal(binding, str) {
    if (isVar(str)) {
        return parseInt(binding[str]);
    }
    return parseInt(str);
}
function checkBuildIn(binding, builtIn) {
    if ((builtIn.length == 3) && (builtIn[1] == '>')) {
        let l = getIntVal(builtIn[0]);
        let r = getIntVal(builtIn[2]);
        if (isNaN(l) || isNaN(r)) return false;
        return l > r;
    }
    return false;
}

function makeActions(bindings, rule) {
    let as = [];
    for (let b of bindings) {
        for (let a of rule["then"]) {
            let newA = [];
            for (let t of a) {
                let newT;
                if (isVar(t)) {
                    if (b[t]) newT = b[t];
                    else newT = "*****";
                }
                else {
                    newT = t;
                }
                newA.push(newT);
            }
            as.push(newA);
        }
    }
    return { "actions": as, "rule": rule };
}

// check whether an assertion in working memory.
function existsSameAssertion(newA, assertions) {
    for (let a of assertions) {
        if (a.length != newA.length) continue;

        let flgSame = true;
        for (let i = 0; i < a.length; i++) {
            flgSame = flgSame && (a[i] == newA[i]);
        }
        if (flgSame) return true;
    }
    return false;
}
function addOneAssertionUnlessSame(a, assertions) {
    if (existsSameAssertion(a, assertions)) return false;
    assertions.push(a);
    return true;
}
/*
function addAssertionsUnlessSame(newAs, assertions) {
    let flgAdded = false;
    for (let a of newAs) {
        let flg = addOneAssertionUnlessSame(a, assertions);
        flgAdded = flgAdded || flg; // ↑の手続きをショートカットされないように
    }
    return flgAdded;
}
*/
function deleteAssertion(delA, assertions) {
    for (let i = assertions.length - 1; i >= 0; i--) {
        if (isSameAssertion(delA, assertions[i])) {
            assertions.splice(i, 1);
            return true;
        }
    }
    return false;
}
function doActs(acts, assertions) {
    for (let action of acts) {
        if (action.length == 1) {
            addOneAssertionUnlessSame(action, assertions);
            continue;
        }
        if (action[0] == 'delete') {
            deleteAssertion(action.slice(1), assertions);
            continue;
        }
        if (action[0] == 'add') {
            addOneAssertionUnlessSame(action.slice(1), assertions);
            continue;
        }
        addOneAssertionUnlessSame(action, assertions);
    }
}

function runRETE(rb, wm, rete) {
    while (true) {
        let beforeWMSize = wm["wm"].length;
        let triggeredRules = [];

        for (let a of wm["wm"]) {
            for (let f of rete["filters"]) {
                throwAssertionToFilter(a, f, triggeredRules);
            }
        }

        if (triggeredRules.length > 0) {
            console.log("Fired rules are:")
            for (let rule of triggeredRules) {
                console.log("   " + rule["rule"]["name"]);
                doActs(rule["actions"], wm["wm"]);
            }
        }
        else {
            console.log("There is no triggered rule.");
            break;
        }

        if (existsSameAssertion(["DONE"], wm["wm"])) break;
    }
}

function runRETEReactionMode(rb, wm, rete) {
    while (true) {
        let beforeWMSize = wm["wm"].length;
        let triggeredRules = [];

        for (let a of wm["wm"]) {
            for (let f of rete["filters"]) {
                throwAssertionToFilter(a, f, triggeredRules);
            }
        }

        if (triggeredRules.length > 0) {
            console.log("Triggered rules are:")
            for (let rule of triggeredRules) {
                console.log("   " + rule["rule"]["name"]);
            }
            // solve conflict
            let firedRule = triggeredRules[triggeredRules.length - 1];
            //console.log(firedRule);
            console.log("Fired rule is : " + firedRule["rule"]["name"]);
            doActs(firedRule["actions"], wm["wm"]);
        }
        else {
            console.log("There is no triggered rule.");
            break;
        }

        if (existsSameAssertion(["DONE"], wm["wm"])) break;

        //if (wm["wm"].length <= beforeWMSize) break;

        // for reaction mode
        for (let a of rete["alphaNodes"]) clearNode(a);
        for (let b of rete["betaNodes"]) clearNode(b);
    }
}

// Rule and WM structure
function jsonStr2RB(str) {
    let rb = JSON.parse(str);
    for (let r of rb["rules"]) {
        let ifs = [];
        let builtIns = [];
        for (let a of r["if"]) {
            ifs.push(a.split(' '));
        }
        //for builtIn
        for (let i = ifs.length - 1; i >= 0; i--) {
            if (isBuiltIn(ifs[i])) {
                builtIns.push(ifs[i]);
                ifs.splice(i, 1);
            }
        }
        r["if"] = ifs;
        r["if-built-in"] = builtIns;

        let thens = [];
        for (let a of r["then"]) {
            thens.push(a.split(' '));
        }
        r["then"] = thens;
    }

    return rb;
}

function jsonStr2WM(str) {
    let wm = JSON.parse(str);
    let as = [];
    for (let a of wm["wm"]) {
        as.push(a.split(' '));
    }
    wm["wm"] = as;
    return wm;
}

function strRB(aRB) {
    let str = "name : " + aRB["name"] + "\n";
    str += "doc : " + aRB["doc"] + "\n";
    for (let r of aRB["rules"]) {
        str += r["name"] + "\n";
        str += "if\n";
        for (let a of r["if"]) {
            str += "     " + a + "\n"
        }
        str += "if-built-in\n";
        for (let a of r["if-built-in"]) {
            str += "     " + a + "\n"
        }
        str += "then\n";
        for (let c of r["then"]) {
            str += "     " + c + "\n"
        }
        str += "\n";
    }
    return str;
}

function strWM(aWM) {
    let str = "name : " + aWM["name"] + "\n";
    str += "doc : " + aWM["doc"] + "\n";
    for (let a of aWM["wm"]) {
        str += "  " + a + "\n";
    }
    return str;
}

let fs = require("fs");
let text = fs.readFileSync("./20191009-rb8.json");
let rulebase = jsonStr2RB(text);

text = fs.readFileSync("./20191009-wm8.json");
let workingMemory = jsonStr2WM(text);

console.log(strRB(rulebase));
console.log(strWM(workingMemory));

let rete = constructRETE(rulebase);
//runRETE(rulebase, workingMemory, rete);
runRETEReactionMode(rulebase, workingMemory, rete);
console.log(workingMemory["wm"]);
