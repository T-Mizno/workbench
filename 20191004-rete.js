
// Rule and WM structure
function jsonStr2RB(str) {
    let rb = JSON.parse(str);
    for (let r of rb["rules"]) {
        let ifs = [];
        for (let a of r["if"]) {
            ifs.push(a.split(' '));
        }
        r["if"] = ifs;
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
        str += a + "\n";
    }
    return str;
}



function isVar(str) {
    return str.charAt(0) == '?';
}


// Construct RETE net

// filter to alpha node
function makeAlphaNode(f) {
    let vars = [];
    for (let t of f) {
        if (isVar(t)) {
            if (vars.indexOf(t) < 0) vars.push(t);
        }
    }
    return { "attrs": vars, "tupples": [], "listeners": [] };
}

// alpha to beta
function makeBetaNode(a1, a2) {
    let attrs = [];
    for (let t of a1["attrs"]) {
        attrs.push(t);
    }
    for (let t of a2["attrs"]) {
        if (attrs.indexOf(t) < 0) attrs.push(t);
    }
    return { "attrs": attrs, "tupples": [], "listeners": [] };
}

function addFilter(af, fs) {
    for (let f of fs) {
        if (f.length != af.length) continue;
        let flg = true;
        for (let i = 0; i < f.length; i++) {
            flg = flg && (f[i] == af[i]);
        }
        if (flg) return f;
    }
    let newF = [];
    for (let t of af) newF.push(t);
    fs.push(newF);
    newF["listeners"] = [];
    return newF;
}

/*
// a1 may contains variable, a2 must not contain variable.
function matchAssertion(a1, a2, bindings) {
    if (a1.length != a2.length) return false;
    for (let i = 0; i < a1.length; i++) {
        if (!matchTerm(a1[i], a2[i], bindings)) return false;
    }
    return true;
}
*/

function constructRETE(rb) {
    let filters = [];
    //let alphaNodes = [];

    for (let i = 0; i < rb["rules"].length; i++) {
        let r = rb["rules"][i];
        let localAlpha = [];
        for (let a of r["if"]) {
            let f = addFilter(a, filters);
            let alpha = makeAlphaNode(f);
            f["listeners"].push(alpha);
            localAlpha.push(alpha);
            //alphaNodes.push(alpha);
        }

        let localBeta = [];

        let b = makeBetaNode(localAlpha[0], localAlpha[0]);
        b["left"] = localAlpha[0];
        b["right"] = localAlpha[0];
        localAlpha[0]["listeners"].push(b);
        localBeta.push(b);

        for (let i = 1; i < localAlpha.length; i++) {
            let b = localBeta[localBeta.length - 1];
            let newB = makeBetaNode(b, localAlpha[i]);
            localBeta.push(newB);
            localAlpha[i]["listeners"].push(newB);
            b["listeners"].push(newB);

            newB["left"] = b;
            newB["right"] = localAlpha[i];
        }
        localBeta[localBeta.length - 1]["triggerRule"] = r;
    }
    return filters;
}

// Working RETE
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
    for (let a of f["listeners"]) {
        throwBindingsToAlpha(bindings, a, added);
    }
}

function throwBindingsToAlpha(b, a, added) {
    for (let t of a["tupples"]) {
        let flg = true;
        for (let k of Object.keys(b)) {
            if (!t[k]) {
                flg = false;
                break;
            }
            flg = flg && (t[k] == b[k]);
        }
        if (flg) return false;
    }
    a["tupples"].push(b);

    for (let beta of a["listeners"]) {
        throwToBeta(beta, added);
    }
}

function duplicates(xs, ys) {
    let d = [];
    for (let x of xs) {
        let flg = false;
        for (let y of ys) {
            if (x == y) flg = flg || true;
        }
        if (flg) d.push(x);
    }
    return d;
}

// add binding to table
function addTuppleWithoutSame(newT, tupples) {
    for (let t of tupples) {
        let flgSame = true;
        for (let k of Object.keys(newT)) {
            if (!t[k]) return false;
            flgSame = flgSame && (t[k] == newT[k]);
        }
        if (flgSame) return false;
    }
    tupples.push(newT);
    return true;
}
function throwToBeta(b, added) {
    let l = b["left"];
    let r = b["right"];
    if (!(l || r)) return false;

    let beforeSize = b["tupples"].length;

    b["tupples"] = [];

    let dattrs = duplicates(l["attrs"], r["attrs"]);
    for (let lt of l["tupples"]) {
        for (let rt of r["tupples"]) {
            let flgSame = true;
            for (let da of dattrs) {
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
                addTuppleWithoutSame(tupple, b["tupples"]);
            }
        }
    }



    if (b["tupples"].length > beforeSize) {
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
    return as;
}

function addAssertionsWithoutSame(newAs, assertions) {
    for (let a of newAs) {
        for (let a2 of assertions) {
            if (a.length != a2.length) break;

            let flgSame = true;
            for (let i = 0; i < a.length; i++) {
                flgSame = flgSame && (a[i] == a2[i]);
            }
            if (flgSame) break;
        }
        assertions.push(a);
    }
}

function runRETE(rb, wm, filters) {
    while (true) {
        let beforeWMSize = wm["wm"].length;
        let added = [];
        for (let a of wm["wm"]) {
            for (let f of filters) {
                throwAssertionToFilter(a, f, added);
            }
        }

        for (let act of added) {
            addAssertionsWithoutSame(act, wm["wm"]);
        }
        if (wm["wm"].length <= beforeWMSize) break;
    }
}


/*
for (let f of filters) {
    console.log("Filters:");
    console.log(f);
    for (let a of f["listeners"]) {
        console.log("Alphas");
        console.log(a);
        for (let b of a["listeners"]) {
            console.log("Betas");
            console.log(b);
        }
    }

}
*/


let fs = require("fs");
let text = fs.readFileSync("./20191004-rb2.json");
let rulebase = jsonStr2RB(text);

text = fs.readFileSync("./20191004-wm2.json");
let workingMemory = jsonStr2WM(text);

console.log(strRB(rulebase));
console.log(strWM(workingMemory));


let filters = constructRETE(rulebase);
runRETE(rulebase, workingMemory, filters);
console.log(workingMemory["wm"]);