
const BOTTOM = { Botom: 1234567, Botom: {} };
let Counter = 0;
function getGCount() {
    Counter++;
    return Counter;
}

function isBottom(s) {
    return s == BOTTOM;
}
function isVar(s) {
    if (s['Var']) return true;
    else return false;
}
function isAtomic(s) {
    return (typeof (s) == 'string');
}
function isComposition(s) {
    if (isAtomic(s)) return false;
    if (isVar(s)) return false;
    if (isBottom(s)) return false;
    return true;
}
function checkVar(t) {
    if (isVar(t)) {
        if (t['id']) {
            t['id'] += 1;
        }
        else {
            t['id'] = 1;
        }
        return;
    }
    for (var k of Object.keys(t)) {
        checkVar(t[k]);
    }
    return;
}

function varsInTerm(t, vars) {
    if (!isComposition(t)) {
        return vars;
    }
    for (var k of Object.keys(t)) {
        if (isVar(t[k])) {
            let flg = false;
            for (let i = 0; i < vars.length; i++) {
                if (vars[i]['Var'] == t[k]['Var']) {
                    /*
                    if (!vars[i]['forward'] && t[k]['forward']) {
                        vars[i]['forward'] = t[i]['forward'];
                    }
                    */
                    t[k] = vars[i];

                    flg = true;
                    break;
                }
            }
            if (!flg) {
                vars.push(t[k]);
            }
        }
        if (isComposition(t[k])) varsInTerm(t[k], vars);
    }
    return vars;
}
function reduceVars(t) {
    let vs = varsInTerm(t, []);
}

function reduceVarsWithRename(t) {
    let vs = varsInTerm(t, []);
    for (let i = 0; i < vs.length; i++) {
        vs[i]['Var'] = vs[i]['Var'] + getGCount();
    }
}

function deref(dag) {
    if (isAtomic(dag)) return dag;
    if (dag['forward']) {
        return deref(dag['forward']);
    }
    let d = dag;
    if (d['arcBinding']) {
        for (var af of Object.keys(d['arcBinding'])) {
            d[af] = d['arcBinding'][af];
        }
    }
    delete d['arcBinding'];

    for (var f of Object.keys(d)) {
        d[f] = deref(d[f]);
    }
    return d;
}

function intersections(dag1, dag2) {
    let f1 = Object.keys(dag1);
    let f2 = Object.keys(dag2);
    return f1.filter(e => f2.indexOf(e) >= 0);
}
function complements(dag1, dag2) {
    let f1 = Object.keys(dag1);
    let f2 = Object.keys(dag2);
    return f1.filter(e => f2.indexOf(e) < 0);
}
function unify(adag1, adag2) {
    let dag1 = deref(adag1);
    let dag2 = deref(adag2);

    //console.log(dag1);
    //console.log(dag2);

    if (dag1 == dag2) return dag2;
    if (isVar(dag1)) {
        dag1['forward'] = dag2;
        return dag2;
    }
    if (isVar(dag2)) {
        dag2['forward'] = dag1;
        //dag1['forward'] = dag2;
        return dag1;
    }
    if (isAtomic(dag1) && isComposition(dag2)) return BOTTOM;
    if (isComposition(dag1) && isAtomic(dag2)) return BOTTOM;
    if (isAtomic(dag1) && isAtomic(dag2) && (dag1 != dag2)) return BOTTOM;
    if (isAtomic(dag1) && isAtomic(dag2) && (dag1 == dag2)) {
        dag1['forward'] = dag2;
        return dag1;
    }
    let shared = intersections(dag1, dag2);
    let newed = complements(dag1, dag2);
    dag1['forward'] = dag2;

    let isSuccess = true;
    for (var arc of shared) {
        let flg = isBottom(unify(dag1[arc], dag2[arc]));
        isSuccess = isSuccess && (!flg);
    }

    if (isSuccess) {
        let newObj = {};
        for (var arc of newed) {
            newObj[arc] = dag1[arc];
        }
        if (Object.keys(newObj).length > 0) {
            dag2['arcBinding'] = newObj;
        }
        return dag2;
    }

    return BOTTOM;
}

function unifyTest(tl, tr) {
    console.log("#######################");
    reduceVars({ l: tl, r: tr });
    console.log(tl);
    console.log(tr);
    let result = unify(tl, tr);
    console.log("result");
    console.log(result);
    console.log("after tl");
    console.log(tl);
    console.log("after tr");
    console.log(tr);
}

console.log("TEST PAP");
let t3341l = { 1: { Var: "x" }, 2: "+", 3: "1" };
let t3341r = { 1: "2", 2: "+", 3: { Var: "y" } };
unifyTest(t3341l, t3341r);
let t3342l = { 1: { Var: "x" }, 2: { Var: "y" } };
let t3342r = { 1: { Var: "y" }, 2: { Var: "x" } };
unifyTest(t3342l, t3342r);
let t3343l = { 1: { Var: "x" }, 2: { Var: "x" } };
let t3343r = { 1: { Var: "y" }, 2: { Var: "y" } };
unifyTest(t3343l, t3343r);
let t3344l = { 1: { Var: "x" }, 2: { Var: "x" }, 3: { Var: "x" } };
let t3344r = { 1: { Var: "y" }, 2: { Var: "y" }, 3: { Var: "y" } };
unifyTest(t3344l, t3344r);

let t3554l = { 1: { Var: "x" }, 2: { Var: "y" }, 3: "a" };
let t3554r = { 1: { Var: "y" }, 2: { Var: "x" }, 3: { Var: "x" } };
unifyTest(t3554l, t3554r);

let t3361l = { Var: "x" };
let t3361r = { 1: "f", 2: { Var: "x" } };
unifyTest(t3361l, t3361r);

let ttt1 = { t0: 'likes', t1: 'Sandy', t2: { Var: 'who12' } };
let ttt2 = { t0: 'likes', t1: 'Kim', t2: 'Robin' };
unifyTest(ttt1, ttt2);

function a2t(a) {
    console.dir(a, { depth: 5 });
    let t = {};
    for (let i = 0; i < a.length; i++) {
        if (Array.isArray(a[i])) {
            t["t" + i] = a2t(a[i]);
        }
        else if (a[i].startsWith("?")) {
            t["t" + i] = { Var: a[i].slice(1) };
        }
        else {
            t["t" + i] = "" + a[i];
        }
    }
    return t;
}
function a2q(a) {
    let r = [];
    for (let j = 0; j < a.length; j++) {
        r.push(a2t(a[j]));
    }
    return r;
}
function a2rules(a) {
    let rs = [];
    for (let i = 0; i < a.length; i++) {
        let r = [];
        for (let j = 0; j < a[i].length; j++) {
            r.push(a2t(a[i][j]));
        }
        rs.push(r);
    }
    return rs;
}

function deepCopy(t) {
    return JSON.parse(JSON.stringify(t));
}


function prove3(aQs, aCs, rs, results) {
    let count = 0;
    count++;
    console.log("######################## CCCOUNT" + count);
    if (aQs.length < 1) {
        console.log("SUCCESS!!!!!!!");
        if (aCs.length > 0) {
            console.dir(aCs, { depth: 5 });
            results.push(aCs);
        }
        return true;
    }

    let flg = false;
    for (let rule of rs) {
        let qs = deepCopy(aQs);
        let q = qs[0];
        qs.shift();
        let cs = deepCopy(aCs);
        reduceVars({ cs: cs, q: q, qs: qs });
        let r = deepCopy(rule);
        reduceVarsWithRename(r);
        console.log("trying query");
        console.log(q);
        console.log("qs");
        console.log(qs);
        console.log("cs");
        console.log(cs);
        console.log("trying rule");
        console.log(r);
        let u = unify(q, r[0]);
        if (!isBottom(u)) {
            console.log("unify!!!");
            console.log(u);
            flg = true;
            r.shift();
            let tmpQ = r.concat(qs);
            cs.push(u);
            console.log("newQs");
            console.log(tmpQ);

            prove3(tmpQ, cs, rs, results);
        }
    }
    if (!flg) {
        console.log("FAILL!!!!!!!!!!!!!!!!");
        //qss.shift();
    }
}

function prove(qs, rules) {
    let results = [];
    prove3(qs, [], rules, results);
    return results;
}



let rule343 = [
    [["likes", "Kim", "Robin"]],
    [["likes", "Sandy", "Lee"]],
    [["likes", "Sandy", "Kim"]],
    [["likes", "Robin", "cats"]],
    [["likes", "Sandy", "?x"], ["likes", "?x", "cats"]],
    [["likes", "Kim", "?x"], ["likes", "?x", "Lee"], ["likes", "?x", "Kim"]],
    [["likes", "?x", "?x"]]];
let q343 = [["likes", "Sandy", "?who"]];
let q3452 = [["likes", "?who", "Sandy"]];
let q3453 = [["likes", "Robin", "LEE"]];
let q3454 = [["likes", "?x", "?y"], ["likes", "?y", "?x"]];
//let q343 = ["likes", "Sandy", "Robin"];

function a2cons(a) {
    if (a.length < 1) return "NIL";
    return { car: a[0], cdr: a2cons(a.slice(1)) };
}

let rule338 = [
    [{ t0: "member", t1: { Var: "item" }, t2: { car: { Var: "item" }, cdr: { Var: "rest" } } }],
    [{ t0: "member", t1: { Var: "item" }, t2: { car: { Var: "x" }, cdr: { Var: "rest" } } }, { t0: "member", t1: { Var: "item" }, t2: { Var: "rest" } }]
];
//let q3393 = [{ t0: "member", t1: { Var: "x" }, t2: { car: "1", cdr: { car: "2", cdr: { car: "3", cdr: "NIL" } } } }];
let q3393 = [{ t0: "member", t1: { Var: "x" }, t2: a2cons([1, 2, 3, 5, 7]) }];


let rules = rule338;//a2rules(rule338);
let query = q3393;//a2q(q3393);
reduceVars(query);

console.log(JSON.stringify(rules));
console.dir(rules, { depth: 5 });
console.dir(query, { depth: 5 });


let RESULT = prove(query, rules);

console.log("RESULT################# " + RESULT.length);
for (let r of RESULT) {
    console.dir(r, { depth: 5 });
    let vs = [];
    varsInTerm(r, vs);
    console.log(vs);
}
console.log("RESULT################# " + RESULT.length);