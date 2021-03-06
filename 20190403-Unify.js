
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

function varsInTerm(aT, vars) {
    if (!isComposition(aT) && !isVar(aT)) {
        return vars;
    }

    let t;
    if (isVar(aT)) t = { sample: aT };
    else t = aT;
    for (var k of Object.keys(t)) {
        if (isVar(t[k])) {
            if (t[k]['forward']) {
                varsInTerm(t[k]['forward'], vars);
            }
            let flg = false;
            for (let i = 0; i < vars.length; i++) {
                if (vars[i]['Var'] == t[k]['Var']) {
                    t[k] = vars[i];
                    flg = true;
                    break;
                }
            }
            if (!flg) {
                vars.push(t[k]);
            }
        }
        if (isComposition(t[k])) {
            varsInTerm(t[k], vars);
        }
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
    //console.log("deref"); console.log(dag);
    if (isAtomic(dag)) return dag;
    if (dag['forward']) {
        return deref(dag['forward']);
    }
    //if(isVar(dag)) return dag;
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

function recCopy(dag) {
    if (dag['copy']) {
        return dag['copy'];
    }
    if (isBottom(dag)) return dag;
    if (isAtomic(dag)) return dag;

    let c = {};
    for (let k of Object.keys(dag)) {
        c[k] = recCopy(dag[k]);
    }
    dag.copy = c;
    return c;
}


function unify(adag1, adag2) {
    //console.log("In u1, before tring..."); console.log(adag1); console.log(adag2);

    let dag1 = deref(adag1);
    let dag2 = deref(adag2);

    //console.log("In u1, tring..."); console.log(dag1); console.log(dag2);

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
        //dag1['forward'] = dag2;
        return dag1;
    }
    let shared = intersections(dag1, dag2);
    let newed = complements(dag1, dag2);

    dag1['forward'] = dag2;

    let isSuccess = true;
    for (var arc of shared) {
        let result = unify(dag1[arc], dag2[arc]);
        let flg = isBottom(result);
        if (flg) {
            isSuccess = false;
            break;
        }
        //isSuccess = isSuccess && (!flg);
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

function outTerm(t) {
    console.dir(t, { depth: 10 });
}
function unifyTest(tl, tr) {
    console.log("#######################");
    reduceVars({ l: tl, r: tr });
    outTerm(tl);
    outTerm(tr);
    let result = unify(tl, tr);
    console.log("result");
    outTerm(result);
    console.log("after tl");
    outTerm(tl);
    console.log("after tr");
    outTerm(tr);
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

//return;

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

let PRVCOUNT = 0;
function prove3(aQs, aCs, rs, results) {
    PRVCOUNT++;
    console.log("######################## PROVE COUNT" + PRVCOUNT);
    //if (PRVCOUNT > 1) return [];
    if (aQs.length < 1) {
        //console.log("SUCCESS!!!!!!!");
        if (aCs.length > 0) {
            //console.dir(aCs, { depth: 5 });
            results.push(aCs);
        }
        return true;
    }

    let flg = false;
    for (let rule of rs) {
        let qs = deepCopy(aQs);
        let cs = deepCopy(aCs);
        reduceVars({ cs: cs, qs: qs });
        let q = qs[0];
        qs.shift();

        let r = deepCopy(rule);
        reduceVarsWithRename(r);


        console.log("trying query");
        outTerm(q);
        console.log("qs");
        outTerm(qs);
        console.log("cs");
        outTerm(cs);
        console.log("trying rule");
        outTerm(r);


        let u = unify(q, r[0]);

        if (!isBottom(u)) {
            console.log("unify!!!"); outTerm(u);
            flg = true;
            r.shift();
            let tmpQ = r.concat(qs);

            //cs.push(u);

            cs.push(q);
            console.log("newQs"); outTerm(tmpQ);

            prove3(tmpQ, cs, rs, results);
        }
    }
    if (!flg) {
        console.log("FAILL!!!!!!!!!!!!!!!!");
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
let q3430 = [["likes", "Sandy", "Robin"]];
let q343 = [["likes", "Sandy", "?who"]];
let q3452 = [["likes", "?who", "Sandy"]];
let q3453 = [["likes", "Robin", "LEE"]];
let q3454 = [["likes", "?x", "?y"], ["likes", "?y", "?x"]];

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

let rule388 = [
    [{ t0: "rev", t1: "NIL", t2: "NIL" }],
    [{ t0: "rev", t1: { car: { Var: "x" }, cdr: { Var: "a" } }, t2: { Var: "b" } }, { t0: "rev", t1: { Var: "a" }, t2: { Var: "c" } }, { t0: "concat", t1: { Var: "c" }, t2: { car: { Var: "x" }, cdr: "NIL" }, t3: { Var: "b" } }],
    [{ t0: "concat", t1: "NIL", t2: { Var: "L" }, t3: { Var: "L" } }],
    [{ t0: "concat", t1: { car: { Var: "x" }, cdr: { Var: "a" } }, t2: { Var: "b" }, t3: { car: { Var: "x" }, cdr: { Var: "c" } } }, { t0: "concat", t1: { Var: "a" }, t2: { Var: "b" }, t3: { Var: "c" } }]
];
let q388c = [{ t0: "concat", t1: a2cons([1, 3, 5, 7]), t2: a2cons(["i", "j", "k"]), t3: { Var: "Z" } }];
let q388c2 = [{ t0: "concat", t1: { Var: "Before" }, t2: { Var: "After" }, t3: a2cons([1, 2, 3]) }];
let q388r = [{ t0: "rev", t1: a2cons([1, 2, 3, 5, 7, 8, 9]), t2: { Var: "Z" } }];

//let rules = a2rules(rule343);
//let query = a2q(q343);
let rules = rule388;//a2rules(rule338);
let query = q388c2;//a2q(q3393);
reduceVars(query);

console.log(JSON.stringify(rules));
console.dir(rules, { depth: 5 });
console.dir(query, { depth: 5 });


let RESULT = prove(query, rules);

console.log("RESULT################# " + RESULT.length);
for (let r of RESULT) {
    console.log("###################################");
    console.dir(r[0], { depth: 5 });

    let vs = [];
    varsInTerm(r, vs);
    console.dir(vs, { depth: 10 });
    /*
    for (let v of vs) {
        v['check'] = "OK";
    }
    console.log("After");
    console.log(vs);
    */

}
console.log("RESULT################# " + RESULT.length);
return;
let uu = unifyTest({
    t0: 'rev',
    t1: { car: 1, cdr: { car: 2, cdr: { car: 3, cdr: 'NIL' } } },
    t2: { Var: 'Z' }
},
    {
        t0: 'rev',
        t1: { car: { Var: 'x1' }, cdr: { Var: 'a2' }, t2: { Var: 'b3' } }
    },
    { t0: 'rev', t1: { Var: 'a2' }, t2: { Var: 'c4' } });
