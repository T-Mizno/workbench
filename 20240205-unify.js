let TIME_COUNTER = 0;
function getCurrentTime() { return TIME_COUNTER;}
function getNextTime() {
    TIME_COUNTER ++;
    return TIME_COUNTER;
}

let GEN_SYM_COUNTER = 5000;
function gensym() {
    GEN_SYM_COUNTER++;
    return "G"+GEN_SYM_COUNTER;
}

function arc(aLabel, aValue){
    return {whatis: "ARC",
            flg: "LIVE",
            label: aLabel,
            timestamp: getNextTime(),
            value: aValue};
}
function arcStr(a){
    return a["label"] + "==>" + a["value"];
}
function isArc(a) { return a["whatis"] == "ARC"; }
function arcP(a) { return isArc(a); }
function arcLabel(a) { return a["label"]; }
function arcSameLabel(a1, a2) { return arcLabel(a1) == arcLabel(a2); }
function arcValue(a) { return a["value"]; }
function arcTimestamp(a) { return a["timestamp"]; }
function arcSetTimestamp(a, aTime) { a["timestamp"] = aTime; }
function arcFlg(a) { return a["flg"]; }
function arcSetFlg(a, aFlg) { a["flg"] = aFlg; }

function arcs() {
    return [];
}
function arcsSize(as) {
    return as.length;
}
function arcsForeach(as, f) {
    as.map(f);
}
function arcsAdd(as, a) { as.push(a); }
function arcsIsEmpty(as) { return as.length <= 0; }
function arcsDiscard(as) {
    while(!arcsIsEmpty(as)){
        as.pop();
    }
} 
function arcsAppend(base, as) {
    arcsForeach(as, (a) => arcsAdd(base, a));
}
function arcsIntersection(as1, as2) {
    return as1.filter((a1)=>as2.some((a2)=> arcSameLabel(a1, a2)));
}
function arcsComplement(as1, as2) {
    return as1.filter((a1)=>! as2.some((a2)=> arcSameLabel(a1, a2)));
}


let ALL_NODES = [];

function _node(){
    let newNode = { type: "BARE-NODE",
                    id: gensym(),
                    name: "NO-NAME",
                    arcList: arcs(),
                    compArcList: arcs(),
                    forward: 0,
                    copy: 0,
                    generation: 0,
                    timestamp: getNextTime()};
    ALL_NODES.push(newNode);
    return newNode;
}

let EMPTY = _node();
EMPTY.type = "EMPTY";
EMPTY.name = "EMPTY";
EMPTY.forward = EMPTY;
EMPTY.copy = EMPTY;

function node() {
    let newNode = _node();
    newNode.forward = EMPTY;
    newNode.copy = EMPTY;
    return newNode;
}

function atm(str) {
    let a = node();
    a.type = "ATOM";
    a.name = str;
    return a;
}
function atmP(a) { return a.type == "ATOM"; }
function atmEqual(a1, a2) { return a1.name == a2.name; }
function atmStr(a) { return "@" + a.name + ":" + a.id; }

function vr(str) { // Variable
    let v = node();
    v.type = "VAR";
    v.name = str;
    return v;
}
function vrP(v) { return v.type == "VAR"; }
function vrStr(v) { return "Var-" + v.name + ":" + v.id; }

function inconsistency() {
    let i = node();
    i.type = "INCONSISTENCY";
    i.name = "INCONSISTENCY";
    return i;
}
function inconsistencyP(i) { return i.type == "INCONSISTENCY"; }
function inconsistencyStr(i) { return "INCONSISTENCY:" + i.id; }

function cfs(){
    let c = node();
    c.type = "COMPLEX";
    return c;
}
function cfsP(c) { return c.type == "COMPLEX"; }
function cfsArc(c, aLabel) {
    let result = undefined;
    arcsForeach(c.arcList, (a) => {
       if(arcLabel(a) == aLabel){
            //return a;
            result = a;
       }
    });
    if(result == undefined){
        console.log("ERROR: In CFS-ARC, there is no arc with label: "+ aLabel);
        // EXIT;
    }
    return result;
}
function cfsArcValue(c, aLabel) { return arcValue(cfsArc(c, aLabel)); }

function cfsClearArcFlgAndTimestamp(c) {
    if(! cfsP(c)) return;
    cfsClearArcFlgAndTimestamp0(c, getNextTime());
}
function cfsClearArcFlgAndTimestamp0(c, past) {
    arcsForeach(c.arcList, (a) => {
        if(arcTimestamp(a) < past) {
            arcSetTimestamp(a, getNextTime());
            arcSetFlg("LIVE");
            if(cfsP(arcValue(a))) cfsClearArcFlgAndTimestamp0(arcValue(a), past);
        }
    });
}

////////////////////////////////////////////////////////////
// for print node
////////////////////////////////////////////////////////////
function nodeStr(n) {
    getNextTime();
    return nodeStrI(n, 0);
}
function nodeStrI(n, i) {
    let eachLineF = (aN) => {
        let aStr = "";
        if(n.timestamp >= getCurrentTime()) aStr += "  ;;; (already appeared)";
        if(! n.forward == EMPTY) str += " ---> " + n.forward.id + "(" + n.generation + ")";
        aStr += "\n";
        n.timestamp = getCurrentTime();
        return aStr;
    }

    let str = "";
    if(atmP(n)) {
        str += atmStr(n);
        str += eachLineF(n);
    }


    if(vrP(n)) {
        str += vrStr(n);
        str += eachLineF(n);
    }

    if(inconsistencyP(n)) {
        str += inconsistencyStr(n);
        str += eachLineF(n);
    }

    if(cfsP(n)){
        str += "[c:" + n.id + "] (" + n.generation+ ")";
        if(! n.forward == EMPTY) str += " ---> " + n.forward.id + "(" + n.generation + ")";
        if(n.timestamp >= getCurrentTime()) {
            str += "  ;;; (already appeared)";
            str += "\n";
        }
        else {
            str += "\n";
            let indent = " ".repeat(i);
            n.timestamp = getCurrentTime();
            arcsForeach(n.arcList, (a)=>{
                str += indent + " *" + arcLabel(a) + " ==> ";
                str += nodeStrI(arcValue(a), i+4);
            });
            arcsForeach(n.compArcList, (a)=>{
                str += indent + " +" + arcLabel(a) + " ==> ";
                str += nodeStrI(arcValue(a), i+4);
            });
        }
    }
    return str;
}

////////////////////////////////////////////////////////////
// Unification
////////////////////////////////////////////////////////////
let UNIFY_GLOBAL_COUNTER = 10;

function unifyDG(dg1, dg2) {
    let result = unify0(dg1, dg2);
    UNIFY_GLOBAL_COUNTER++;
    return result;
}

function unify0(dg1, dg2) {
    try {
        let result = unify1(dg1, dg2);
        return copyDGWithCompArcs(dg1);
    }
    catch(e) {
        if(e instanceof UnifyFailException) {
            cfsClearArcFlgAndTimestamp(dg1); 
            cfsClearArcFlgAndTimestamp(dg2); 
            return inconsistency(); 
        }
        else {
            console.log("ERROR; Unify0");
            console.log(e.stack);
        }
    }
}

function UnifyFailException(message) {
  this.message = message;
  this.name = "UnifyFailException";
}

function unify1(dg1underef, dg2underef) {
    let dg1 = dereference(dg1underef);
    let dg2 = dereference(dg2underef);

    dg1.copy = EMPTY;
    dg2.copy = EMPTY;

    if( inconsistencyP(dg1) || inconsistencyP(dg2) ) {
        throw new UnifyFailException("Graph is Inconsinstncy");
    }

    if(dg1 == dg2) {
        return "UNIFY-SUCCESS";
    }

   if(vrP(dg1)) {
        dg1.forward = dg2;
        dg1.generation = UNIFY_GLOBAL_COUNTER;
        return "UNIFY-SUCCESS";
    }

    if(vrP(dg2)) {
        dg2.forward = dg1;
        dg2.generation = UNIFY_GLOBAL_COUNTER;
        return "UNIFY-SUCCESS";
    }

    if( atmP(dg1) && atmP(dg2) ) {
        if(atmEqual(dg1, dg2)) {
            dg2.forward = dg1;
            dg2.generation = UNIFY_GLOBAL_COUNTER;
            return "UNIFY-SUCCESS";
        }
        else {
            throw new UnifyFailException("Atoms are different");
        }
    }

    if( atmP(dg1) || atmP(dg2) ) {
        throw new UnifyFailException("Atoms are different");
    }

    // Both nodes are CFS;
    let shared = arcsIntersection(dg1.arcList, dg2.arcList);
    arcsForeach(shared, (s) => {
        let a1 = cfsArc(dg1, arcLabel(s));
        let a2 = cfsArc(dg2, arcLabel(s));

        if( arcTimestamp(a1) != arcTimestamp(a2) ) {
            arcSetTimestamp(a1, getNextTime());
            arcSetTimestamp(a2, arcTimestamp(a1));

            unify1(arcValue(a1), arcValue(a2));

            arcSetTimestamp(a1, getNextTime());
            arcSetTimestamp(a2, getNextTime());
        }
    });
    dg2.forward = dg1;
    dg2.generation = UNIFY_GLOBAL_COUNTER;

    let newarcs = arcsComplement(dg2.arcList, dg1.arcList);
    if(arcsIsEmpty(dg1.compArcList)) {
        dg1.generation = UNIFY_GLOBAL_COUNTER;
        arcsAppend(dg1.compArcList, newarcs);
    }
    else {
        if(dg1.generation == UNIFY_GLOBAL_COUNTER) {
            arcsAppend(dg1.compArcList, newarcs);
        }
        else {
            arcsDiscard(dg1.compArcList);
        }
    }
    return "UNIFY-SUCCESS";
}

function dereference(dg) {
    let dest = dg.forward;
    if(dg == dest) {
        dg.forward = EMPTY;
        return dg;
    }

    if(dest == EMPTY) {
        return dg;
    }

    if( (dg.generation == UNIFY_GLOBAL_COUNTER) || (dg.generation <= 9) ) {
        return dereference(dest);
    }

    dg.forward = EMPTY;
    return dg;
}

function copyDGWithCompArcs(dgUnderef) {
    let dg = dereference(dgUnderef);
    if( (dg.copy != EMPTY) && (dg.copy.generation == UNIFY_GLOBAL_COUNTER) ) {
        return dg.copy;
    }
    if(atmP(dg)) {
        let newcopy = atm(dg.name);
        newcopy.generation = UNIFY_GLOBAL_COUNTER;
        dg.copy = newcopy;
        return newcopy;
    }
    if(vrP(dg)) {
        let newcopy = vr(dg.name);
        newcopy.generation = UNIFY_GLOBAL_COUNTER;
        dg.copy = newcopy;
        return newcopy;
    }

    let newcopy = cfs();
    newcopy.generation = UNIFY_GLOBAL_COUNTER;
    dg.copy = newcopy;
    arcsForeach(dg.arcList, (a) => {
        let newarc = copyArc(a);
        arcsAdd(newcopy.arcList, newarc);
    });
    if( (! arcsIsEmpty(dg.compArcList)) && (dg.generation == UNIFY_GLOBAL_COUNTER) ) {
        arcsForeach(dg.compArcList, (a) => {
            let newarc = copyArc(a);
            arcsAdd(newcopy.arcList, newarc);
        });
    }
    arcsDiscard(dg.compArcList);
    return newcopy;
}

function copyArc(a) {
    return arc(arcLabel(a), copyDGWithCompArcs(arcValue(a)));
}

////////////////////////////////////////////////////////////
// Generalization
////////////////////////////////////////////////////////////
function generalizeDG(dg1, dg2) {
    UNIFY_GLOBAL_COUNTER++;
    cfsClearArcFlgAndTimestamp(dg1);
    let result = generalize00(dg1, dg2);
    UNIFY_GLOBAL_COUNTER++;
    return result;
}

function generalNode(dg1, dg2, f){
    let newnode = f();
    newnode.generation = UNIFY_GLOBAL_COUNTER;
    if(dg1.generation != UNIFY_GLOBAL_COUNTER) {
        dg1.generation = UNIFY_GLOBAL_COUNTER;
        dg1.forward = newnode;
    }
    if(dg2.generation != UNIFY_GLOBAL_COUNTER) {
        dg2.generation = UNIFY_GLOBAL_COUNTER;
        dg2.forward = newnode;
    }
    return newnode;
}

function generalize00(dg1Underef, dg2Underef){
    let dg1 = dereference(dg1Underef);
    let dg2 = dereference(dg2Underef);

    if(dg1 == dg2) return dg1;

    if(vrP(dg1) || vrP(dg2)) {
        return generalNode(dg1, dg2, ()=>{return vr("GDG01");});
    }

    if(atmP(dg1) && atmP(dg2)) {
        if(atmEqual(dg1, dg2)) {
            return generalNode(dg1, dg2, ()=>{return atm(dg1.name);});
        }
        else {
            return generalNode(dg1, dg2, ()=>{return vr("GDG1");});
        }
    }

    if(atmP(dg1) || atmP(dg2)){
        return generalNode(dg1, dg2, ()=>{return vr("GDG2");});
    }

    if(inconsistencyP(dg1) && inconsistencyP(dg2)) {
        return inconsistency();
    }

    if(inconsistencyP(dg1) || inconsistencyP(dg2)) {
        return generalNode(dg1, dg2, ()=>{return vr("GDG3");});
    }

    let shared = arcsIntersection(dg1.arcList, dg2.arcList);
    if(arcsIsEmpty(shared)) {
        let newnode = generalNode(dg1, dg2, ()=>{return vr("GDG40");});
        return vr("GDG41");
    }
    let newnode = generalNode(dg1, dg2, ()=>{return cfs();});
    let as = arcs();

    arcsForeach(shared, (s) => {
        let a1 = cfsArc(dg1, arcLabel(s));
        let a2 = cfsArc(dg2, arcLabel(s));
        if(arcTimestamp(a1) != arcTimestamp(a2)) {
            arcSetTimestamp(a1, getNextTime());
            arcSetTimestamp(a2, arcTimestamp(a1));
            arcsAdd(as, arc(arcLabel(s), generalize00(arcValue(a1), arcValue(a2))));
        }
    });

    arcsAppend(newnode.arcList, as);
    return newnode; 
}

//////////////////////////////////////////////////////////////////
// dgEqual for debug
//////////////////////////////////////////////////////////////////
function dgEqual(dg1, dg2){
    if(dg1 == dg2) return true;
    return dgEqual00(dg1, dg2, getNextTime());
}

function dgEqual00(dg1, dg2, past) {
    if( ( (dg1.timestamp >= past) || (dg2.timestamp >= past) )
        && (dg1.timestamp != dg2.timestamp) ) {
        return false;
    }

    dg1.timestamp = getNextTime();
    dg2.timestamp = dg1.timestamp;

    if(atmP(dg1) && atmP(dg2)){
        return atmEqual(dg1, dg2);
    }

    if(vrP(dg1) && vrP(dg2)){
        return true;
    }

    if(! (cfsP(dg1) && cfsP(dg2))){
        return false;
    }
    
    let shared = arcsIntersection(dg1.arcList, dg2.arcList);
    let flg = true;

    if(arcsSize(shared) != arcsSize(dg1.arcList)){
        return false;
    }

    if(arcsSize(shared) != arcsSize(dg2.arcList)){
        return false;
    }

    arcsForeach(shared, (s)=>{
        let v1 = arcValue(cfsArc(dg1, arcLabel(s)));
        let v2 = arcValue(cfsArc(dg2, arcLabel(s)));
        let a1 = cfsArc(dg1, arcLabel(s));

        if(arcTimestamp(a1) < past) {
            arcSetTimestamp(a1, getCurrentTime());
            flg = flg && dgEqual00(v1, v2, past);
        }
    });
    return flg;
}

//////////////////////////////////////////////////////////////////
// which is informative
//////////////////////////////////////////////////////////////////
function dgEqualOrMoreInformativeThan(dg1, dg2){
    let g = generalizeDG(dg1, dg2);
    return dgEqual(g, dg2);
}

//////////////////////////////////////////////////////////////////
// Object to nodes
//////////////////////////////////////////////////////////////////
function o2n(aObj){
    if(typeof aObj === "string") {
        if(aObj == "INCONSISTENCY") return inconsistency();
    }

    let n = o2n00(aObj, {});
    return copyDGWithCompArcs(n);
}

function o2n00(aObj, aTaggedNodes){
    if(typeof aObj === "string") {
        return atm(aObj);
    }

    for(let key in aObj){
        if(key.toUpperCase() == "ATOM") return atm(aObj[key]);
    }

    for(let key in aObj){
        if(key.toUpperCase() == "VAR") return vr(aObj[key]);
    }

    if(objIsTag(aObj)) {
        let tagName = objTagName(aObj);
        let v = aTaggedNodes[tagName];
        if(v == undefined) {
            v = vr("O2N-DEF");
            aTaggedNodes[objTagName(aObj)] = v;
        }
        // すでに同じタグ名で登録されている場合は、後に定義された方に forward する。
        v = dereference(v);
        let n = o2n00(objTagDef(aObj), aTaggedNodes);
        v.forward = n;
        return n;
    }

    if(objIsRef(aObj)) {
        let refName = objRefName(aObj);
        let v = aTaggedNodes[refName];
        if(v == undefined) {
            aTaggedNodes[refName] = vr("O2N-REF");
            return aTaggedNodes[refName];
        }
        return dereference(v);
    }

    let c = cfs();
    for(let key in aObj){
        let label = key;
        let value = o2n00(aObj[key], aTaggedNodes);
        arcsAdd(c.arcList, arc(label, value));
    }

    return c;
}

function objIsTag(o){
    let keys = Object.keys(o);
    if(keys.length != 2) return false;
    return ["TAG", "DEF"].every((t)=> keys.some((l)=>{ return l.toUpperCase() == t; }));
}
function objTagName(o){
    for(let key in o){
        if(key.toUpperCase() == "TAG") return o[key];
    }
    return "ERROR: CANNOT FIND TAG NAME";
}
function objTagDef(o){
    for(let key in o){
        if(key.toUpperCase() == "DEF") return o[key];
    }
    return "ERROR: CANNOT FIND TAG DEF VALUE";
}

function objRefName(o){
    for(let key in o){
        if(key.toUpperCase() == "REF") return o[key];
    }
    return "ERROR: CANNOT FIND REF NAME";
}
function objIsRef(o){
    let keys = Object.keys(o);
    if(keys.length != 1) return false;
    return Object.keys(o).some((l)=>{ return l.toUpperCase() == "REF"; });
}

//////////////////////////////////////////////////////////////////////
// Node to Object
//////////////////////////////////////////////////////////////////////
function n2o(dg) {
    if(inconsistencyP(dg)) return "INCONSISTENCY";
    let reentNodes = [];
    n2o00(dg, reentNodes, getNextTime());
    return n2o01(dg, reentNodes, getNextTime());
}
// record reent nodes
function n2o00(underefDG, aReentNodes, past) {
    let dg = dereference(underefDG);
    if(dg.timestamp >= past) aReentNodes.push(dg); // touch reent nodes
    dg.timestamp = getNextTime();
    if(cfsP(dg)) {
        arcsForeach(dg.arcList, (a) => {
            if(arcTimestamp(a) < past) {
                arcSetTimestamp(a, getNextTime());
                n2o00(arcValue(a), aReentNodes, past);
            }
        });
    }
}
function n2o01(underefDG, aReentNodes, past){
    let dg = dereference(underefDG);
    for(let n of aReentNodes) {
        if(n == dg){
            if(dg.timestamp >= past) {
                return {REF: dg.id};
            }
            dg.timestamp = getNextTime();
            return {TAG: dg.id, DEF:n2o02(dg, aReentNodes, past)};
        }
    }
    return n2o02(dg, aReentNodes, past);
}
function n2o02(dg, aReentNodes, past){
    if(atmP(dg)) return {ATOM: dg.name};
    if(vrP(dg))  return {VAR: dg.name};

    if(cfsP(dg)){
        let c = {};
        arcsForeach(dg.arcList, (a)=>{
            if(a.timestamp < past){
                arcSetTimestamp(a, getNextTime());
                c[arcLabel(a)] = n2o01(arcValue(a), aReentNodes, past);
            }
        });
        return c;
    }
    return {UNDEF: "ERROR; UNDEF"};
}

////////////////////////////////////////////////////////////
// for tests
////////////////////////////////////////////////////////////
function assertUnifyObject(comment, aO1, aO2, aOAns){
    let g1 = o2n(aO1);
    let g2 = o2n(aO2);
    let gAns = o2n(aOAns);
    return assertUnifyDG(comment, g1, g2, gAns);
}
function assertUnifyDG(comment, aG1, aG2, aGAns){
    console.log("### "+ comment + " ### assert unify #############################");
    console.log(nodeStr(aG1));
    console.log(nodeStr(aG2));
    let result = unifyDG(aG1, aG2);
    console.log("unify ;;;;;;;;;;;;");
    return assertEqualDG(result, aGAns);
}
function assertGeneralizeObject(comment, aO1, aO2, aOAns){
    let g1 = o2n(aO1);
    let g2 = o2n(aO2);
    let gAns = o2n(aOAns);
    return assertGeneralizeDG(comment, g1, g2, gAns);
}
function assertGeneralizeDG(comment, aG1, aG2, aGAns){
    console.log("### "+ comment + " ### assert generalize #############################");
    console.log(nodeStr(aG1));
    console.log(nodeStr(aG2));
    let result = generalizeDG(aG1, aG2);
    console.log("generalize ;;;;;;;;;;;;");
    return assertEqualDG(result, aGAns);
}
function assertEqualDG(aG1, aG2){
    console.log(nodeStr(aG1));
    console.log(nodeStr(aG2));
    let isRight = dgEqual(aG1, aG2);
    if(isRight){
        console.log("OK");
        console.log();
    }
    else{
        console.log("!!!!!!!!!!!!!!! WRONG !!!!!!!!!!!!!!!!!!");
        console.log();
    }
    return isRight;
}
function assertEqualObject(aO1, aO2){
    return assertEqualDG(o2n(aO1), o2n(aO2));
}

////////////////////////////////////////////////////////////
// for lisp
////////////////////////////////////////////////////////////
function o2s(o){
    if(objIsTag(o)){
        let tagName = objTagName(o);
        let tagDef = objTagDef(o);
        return "(TAG "+ tagName+ " "+o2s(tagDef) + ")";
    }
    if(objIsRef(o)){
        let refName = objRefName(o);
        return "(REF " + refName + ")";
    }
    if(typeof o === "string") return "(ATOM \"" + o + "\")";
    for(let key in o) if(key.toUpperCase() == "ATOM") return "(ATOM \""+ o[key] + "\")";
    for(let key in o) if(key.toUpperCase() == "VAR") return "(VAR \""+ o[key] + "\")";
    
    let str = "([] ";
    for(let key in o){
        str += "("+key+" "+o2s(o[key])+")";
    }
    str = str + ")";
    return str;
}

function o2sSetf(varName, o){
    return "(setf " + varName + " '" + o2s(o) + ")";
}

//////////////////////////////////////////////////////////////////////
// Test sets
//////////////////////////////////////////////////////////////////////
let sampleObjects = {
// for primitive
primA00: {atom: "123"},
primA01: "123",
primA02: {atom: "789"},
primV00: {var: "345"},
primC00: {"hai": {atom: "foo"}},
primC01: {"delta": {var: "go"}},
primC02: {"hai":   {var: "went"}},
primC03: {"hai":  "foo"},
primI00: "INCONSISTENCY",
primR01: {a: {TAG:"A", DEF:{atom:"hai"}},
          b: {REF:"A"}},
primR02: {a: {ATOM:"hai"},
          b: "hai"},

//for dg-equal
eqG1: {a: {atom: "hai"},
       b: {atom: "foo"}},
eqG1Copy: {a: "hai",
           b: {atom: "foo"}},
eqG2: {a: {tag:"H", def:{var:"hei"}},
       b: {ref:"H"}},
eqG2Copy: {a: {REF:"I"},
           b: {Tag:"I", def:{VAR:"hai"}}},
eqG3: {a: {aa: {ATOM: "hoo"}},
       b: {aa: "hoo"}},
eqG3Copy: {a: {aa: "hoo"},
           b: {aa: {atom:"hoo"}}},
eqG4: {a: {tag:"AA", def:{aa: "hoo",
                          bb: {var:"fff"}}},
       b: {ref:"AA"}},
eqG4Copy: {a: {ref:"BB"},
           b: {tag:"BB", def:{aa: {atom:"hoo"},
                              bb: {var:"hei"}}}},
eqG5: {tag:"X", def:{z: {ref:"X"},
                     a: {tag:"AA", def:{aa: {atom:"hoo"},
                                        bb: {var:"fff"},
                                        cc: {ref:"X"}}},
                     b: {ref:"AA"},
                     c: {atom:"hoo"}}},
eqG5Copy: {tag:"Y", def:{a: {ref:"BB"},
                         b: {tag:"BB", def:{aa: {atom:"hoo"},
                                            bb: {var:"hei"},
                                            cc: {ref:"Y"}}},
                         z: {ref:"Y"},
                         c: {atom:"hoo"}}},

//example fail
failG1: {a: {b: "test"}},
failG2: {a: {atom:"ex"}},

//example reentrancy
rc10: {a: {tag:"a1", def:{atom:"1"}},
       b: {atom:"2"},
       c: {ref:"a1"}},
rc11: {a: {aa: {tag:"a1", def:{atom:"11"}},
           ab: {ref:"a1"}},
       b: {bb:   {atom:"22"},
           beta: {atom:"777"}},
       c: {ref:"a1"}},
rc12: {a: {ab:   {tag:"a1", def:{atom:"12"}},
           aa:   {ref:"a1"}},
       b: {beta: {atom:"777"},
           bb:   {atom:"777"}},
       e: {ref:"a1"}},
rc14: {a: "12",
       b: "14",
       c: "15"},
rc20: {a: "1",
       b: {tag:"b2", def:"2"},
       c: {ref:"b2"}},
rc22: {a: "12",
       b: {tag:"b2", def:"22"},
       c: {ref:"b2"}},

//example minimal syclic
mc1: {tag:"mc1", def: {a: {ref:"mc1"},
                       b: {c: {ref:"mc1"}}}},
mc2: {tag:"mc2", def: {a: {ref:"mc2"},
                       b: {tag:"mcb", def: {c: {ref:"mcb"},
                                            d: {atom:"y"}}}}},
mcu: {tag:"t", def:{a: {ref:"t"},
                    b: {tag:"m", def:{c: {ref:"t"},
                                      d: {tag:"Y", def:"y"}}},
                    c: {ref:"m"},
                    d: {ref:"Y"}}},
mcg: {tag:"t", def:{a: {ref:"t"},
                    b: {c: {var:"New"}}}},

//example p.24
p24dg: {subject:   {agreement: {tag:"X01", def:{number: "singular",
                                               person: {atom: "third"},
                                               gender: {atom: "feminine"}}}},
       predicate: {agreement: {ref: "X01"}}},

//example p.26
p26dg1: {born: {tag:"X01", def: "Tokyo"},
         home: {ref:"X01"}},
p26dg2: {born: {atom: "Tokyo"},
         home: "Tokyo"},

//example p.27-28
p27dg1: {var: "[]"},
p27dg2: {category: {var: "[]"}},
p28dg3: {category:  {atom:"N"},
         agreement: {number: "singular"}},
p28dg4: {category:  {atom:"N"},
         agreement: {number: {atom:"singular"},
                     person: {atom:"third"}}},
p28dg5: {category:  {atom:"N"},
         agreement: {number: "singular",
                     person: "third"},
         subject:   {number: {atom:"singular"},
                     person: {atom:"third"}}},
p27dg6: {category:  {atom:"N"},
         agreement: {tag:"X01", def:{number: "singular",
                                     person: {atom: "third"}}},
         subject: {REF:"X01"}},

//example p.37-38
p37dg1: {a: {a: {tag:"X01", def: {var:"X01"}}},
            b: {ref:"X01"}},
p37dg2: {a: {tag:"X02", def:{var:"X02"}},
        b: {a: {ref:"X02"}}},
p38dg3: {a: {tag:"X01", def: {a: {ref:"X02"}}},
        b: {tag:"X02", def: {a: {ref:"X01"}}}},

//example p.39
p39dg1: {a: {a: {tag:"X01", def:{var:"[]"}}},
         b: {ref:"X01"}},
p39dg2: {a: {tag:"X02", def:{var:"[]"}},
         b: {a: {ref:"X02"}}},
p39dg3: {a: {tag:"X01", def:{a: {ref:"X02"}}},
         b: {tag:"X02", def:{a: {ref:"X01"}}}},

//example p.74
p74dg1: {a: "S",
         b: {var:"[]"}},
p74dg2: {a: {tag:"X", def:{var:"[]"}},
         b: {ref:"X"},
         c: {atom:"t"}},
p75dg: {a: {tag:"Y", def:{atom:"S"}},
        b: {ref:"Y"},
        c: "t"},

//example pp.78-79
p78dg1: {a: "X",
         b: {var:"[]"} },
p78dg2: {a: {var:"[]"},
         b: {d: {atom:"Y"}},
         c: {atom:"Z"}},
p79dg3: {a: {atom:"X"},
         b: {d: {atom:"Y"}},
         c: {atom:"Z"}},
p79dg: {a: {var:"V1"},
        b: {var:"V2"}},

//example pp.79-80
p79dg1: {a: {d: "X"},
         b: {var:"[]"},
         c: {atom:"Y"} },
p79dg2: {a: {d: {tag:"X", def:{atom:"X"}},
             e: {ref:"X"}},
         b: {f: {atom:"Z"}},
         c: {var:"[]"}},
p80dg3: {a: {d: {ref:"X"},
             e: {tag:"X", def:"X"}},
         b: {f: "Z"},
         c: "Y"},
p80dg: {a: {d: "X"},
        b: {var:"[]"},
        c: {var:"New"}},
        

//example pp.81-82
p81dg1: {a: {tag:"dV", def: {d: {tag:"V", def:{var:"[]"}}}},
         b: {ref:"dV"},
         c: {ref:"V"}},
p81dg2: {tag:"X", def: {a: {var:"[]"},
                        b: {d: {var:"[]"}},
                        c: {e: {ref:"X"}},
                        f: {atom:"X"}}},
p82dg3: {TAG:"START", def:{a: {TAG:"M", def:{d: {TAG:"BOTTOM", def:{e:{REF:"START"}}}}},
                           b: {REF:"M"},
                           c: {REF:"BOTTOM"},
                           f: "X"}}, 

//example pp.83-84
p83dg1: {a: {a: {tag:"V", def:{var:"[]"}}},
             b: {ref:"V"}},
p83dg2: {a: {tag:"V", def:{var:"[]"}},
         b: {a: {ref:"V"}}},
p84dg3: {a: {TAG:"M", DEF:{a: {TAG:"B", DEF:{a: {REF:"M"}}}}},
         b: {REF:"B"}},
//example pp.112-114
p112rule1: {DTRS: {DTR1: {SYN: {HEAD: {tag:"X05", def:{var:"X05"}}}}},
            SYN:  {HEAD: {ref:"X05"}}},

p113rule2: {DTRS: {DTR1: {SYN: {HEAD:   {COH: {tag:"X06", def:{var:"X06"}}}}},
                   DTR2: {SYN: {SUBCAT: {FIRST: {ref:"X06"},
                                         REST:  {tag:"X10", def:{var:"X10"}}}}}},
            SYN:  {SUBCAT: {ref:"X10"}}},

p113rule3: {DTRS: {DTR1: {SYN: {HEAD: {COH: {tag:"X06", def:{var:"X06"}}}}},
                   DTR2: {ref:"X06"}}},

p114dg1: {DTRS: {DTR1: {SYN: {HEAD: {tag:"X05", def:{COH: {tag:"X06", def:{SYN: {SUBCAT: {FIRST: {ref:"X06"},
                                                                                          REST:  {tag:"X09", def:{var:"[]"}}}}}}}}}},
                DTR2: {ref:"X06"}},
          SYN: {HEAD: {ref:"X05"},
                SUBCAT: {ref:"X09"}}},

//example pp.114-115
p114lex1: {"SYN": {"HEAD": {"AGR": {"GEN":  {atom: "FEM"},
                                    "NUM":  {atom: "SING"},
                                    "PERS": {atom: "THIRD"}},
                            "CASE":  {atom: "-MINIATIVE"},
                            "MAJ":   {atom: "N"},
                            "NFORM": {atom: "NORMAL"},
                            "PRED":  {atom: "MINUS"}}}},

p114lex2: {"SYN": {"HEAD": {"AGR": {"GEN":  "FEM",
                                    "NUM":  "SING",
                                    "PERS": "THIRD"},
                            "CASE":  "OBJECTIVE",
                            "MAJ":   "N",
                            "NFORM": "NORMAL",
                            "PRED":  "MINUS"}}},

p115dg2: {DTRS: {DTR2: {tag:"X03", def:{SYN: {SUBCAT: {REST:  {tag:"X06", def:{var:"[]"}},
                                                       FIRST: {ref:"X03"}}}}},
                 DTR1: {SYN: {HEAD: {tag:"X09", def:{COH:   {ref:"X03"},
                                                     PRED:  "MINUS",
                                                     NFORM: "NORMAL",
                                                     MAJ:   "N",
                                                     CASE:  "-MINIATIVE",
                                                     AGR:   {PERS: "THIRD",
                                                             NUM: "SING",
                                                             GEN: "FEM"}}}}}},
           SYN: {SUBCAT: {ref:"X06"},
                 HEAD:   {ref:"X09"}}},

p115dg3: {DTRS: {DTR2: {tag:"X03", def:{SYN: {SUBCAT: {REST:  {tag:"X06", def:{var:"[]"}},
                                                       FIRST: {ref:"X03"}}}}},
                 DTR1: {SYN: {HEAD: {tag:"X09", def:{COH:   {ref:"X03"},
                                                     PRED:  "MINUS",
                                                     NFORM: "NORMAL",
                                                     MAJ:   "N",
                                                     CASE:  "OBJECTIVE",
                                                     AGR:   {PERS: "THIRD",
                                                             NUM: "SING",
                                                             GEN: "FEM"}}}}}},
           SYN: {SUBCAT: {ref:"X06"},
                 HEAD:   {ref:"X09"}}},

last: "INCONSISTENCY"
};

for(let key in sampleObjects) {
    console.log(key);
    console.log(nodeStr(sampleObjects[key]));
}

let tests = [
    assertEqualObject(sampleObjects["primA00"], sampleObjects["primA01"]),
    (!assertEqualObject(sampleObjects["primA00"], sampleObjects["primA02"])),
    assertEqualObject(sampleObjects["primC00"], sampleObjects["primC03"]),
    assertEqualObject(sampleObjects["eqG1"],    sampleObjects["eqG1Copy"]),
    assertEqualObject(sampleObjects["eqG2"],    sampleObjects["eqG2Copy"]),
    assertEqualObject(sampleObjects["eqG3"],    sampleObjects["eqG3Copy"]),
    assertEqualObject(sampleObjects["eqG4"],    sampleObjects["eqG4Copy"]),
    assertEqualObject(sampleObjects["eqG5"],    sampleObjects["eqG5Copy"]),
    assertUnifyObject("minimal cycle", sampleObjects["mc1"], sampleObjects["mc2"], sampleObjects["mcu"]),
    assertGeneralizeObject("minimal cycle", sampleObjects["mc1"], sampleObjects["mc2"], sampleObjects["mcg"]),
    assertGeneralizeObject("p.26", sampleObjects["p26dg1"], sampleObjects["p26dg2"], sampleObjects["p26dg2"]),
    (!assertGeneralizeObject("p.26", sampleObjects["p26dg2"], sampleObjects["p26dg1"], sampleObjects["p26dg1"])),

    assertUnifyObject("pp.37-38", sampleObjects["p37dg1"], sampleObjects["p37dg2"], sampleObjects["p38dg3"]),
    assertUnifyObject("p.39",     sampleObjects["p39dg1"], sampleObjects["p39dg2"], sampleObjects["p39dg3"]),
    assertUnifyObject("pp.74-75", sampleObjects["p74dg1"], sampleObjects["p74dg2"], sampleObjects["p75dg"]),
    assertUnifyObject("pp.78-79", sampleObjects["p78dg1"], sampleObjects["p78dg2"], sampleObjects["p79dg3"]),
    assertGeneralizeObject("pp.78-79", sampleObjects["p78dg1"], sampleObjects["p78dg2"], sampleObjects["p79dg"]),
    assertUnifyObject("pp.79-80", sampleObjects["p79dg1"], sampleObjects["p79dg2"], sampleObjects["p80dg3"]),
    assertGeneralizeObject("pp.79-80", sampleObjects["p79dg1"], sampleObjects["p79dg2"], sampleObjects["p80dg"]),
    assertUnifyObject("pp.81-82", sampleObjects["p81dg1"], sampleObjects["p81dg2"], sampleObjects["p82dg3"]),
    assertUnifyObject("pp.83-84", sampleObjects["p83dg1"], sampleObjects["p83dg2"], sampleObjects["p84dg3"]),
    assertUnifyDG("pp.112-114", unifyDG(o2n(sampleObjects["p112rule1"]), o2n(sampleObjects["p113rule2"])), o2n(sampleObjects["p113rule3"]), o2n(sampleObjects["p114dg1"])),
    assertUnifyObject("pp.114-115-1", sampleObjects["p114dg1"], sampleObjects["p114lex1"], sampleObjects["p115dg2"]),
    assertUnifyObject("pp.114-115-2", sampleObjects["p114lex2"], sampleObjects["p114dg1"], sampleObjects["p115dg3"]),
    assertEqualDG(o2n(n2o(o2n(sampleObjects["p114lex2"]))), o2n(sampleObjects["p114lex2"])),
    true];

console.log("All tests are right?");
//console.log(tests.every((t)=>{return t}));

/*
for(let key in sampleObjects) {
    let o = sampleObjects[key];
    console.log(o2sSetf(key, o));
}
*/
