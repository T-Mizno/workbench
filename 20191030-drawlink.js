let W = 700;
let H = 500;

let n = 5;

let LM = 300;
let LN = 400;

let TOP = 10;
let BOTTOM = 3;
let LEFT = 50;
let RIGHT = 5;

let LINE_WIDTH = 10;
let LINE_MARGIN = 5;

let deltaLM = (H - BOTTOM - LM - TOP) / n;
let deltaLN = (W - RIGHT - LN - LEFT) / n;

function newMatrix(m, n) {

    let a = [];
    for (let i = 0; i < m; i++) {
        a[i] = [];
        for (let j = 0; j < n; j++) {
            a[i][j] = 0;
        }
    }
    return a;
}

let label = ["(a,1)", "(a,2)", "(a,3)", "(a,4)", "(a,5)", "(a,6)", "(a,7)", "(a,8)"];

let A = newMatrix(n, n);
let B = newMatrix(n, n);

for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
        if ((i + j) % 2 == 0) A[i][j] = 2;
        else A[i][j] = 0;
        if ((i + j) % 3 == 0) B[i][j] = 2;
        else B[i][j] = 0;
    }
}
function matStr(mat) {
    let str = "";
    for (let i = 0; i < mat.length; i++) {
        for (let j = 0; j < mat[i].length; j++) {
            str += "  " + mat[i][j];
        }
        str += "\n";
    }
    return str;
}
function matStdout(mat) { console.log(matStr(mat)); }

console.log("<PRE>");
matStdout(A);
matStdout(B);
console.log("</PRE>");

function begin() {
    return "<svg width=\"" + W + "\" height=\"" + H + "\">";
}
function end() {
    return "</svg>";
}
function line(fromX, fromY, toX, toY) {
    let str = "";
    str += "<line x1=\"" + fromX + "\" y1=\"" + fromY + "\" x2=\"" + toX + "\" y2=\"" + toY + "\" style=\"stroke-width:" + LINE_WIDTH + ";stroke: black;\"/>";
    return str;
}
function vendLine(fromX, fromY, viaX, viaY, toX, toY) {
    return "<polyline points=\"" + fromX + " " + fromY + ", " + viaX + " " + viaY + ", " + toX + " " + toY + "\" style=\"stroke: black; stroke-width: " + LINE_WIDTH + "; fill:none;\"/>";
}
let strSVG = "";
strSVG += begin();

for (let i = 0; i < n; i++) {
    let fx = LEFT + i * deltaLN;
    let fy = TOP + i * deltaLM;
    strSVG += vendLine(LEFT + LN - deltaLN / 2, fy, fx, fy, fx, TOP + LM - deltaLM / 2);

    fx = LEFT + LN + i * deltaLN;;
    fy = TOP + LM + i * deltaLM;
    strSVG += vendLine(fx, TOP + deltaLM * (n - 1) + deltaLM / 2, fx, fy, LEFT + deltaLN * (n - 1) + deltaLN / 2, fy);

    fx = LEFT + i * deltaLN;
    fy = TOP + LM + i * deltaLM;
    strSVG += vendLine(fx, fy - deltaLM / 2, fx, fy, fx + deltaLN / 2, fy);

    fx = LEFT + LN + i * deltaLN;
    fy = TOP + i * deltaLM;
    strSVG += vendLine(fx - deltaLN / 2, fy, fx, fy, fx, fy + deltaLM / 2);
}

for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
        let fx = LEFT + j * deltaLN;
        let fy = TOP + LM + i * deltaLM;
        if (A[i][j] < 1) {
            strSVG += line(fx, fy - deltaLM / 2, fx, fy + deltaLM / 2);
            strSVG += line(fx - deltaLN / 2, fy, fx - LINE_WIDTH / 2 - LINE_MARGIN, fy);
            strSVG += line(fx + LINE_WIDTH / 2 + LINE_MARGIN, fy, fx + deltaLN / 2, fy);
        }
        else {
            strSVG += line(fx - deltaLN / 2, fy, fx + deltaLN / 2, fy);
            strSVG += line(fx, fy - deltaLM / 2, fx, fy - LINE_WIDTH / 2 - LINE_MARGIN);
            strSVG += line(fx, fy + LINE_WIDTH / 2 + LINE_MARGIN, fx, fy + deltaLM / 2);
        }
    }
}
for (let i = 1; i < n; i++) {
    for (let j = 0; j < i; j++) {
        let fx = LEFT + LN + j * deltaLN;
        let fy = TOP + i * deltaLM;
        if (B[i][j] < 1) {
            strSVG += line(fx - deltaLN / 2, fy, fx + deltaLN / 2, fy);
            strSVG += line(fx, fy - deltaLM / 2, fx, fy - LINE_WIDTH / 2 - LINE_MARGIN);
            strSVG += line(fx, fy + LINE_WIDTH / 2 + LINE_MARGIN, fx, fy + deltaLM / 2);
        }
        else {
            strSVG += line(fx, fy - deltaLM / 2, fx, fy + deltaLM / 2);
            strSVG += line(fx - deltaLN / 2, fy, fx - LINE_WIDTH / 2 - LINE_MARGIN, fy);
            strSVG += line(fx + LINE_WIDTH / 2 + LINE_MARGIN, fy, fx + deltaLN / 2, fy);
        }
    }
}

for (let i = 0; i < n; i++) {
    let fx = LEFT + i * deltaLN - LINE_WIDTH / 2;
    let fy = TOP + i * deltaLM;
    strSVG += "< g style=\"fong-size: 14pt;\">";
    strSVG += "<text x=\"" + fx + "\" y=\"" + fy + "\" style=\"text-anchor: end\">" + label[i] + "</text>";
}

strSVG += end();
console.log(strSVG);
