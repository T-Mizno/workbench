/* 線に着色すると問題発生 */


// SVG の描画サイズ
let W = 700;
let H = 600;

// ループの本数
let n = 3;

// ループの縦の長さ
let LM = 1.5 * H / 2.5;

// ループの横の長さ
let LN = 1.5 * W / 2.5;

// ループを描画する範囲
let TOP = (H - LM) / (n + 1);
let BOTTOM = TOP;
let LEFT = (W - LN) / (n + 1);
let RIGHT = LEFT;

// ループ同士をずらす縦の距離
let deltaLM = (H - BOTTOM - LM - TOP) / (n - 1);
// ループ同士をずらす横の距離
let deltaLN = (W - RIGHT - LN - LEFT) / (n - 1);

// ループの線の幅
let LINE_WIDTH = Math.min(Math.min(deltaLM, deltaLN) / 2.5, 20);

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

//A = [[1, 2, 3], [1, 3, 3], [3, 4, 4]];

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

function loop(topLeftX, topLeftY, width, height, lWidth, aColor, isDash) {
    let dashStr = "";
    if (isDash) {
        dashStr = "stroke-dasharray: " + lWidth + " " + (0.3 * lWidth) + ";";
    }
    return "<rect x=\"" + topLeftX + "\" y=\"" + topLeftY + "\" width=\"" + width + "\" height=\"" + height + "\" style=\"fill: none; stroke: " + aColor + "; stroke-width: " + lWidth + ";" + dashStr + "\"/>";
}
function fill(topLeftX, topLeftY, width, height) {
    return "<rect x=\"" + topLeftX + "\" y=\"" + topLeftY + "\" width=\"" + width + "\" height=\"" + height + "\" style=\"fill: white; \"/>";
}

function drawBlock(topLeftX, topLeftY, width, height, r) {
    return "<rect x=\"" + topLeftX + "\" y=\"" + topLeftY + "\" width=\"" + width + "\" height=\"" + height + "\" rx=\"" + r + "\" ry=\"" + r + "\" style=\"fill:none; stroke: black; stroke-width:3; stroke-dasharray: 5 2; \"/>";
}

let strSVG = "";
strSVG += begin();

strSVG += fill(0, 0, W, H);

strSVG += drawBlock(LEFT / 3, TOP + LM - deltaLM, 2 * LEFT / 3 + deltaLN * n, deltaLM * n + 2 * BOTTOM / 3, 10);
strSVG += drawBlock(LEFT + LN - deltaLN, TOP / 3, deltaLN * n + 2 * RIGHT / 3, deltaLM * n + 2 * TOP / 3, 10);

for (let i = 0; i < n; i++) {
    let fx = LEFT + i * deltaLN;
    let fy = TOP + i * deltaLM;
    let strColor = "black";
    strSVG += loop(fx, fy, LN, LM, LINE_WIDTH, strColor, false);
}

for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
        let fx = LEFT + j * deltaLN;
        let fy = TOP + LM + i * deltaLM;

        // (fx, fy) は左下のブロックにおける i,j に対応するループの交点
        if (A[i][j] >= 1) {
            strSVG += fill(fx - deltaLN / 2, fy - LINE_WIDTH, deltaLN, LINE_WIDTH / 2);
            strSVG += fill(fx - deltaLN / 2, fy + LINE_WIDTH / 2, deltaLN, LINE_WIDTH / 2);
        }
        else {
            strSVG += fill(fx - LINE_WIDTH, fy - deltaLM / 2, LINE_WIDTH / 2, deltaLM);
            strSVG += fill(fx + LINE_WIDTH / 2, fy - deltaLM / 2, LINE_WIDTH / 2, deltaLM);
        }

        // (fx, fy) は右上のブロックにおける i,j に対応するループの交点
        fx = LEFT + LN + i * deltaLN;
        fy = TOP + j * deltaLM;
        if (B[i][j] >= 1) {
            strSVG += fill(fx - LINE_WIDTH, fy - deltaLM / 2, LINE_WIDTH / 2, deltaLM);
            strSVG += fill(fx + LINE_WIDTH / 2, fy - deltaLM / 2, LINE_WIDTH / 2, deltaLM);
        }
        else {
            strSVG += fill(fx - deltaLN / 2, fy - LINE_WIDTH, deltaLN, LINE_WIDTH / 2);
            strSVG += fill(fx - deltaLN / 2, fy + LINE_WIDTH / 2, deltaLN, LINE_WIDTH / 2);
        }

    }
}

for (let i = 0; i < n; i++) {
    let fx = LEFT + i * deltaLN - LINE_WIDTH / 2;
    let fy = TOP + i * deltaLM - 2 * LINE_WIDTH / 3;
    strSVG += "<g style=\"fong-size: 14pt;\"/>";
    strSVG += "<text x=\"" + fx + "\" y=\"" + fy + "\" style=\"font-family: Times New Roman; font-size: " + (deltaLM - LINE_WIDTH) + "; stroke:none; fill:black;\">" + label[i] + "</text>";
}

strSVG += end();
console.log(strSVG);
