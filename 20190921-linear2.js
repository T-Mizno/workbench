function isZero(x) {
  return Math.abs(x) < 0.0000000001;
}

function assert(condition) {
  if (!condition) {
    //throw message || "Assertion failed";
    throw "Assertion failed";
  }
}

function newMatrix(m, n) {
  assert(m > 0);
  assert(n > 0);

  let a = [];
  for (let i = 0; i < m; i++) {
    a[i] = [];
    for (let j = 0; j < n; j++) {
      a[i][j] = 0;
    }
  }
  return a;
}
function newVector(m) {
  assert(m > 0);
  let a = newMatrix(m, 1);
  for (let i = 0; i < m; i++) {
    a[i][0] = 0;
  }
  return a;
}

function matM(mat) { return mat.length; }
function matN(mat) { return mat[0].length; }

function matSetCopy(a, b) {
  assert(matM(a) <= matM(b));
  assert(matN(a) <= matN(a));
  for (let i = 0; i < matM(a); i++) {
    for (let j = 0; j < matN(a); j++) {
      b[i][j] = a[i][j];
    }
  }
}

function matRowForEach(mat, i, f) {
  assert(0 <= i && i < matM(mat));
  for (let j = 0; j < matN(mat); j++) {
    f(mat, i, j);
  }
}
function matColumnForEach(mat, j, f) {
  assert(0 <= j && j < matN(mat));
  for (let i = 0; i < matM(mat); i++) {
    f(mat, i, j);
  }
}
function matForEach(mat, f) {
  for (let i = 0; i < matM(mat); i++) {
    for (let j = 0; j < matN(mat); j++) {
      f(mat, i, j);
    }
  }
}

function matSetVal(a, v) {
  matForEach(a, function (mat, i, j) { mat[i][j] = v; });
}
function matSetMulti(a, b, c) {
  assert(matN(a) == matM(b));
  assert(matM(c) >= matM(a));
  assert(matN(c) >= matN(b));
  matForEach(c, function (mat, i, j) {
    let sum = 0;
    for (let k = 0; k < matN(a); k++) {
      sum += a[i][k] * b[k][j];
    }
    mat[i][j] = sum;
  });
}

function matNormalizeColumn(a, col) {
  let sum = 0;
  matColumnForEach(a, col, function (mat, i, j) { sum += mat[i][j]; });
  matColumnForEach(a, col, function (mat, i, j) { mat[i][j] = mat[i][j] / sum; });

}

function matDiff(x, y) {
  let sum = 0;
  matForEach(x, function (mat, i, j) {
    sum += Math.abs(mat[i][j] - y[i][j]);
  });
  return sum;
}

function powerMethod(mat, x, itrMax) {
  assert(matM(mat) == matN(mat));
  assert(matM(mat) == matM(x));

  let preX = newVector(matM(x));
  let lambda = matM(mat) * matM(mat);
  matSetVal(preX, 1);

  for (let itr = 1; itr <= itrMax; itr++) {
    matSetMulti(mat, preX, x);

    lambda = x[0][0] / preX[0][0];
    matNormalizeColumn(x, 0);

    x.itr = itr;
    x.lambda = lambda;

    if (isZero(matDiff(x, preX))) break;

    matSetCopy(x, preX);
  }

  x.ci = (x.lambda - matM(x)) / (matM(x) - 1);

  return;
}

//unsafe ver
function simpleGauss(a, b, x) {
  matSetVal(x, 1);

  for (let pivoti = 0; pivoti < matM(a); pivoti++) {
    for (let i = pivoti + 1; i < matM(a); i++) {
      let d = a[i][pivoti] / a[pivoti][pivoti];
      for (let j = pivoti; j < matN(a); j++) {
        a[i][j] = a[i][j] - d * a[pivoti][j];
      }
      b[i][0] = b[i][0] - d * b[pivoti][0];
    }
  }

  for (let i = matM(a) - 1; i >= 0; i--) {
    let sum = 0;
    for (let j = i + 1; j < matN(a); j++) {
      sum += a[i][j] * x[j][0];
    }
    x[i][0] = (b[i][0] - sum) / a[i][i];
  }

}

function matStr(mat) {
  let str = "";
  for (let i = 0; i < mat.length; i++) {
    str += matRowStr(mat, i) + "\n";
  }
  return str;
}
function matRowStr(mat, row) {
  let str = "";
  matRowForEach(mat, row, (a, i, j) => { str += " " + a[row][j].toFixed(8); });
  return str;
}
function matColumnStr(mat, col) {
  let str = "";
  matColumnForEach(mat, col, (a, i, j) => { str += " " + a[i][col].toFixed(8) + "\n"; });
  return str;
}

function matStdout(mat) { console.log(matStr(mat)); }

function matReciprize(mat) {
  for (let i = 0; i < matM(mat); i++) {
    mat[i][i] = 1;
    for (let j = i + 1; j < matN(mat); j++) {
      mat[j][i] = 1 / mat[i][j];
    }
  }
}

/*
let A = [
  [2, 1, 1],
  [4, 1, 0],
  [- 2, 2, 1]];

let b = [
  [1],
  [-2],
  [7]
];

let x = newVector(3);

matStdout(A);
console.log(matColumnStr(b, 0));

simpleGauss(A, b, x);
console.log(matColumnStr(x, 0));

*/