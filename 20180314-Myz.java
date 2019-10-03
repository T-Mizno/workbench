import java.util.*;

public class Myz {
    public static void multi(double[][] A, double[][] B, double[][] AB) {
        assert A[0].length == B.length;
        assert A.length == AB.length;
        assert B[0].length == AB[0].length;

        for (int i = 0; i < A.length; i++) {
            for (int j = 0; j < B[0].length; j++) {
                AB[i][j] = 0.0;
                for (int k = 0; k < A[0].length; k++) {
                    AB[i][j] += A[i][k] * B[k][j];
                }
            }
        }
    }

    public static double[][] multi(double[][] A, double[][] B) {
        assert A[0].length == B.length;

        double[][] AB = new double[A.length][B[0].length];
        multi(A, B, AB);
        return AB;
    }

    public static void multi(double[][] A, double[] x, double[] Ax) {
        assert A[0].length == x.length;
        assert A.length == Ax.length;

        for (int i = 0; i < A.length; i++) {
            Ax[i] = 0.0;
            for (int j = 0; j < A[0].length; j++) {
                Ax[i] += A[i][j] * x[j];
            }
        }
    }

    public static double[] multi(double[][] A, double[] x) {
        assert A[0].length == x.length;

        double[] Ax = new double[A.length];
        multi(A, x, Ax);
        return Ax;
    }

    public static double[] add(double[] a, double[] b) {
        assert a.length == b.length;

        double[] c = new double[a.length];
        add(a, b, c);
        return c;
    }

    public static void add(double[] a, double[] b, double[] c) {
        assert a.length == b.length;
        assert a.length == c.length;

        for (int i = 0; i < a.length; i++) {
            c[i] = a[i] + b[i];
        }
    }

    public static double[][] trans(double[][] A) {
        double[][] tA = new double[A[0].length][A.length];

        for (int i = 0; i < A.length; i++) {
            for (int j = 0; j < A[0].length; j++) {
                tA[j][i] = A[i][j];
            }
        }
        return tA;
    }

    public static double[][] randomMatrix(int m, int n) {
        assert m > 0;
        assert n > 0;

        double[][] mat = new double[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                mat[i][j] = Math.random() * 100;
            }
        }
        return mat;
    }

    public static double[] randomVector(int m) {
        assert m > 0;

        double[] vec = new double[m];
        for (int i = 0; i < m; i++) {
            vec[i] = Math.random() * 100;
        }
        return vec;
    }

    public static double norm2(double[] a) {
        double x = 0.0;
        for (int i = 0; i < a.length; i++) {
            x += a[i] * a[i];
        }
        return Math.sqrt(x);
    }

    public static void stdout(double[][] A) {
        for (int i = 0; i < A.length; i++) {
            for (int j = 0; j < A[0].length; j++) {
                System.out.print("  " + A[i][j]);
            }
            System.out.println();
        }
    }

    public static void stdout(double[] x) {
        for (int j = 0; j < x.length; j++) {
            System.out.print("  " + x[j]);
        }
        System.out.println();
    }

    public static double diff(double[] a, double[] b) {
        assert a.length == b.length;

        double x = 0.0;
        for (int i = 0; i < a.length; i++) {
            x += Math.pow(a[i] - b[i], 2);
        }
        return Math.sqrt(x);
    }

    // use Gauss
    public static double[] GaussLeastSquare(double[][] A, double[] b) {
        double[][] At = trans(A);
        double[][] AtA = multi(At, A);
        double[] Atb = multi(At, b);

        Gauss g = new Gauss(AtA, Atb);
        g.gauss();

        return g.x;
    }

    public static void setGeomMean(double[][] A, double[] b, int m, int n) {
        for (int i = 0; i < m; i++) {
            double mean = 1.0;
            for (int j = 0; j < n; j++) {
                mean *= A[i][j];
            }
            b[i] = Math.pow(mean, 1.0 / (double) n);
        }
    }

    public static void powerMethod(double[][] A, double[] x, double[] preX, double[] cis, int n) {
        for (int i = 1; i < n; i++) {
            preX[i] = 1.0;
        }
        int itr = 0;
        double lambda = 1.0;

        while (true) {
            multi(A, preX, x);
            lambda = x[0] / preX[0];
            normalize(x);
            double dif = 0.0;
            for (int i = 0; i < n; i++) {
                dif += Math.abs(preX[i] - x[i]);
            }
            if ((dif < 10E-8) || (itr > 3000)) {
                break;
            }
            for (int i = 0; i < n; i++) {
                preX[i] = x[i];
            }
            itr++;
        }
        // cis[0] = (lambda-n)/(n-1);
        cis[0] = lambda;
        cis[1] = itr;
    }

    public static void normalize(double[] x) {
        double norm = 0.0;
        for (int i = 0; i < x.length; i++) {
            norm += x[i];
        }
        for (int i = 0; i < x.length; i++) {
            x[i] /= norm;
        }
    }

    public static void main(String[] argv) {
        int n = 4;
        /*
         * double[][] A = {{ 0.0, 1.0/3.0, 5.0, 3.0} ,{3.0, 0.0, 1.0/3.0, 1.0/2.0}
         * ,{1.0/5.0, 3.0, 0.0, 3.0} ,{1.0/3.0, 2.0, 1.0/3.0, 0.0}};
         */

        double[][] A = { { 1.0, 2.0, 6.0, 2.0 }, { 1.0 / 2.0, 1.0, 4.0, 3.0 }, { 1.0 / 6.0, 1.0 / 4.0, 1.0, 1.0 / 2.0 },
                { 1.0 / 2.0, 1.0 / 3.0, 2.0, 1.0 } };

        /*
         * double[][] A = {{ 1.0, 5.0, 5.0, 9.0} ,{1.0/5.0, 1.0, 1.0/7.0, 5.0}
         * ,{1.0/5.0, 7.0, 1.0, 1.0/5.0} ,{1.0/9.0, 1.0/5.0, 5.0, 1.0}};
         */
        double[][] cA = new double[4][4];
        double[][] Ax = new double[4][4];
        double[][] c = new double[4][4];
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++) {
                c[i][j] = 0.0;
            }
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++) {
                cA[i][j] = c[i][j] * A[i][j];
            }
        double[] eigen = new double[4];
        double[] tmpV = new double[4];
        double[] CIs = new double[2];
        int itr;

        stdout(A);
        powerMethod(A, eigen, tmpV, CIs, 4);

        itr = 0;
        while (true) {
            for (int i = 0; i < 4; i++) {
                double tmpC = 0.0;
                for (int j = 0; j < 4; j++)
                    if (j != i)
                        tmpC += Math.abs(eigen[i] - A[i][j] * eigen[j]);
                if (Math.abs(tmpC) < 10E-7) {
                    for (int j = 0; j < 4; j++)
                        c[i][j] = 1.0 / 3.0;
                } else {
                    for (int j = 0; j < 4; j++)
                        if (j != i)
                            c[i][j] = (tmpC - Math.abs(eigen[i] - A[i][j] * eigen[j])) / tmpC / 2.0;
                }
                c[i][i] = 0.0;
            }
            // for(int i=0; i<4; i++) for(int j=0; j<4; j++) { c[i][j] = 0.0;} c[0][0] =
            // 1.0; c[1][2] = 1.0; c[2][3] = 1.0; c[3][1]=1.0;
            /*
             * for(int i=0; i<4; i++) for(int j=0; j<4; j++) {c[i][j] = 0.0;} c[0][0] = 1.0;
             * //0.0; for(int i=1; i<4; i++) for(int j=0; j<i; j++) { c[i][j] = 1.0 /
             * (double)(i); }
             */
            for (int i = 0; i < 4; i++)
                for (int j = 0; j < 4; j++) {
                    cA[i][j] = c[i][j] * A[i][j];
                }
            powerMethod(cA, eigen, tmpV, CIs, 4);
            if ((Math.abs(CIs[0] - 1.0) < 10E-8) || (itr > 2999))
                break;
            itr++;
        }
        System.out.println("cA");
        stdout(cA);
        System.out.println("cof");
        stdout(c);
        System.out.println(";;; lambda = " + CIs[0] + " ;;;  itr = " + CIs[1]);
        normalize(eigen);
        stdout(eigen);
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                Ax[i][j] = A[i][j] * eigen[j];
        System.out.println("Ax");
        stdout(Ax);
        System.out.println("********* itr ****" + itr);

        // powerMethod(A, eigen, tmpV, CIs, 4);
        System.out.println(";;; Base lambda = " + CIs[0] + " ;;;  itr = " + CIs[1]);
        normalize(eigen);
        stdout(eigen);

        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++) {
                A[i][j] = A[i][j] * eigen[j] - eigen[i];
            }
        stdout(A);
        int[][] s = new int[n][n];
        int[][] d = new int[n][n];
        int[] ns = new int[n];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++) {
                if (Math.abs(A[i][j] - 0.0) < 10E-12)
                    s[i][j] = 0;
                else if (A[i][j] > 0.0)
                    s[i][j] = 1;
                else
                    s[i][j] = -1;
            }
        SC.stdout("s", s);
        if (SC.isSC(s, ns, d, n))
            System.out.println("Efficient");
    }
}

class Gauss {
    double[][] A;
    double[] b;
    int M;
    int N;
    int[] P;

    Vector<int[]> pivots;

    double[] x;
    double[][] xs;

    double epsillon = 1.0e-7;

    public Gauss(double[][] aA, double[] ab) {
        M = aA.length;
        N = aA[0].length;
        A = new double[M][N];
        b = new double[M];
        P = new int[M];

        pivots = new Vector<int[]>();

        x = new double[N];
        xs = new double[0][N];

        for (int i = 0; i < M; i++) {
            for (int j = 0; j < N; j++) {
                A[i][j] = aA[i][j];
            }
            b[i] = ab[i];
            P[i] = i;
        }
    }

    void gauss() {
        forward();

        for (int j = 0; j < N; j++) {
            x[j] = 0.0;
        }
        backward();

        {
            Vector<Integer> freeJs = new Vector<Integer>();
            double[] tmpB = new double[M];

            for (int j = 0; j < N; j++) {
                if (isFreeJ(j))
                    freeJs.add(new Integer(j));
            }
            for (int i = 0; i < M; i++) {
                tmpB[i] = 0.0;
            }
            xs = new double[freeJs.size()][N];
            for (int f = 0; f < freeJs.size(); f++) {
                for (int j = 0; j < N; j++) {
                    xs[f][j] = 0.0;
                }
                xs[f][freeJs.elementAt(f).intValue()] = 1.0;
                backward(tmpB, xs[f]);
            }
        }
    }

    void forward() {
        int pi = 0, pj = 0;
        for (pi = 0; pi < M; pi++) {
            pj = nextPivotj(pi, pj);
            if (!ijInRange(pi, pj))
                return;
            updateP(pi, pj);
            addPivot(pi, pj);
            for (int i = pi + 1; i < M; i++) {
                forwardOneLine(pi, pj, i);
            }
            pj = pj + 1;
        }
    }

    void forwardOneLine(int pi, int pj, int i) {
        double pivot = A[P[i]][pj] / A[P[pi]][pj];
        A[P[i]][pj] = pivot;
        for (int j = pj + 1; j < N; j++) {
            A[P[i]][j] = A[P[i]][j] - (pivot * A[P[pi]][j]);
        }
        b[P[i]] = b[P[i]] - (pivot * b[P[pi]]);
    }

    void updateP(int pi, int pj) {
        double max = Math.abs(A[P[pi]][pj]);
        int maxPI = pi;

        for (int i = pi + 1; i < M; i++) {
            if (Math.abs(A[P[i]][pj]) > max) {
                max = Math.abs(A[P[i]][pj]);
                maxPI = i;
            }
        }

        // swap
        {
            int tmp;
            tmp = P[pi];
            P[pi] = P[maxPI];
            P[maxPI] = tmp;
        }
    }

    boolean isZero(double val) {
        return Math.abs(val) < epsillon;
    }

    boolean ijInRange(int row, int col) {
        return (row >= 0) && (col >= 0) && (row < M) && (col < N);
    }

    boolean underColumnAreZero(int pi, int pj) {
        for (int i = pi; i < M; i++) {
            if (!isZero(A[P[i]][pj]))
                return false;
        }
        return true;
    }

    int nextPivotj(int pi, int pj) {
        for (int j = pj; j < N; j++) {
            if (!underColumnAreZero(pi, j))
                return j;
        }
        return -1; // fail
    }

    void addPivot(int pi, int pj) {
        int[] pair = new int[2];
        pair[0] = pi;
        pair[1] = pj;
        pivots.add(pair);
    }

    boolean isFreeI(int row) {
        for (int i = 0; i < pivots.size(); i++) {
            if (pivots.elementAt(i)[0] == row)
                return false;
        }
        return true;
    }

    boolean isFreeJ(int col) {
        for (int i = 0; i < pivots.size(); i++) {
            if (pivots.elementAt(i)[1] == col)
                return false;
        }
        return true;
    }

    void backward() {
        backward(b, x);
    }

    void backward(double[] ab, double[] ax) {
        for (int p = pivots.size() - 1; p >= 0; p--) {
            double sum = 0.0;
            int pi = pivots.elementAt(p)[0];
            int pj = pivots.elementAt(p)[1];
            for (int k = pj + 1; k < N; k++) {
                sum = sum + A[P[pi]][k] * ax[k];
            }
            ax[pj] = (ab[P[pi]] - sum) / A[P[pi]][pj];
        }

    }

    public boolean isSolvable() {
        for (int i = 0; i < M; i++) {
            if (isFreeI(i) && (!isZero(b[P[i]])))
                return false;
        }
        return true;
    }

    public void stdout() {
        for (int i = 0; i < M; i++) {
            System.out.print("(" + P[i] + ") ");
            for (int j = 0; j < N; j++) {
                System.out.print(" ");
                System.out.print(A[P[i]][j]);
            }
            System.out.print(" | ");
            System.out.println(b[P[i]]);
        }

        System.out.print("pivots : ");
        for (int i = 0; i < pivots.size(); i++) {
            System.out.print(": (" + P[pivots.elementAt(i)[0]] + ", " + pivots.elementAt(i)[1] + ")");
        }
        System.out.println();

        System.out.print("freeI :");
        for (int i = 0; i < M; i++) {
            if (isFreeI(i))
                System.out.print(", " + P[i]);
        }
        System.out.println();
        System.out.print("freeJ :");
        for (int j = 0; j < N; j++) {
            if (isFreeJ(j))
                System.out.print(", " + j);
        }
        System.out.println();

        System.out.println("Solvable: " + isSolvable());
        if (!isSolvable())
            return;

        System.out.print("x : ");
        for (int j = 0; j < x.length; j++) {
            System.out.print("  " + x[j]);
        }
        System.out.println();

        for (int f = 0; f < xs.length; f++) {
            System.out.print("x" + f + " : ");
            for (int j = 0; j < xs[f].length; j++) {
                System.out.print("  " + xs[f][j]);
            }
            System.out.println();
        }
    }

    public void stdout(String msg) {
        System.out.println(msg);
        stdout();
    }

}
