public class SC2
{
    public SC2(){

    }
    static void setMulti(double[][] a, double[][] b, double[][] ab) {
        for(int i=0; i<a.length; i++) {
            for(int j=0; j<b[0].length; j++) {
                double sum = 0.0;
                for(int k=0; k<a[0].length; k++) {
                    sum += a[i][k] * b[k][j];
                }
                ab[i][j] = sum;
            }
        }
        return;
    }
    static void setMulti(int[][] a, int[][] b, int[][] ab) {
        for(int i=0; i<a.length; i++) {
            for(int j=0; j<b[0].length; j++) {
                int sum = 0;
                for(int k=0; k<a[0].length; k++) {
                    sum += a[i][k] * b[k][j];
                }
                ab[i][j] = sum;
            }
        }
        return;
    }
    static void stdout(double[][] A) {
        for(int i=0; i<A.length; i++){
            for(int j=0; j<A[0].length; j++) {
                System.out.print(String.format("%10.6f", A[i][j]));
            }
            System.out.println();
        }
    }

    static void stdout(int[][] A) {
        for(int i=0; i<A.length; i++){
            for(int j=0; j<A[0].length; j++) {
                System.out.print(String.format("%5d", A[i][j]));
            }
            System.out.println();
        }
    }
    static void stdout(double[] w) {
        for(int i=0; i<w.length; i++) {
            System.out.println(String.format("%10.6f", w[i]));
        }
    }

    static double[][] dMatrix(double[][] A, double[] w) {
        double[][] d = new double[A.length][A[0].length];
        for(int i=0; i<A.length; i++) {
            for(int j=0; j<A[0].length; j++) {
                d[i][j] = w[i]/w[j] - A[i][j];
            }
        }
        return d;
    }
    static int[][] mMatrix(double[][] d) {
        int[][] M = new int[d.length][d[0].length];
        for(int i=0; i<d.length; i++) {
            for(int j=0; j<d[0].length; j++) {
                if(d[i][j] >=0.0) M[i][j] =1;
                else M[i][j] = 0;
            }
        }
        for(int i=0; i<M.length; i++) M[i][i] = 0;
        return M;
    }
    static void copy(int[][] A, int[][] B) {
        for(int i=0; i<A.length; i++) {
            for(int j=0; j<A[0].length; j++) {
                B[i][j] = A[i][j];
            }
        }

    }
    static void setEigen(double[][] A, double[] w) {
        int itrMax = 1000;
        double[] y = new double[A.length];  // pre
        for(int itr=0; itr<itrMax; itr++) {
            for(int i=0; i<A.length; i++) {
                y[i] = 0;
                for(int j=0; j<A.length; j++) y[i] += A[i][j] * w[j];
            }
            double sum = 0.0;
            for(int i=0; i<A.length; i++) sum += y[i];
            for(int i=0; i<A.length; i++)  w[i] = y[i]/sum;
        }
        double lambda = y[0]/w[0];
        double ci = (lambda-A.length)/(A.length-1);
        System.out.println("                 CI="+ci+",    lambda="+lambda);
    }
    static void setGeom(double[][] A, double[] w) {
        for(int i=0; i<A.length; i++) {
            double ans = 1.0;
            for(int j=0; j<A.length; j++) {
                ans = ans * A[i][j];
            }
            w[i] = Math.pow(ans, 1.0/(double)A.length);
        }
        double sum = 0.0;
        for(int i=0; i<A.length; i++) sum += w[i];
        for(int i=0; i<A.length; i++)  w[i] = w[i]/sum;
    }
    static void accum(int[][] to , int[][] from) {
        for(int i=0; i<to.length; i++) for(int j=0; j<to[0].length; j++) to[i][j] += from[i][j];
    }
    public static void main2(String[] argv) {
        int N=4;
        double[][] A = {{1, 1, 5, 9}
                            ,{1,1,4,7}
                            ,{1,1,1,5}
                            ,{1,1,1,1}
                        };
                        A[0][0] = 1;  A[0][1] = 1; A[0][2] = 4; A[0][3] = 9;
                        A[1][0] = 1;  A[1][1] = 1; A[1][2] = 7; A[1][3] = 5;
                        A[2][0] = 1;  A[2][1] = 2; A[2][2] = 1; A[2][3] = 4;
                        A[3][0] = 1;  A[3][1] = 2; A[3][2] = 2; A[3][3] = 1;
        double[] w = {9,7,5,1};

        for(int i=1; i<N; i++) for(int j=0; j<i; j++) { A[i][j] = 1.0/A[j][i];}
        //for(int i=0; i<w.length; i++) w[i] = 1.0;

        System.out.println("A"); stdout(A);
        setEigen(A, w); System.out.println("w eigen"); stdout(w);
        //setGeom(A, w); System.out.println("w geom"); stdout(w);

        double[][] D = dMatrix(A, w);
        System.out.println("D"); stdout(D);
        int[][] M = mMatrix(D);
        System.out.println("M"); stdout(M);
        int[][] M2 = new int[M.length][M[0].length];
        int[][] Mpre = new int[M.length][M[0].length];
        int[][] Mn = new int[M.length][M[0].length];
        copy(M, Mn);
        copy(M, M2);
        //setMulti(M, M, M2);
        System.out.println("M2"); stdout(M2);
        for(int i=0; i<N; i++) {
            setMulti(M, M2, Mpre);
            copy(Mpre, M2);
            accum(Mn, Mpre);
        }
        System.out.println("Mn"); stdout(Mn);
    }
    public static void main(String[] argv) {
        int N=4;
        double[][] A = {{1, 1, 5, 9}
                            ,{1,1,4,7}
                            ,{1,1,1,5}
                            ,{1,1,1,1}
                        };
                        A[0][0] = 1;  A[0][1] = 1.0/2.0; A[0][2] = 2; A[0][3] = 1.0/2.0;
                        A[1][0] = 1;  A[1][1] = 1; A[1][2] = 2.0; A[1][3] = 2.0;
                        A[2][0] = 1;  A[2][1] = 2; A[2][2] = 1; A[2][3] = 2.0;
                        A[3][0] = 1;  A[3][1] = 2; A[3][2] = 2; A[3][3] = 1;
        double[] w = {9,7,5,1};

        for(int i=1; i<N; i++) for(int j=0; j<i; j++) { A[i][j] = 1.0/A[j][i];}
        //for(int i=0; i<w.length; i++) w[i] = 1.0;

        System.out.println("A"); stdout(A);
        setEigen(A, w); System.out.println("w eigen"); stdout(w);
        //setGeom(A, w); System.out.println("w geom"); stdout(w);

    }
}
