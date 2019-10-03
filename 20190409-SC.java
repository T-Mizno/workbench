public class SC
{
        public static void stdout(String msg, int[][] A)
        {
            System.out.println(msg);
            for(int i=0; i<A.length; i++){
                for(int j=0; j<A[0].length; j++){
                    System.out.print("  "+A[i][j]);
                }
                System.out.println();
            }
        }
        public static void stdoutC(String msg, int[][] A)
        {
            System.out.println(msg);
            for(int i=0; i<A.length; i++){
                for(int j=0; j<i; j++) System.out.print("    ");
                for(int j=i; j<A[0].length; j++){
                    System.out.print(" "); if(i==j)System.out.print(" +-"); else if(A[i][j] > 0) System.out.print("---"); else if(A[i][j]<0) System.out.print("-|-"); else System.out.print("-O-");
                }
                System.out.println();
            }
        }
        public static void stdout(String msg, int[] x)
        {
            System.out.print(msg+ " : ");
            for(int i=0; i<x.length; i++){
                    System.out.print("  "+x[i]);
            }
            System.out.println();
        }
        public static void stdoutState(String msg, int[] ns, int[][] d, int[][] s) {
            System.out.println(msg);
            stdout("SC: input", s);
            stdout("SC: result", ns);
            stdout("SC: dependency", d);
            stdoutC("SC: graph", s);

        }
    public static boolean isSC(int[][] s, int[] ns, int [][] d, int n) {
        boolean isConnect = false;

        for(int i=0; i<n; i++) for(int j=0; j<=i; j++) s[i][j] = 0;
        for(int i=0; i<n; i++) ns[i] = s[0][i] * -1;

        for(int pivot=0; pivot<n; pivot++) {
                //for(int i=pivot; i<n; i++) ns[i] = (ns[i] * ns[i]) * s[pivot][i] * -1;
                for(int i=pivot; i<n; i++) ns[i] = s[pivot][i] * -1;
                for(int i=0; i<n; i++) for(int j=0; j<n; j++) d[i][j] = j;
                for(int i=pivot+1; i<n; i++) {
                    for(int j=i+1; j<n; j++) {
                        //stdoutState("start"+i+":" + j+": ", ns, d, s);
                        if(ns[i] == 0) {
                            if(s[i][j] == 0) ns[j] = 0;
                            else if(ns[j] == s[i][j]) ns[j] = 0;
                        }
                        else {
                            if(ns[j]==0) {
                                if(s[i][j]==0) ns[i]=0;
                                else if(ns[i] != s[i][j]) ns[i]=0;
                            }
                            else if(ns[i] == ns[j]) {
                                if(ns[i] != s[i][j]) d[i][j] = i;
                                if(ns[i] == s[i][j]) d[j][i] = j;
                            }
                            else if(ns[i] != ns[j]) {
                                if(ns[i] != s[i][j]) { ns[i] =0; ns[j] = 0; }
                            }
                        }
                    }
                }

            while(true) {
                boolean existChange = false;
                for(int i=pivot+1; i<n; i++) {
                    if(ns[i] ==0) for(int j=0; j<n; j++) {
                        if(ns[d[j][i]] != 0) {
                            ns[d[j][i]] = 0;
                            existChange = true || existChange;
                        }
                    }
                }
                if(! existChange) break;
            }

            isConnect = true;
            for(int i=pivot+1; i<n; i++) if(ns[i] != 0) isConnect = isConnect && false;
            if(isConnect) ns[pivot] = 0;
            else ns[pivot] = 3;

            //stdoutState("result", ns, d, s);

                break;
            }

        return isConnect;
    }
    public static void main(String[] argv) {
        int N = 6;
        int[][] A6 = {{0, -1, -1, -1, 1, -1}
                    ,{  0, 0, -1, -1, -1, -1}
                    ,{  0, 0, 0, 1, 1, -1}
                    ,{ 0, 3, 4, 5,  1, 1}
                    ,{ 0, 0, 0, 0, 0,  1}
                    ,{ 1, 1, 1, 1, 1, -1}};
        N = 4;
        int[][] A4 = {{0, -1, -1, 0}
                        ,{1, 0, 0, 1}
                        ,{1, 0, 0, 1}
                        ,{0, -1, -1, 0}};
        int[][] A;
        A = A4;
        int[] ns = new int[N];
        int[][] d = new int[N][N];
        System.out.println(isSC(A, ns, d, N));
        stdoutState("test", ns, d, A);
    }
}
