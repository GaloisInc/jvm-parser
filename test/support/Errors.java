/*
Module           : Errors.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/


public class Errors
{
    // allPathsThrowExc
    public static void allPathsThrowExc(boolean b) throws Exception { foo(b, null); }
    public static void foo(boolean b, Object ref)  throws Exception { bar(b, ref); }
    public static void bar(boolean b, Object ref) throws Exception {
        if (b) {
            int x = ref.hashCode();
            System.out.print(x);
        }
        else {
            throw new Exception ("exception");
        }
    }

    public static int[] getArrayRef(int i, int[][] arr)
    {
        return arr[i];
    }

    public static void updArrayRef(int i, int[] r, int[][] arr)
    {
        arr[i] = r;
    }


    public static void main(String[] args) throws Exception
    {
        allPathsThrowExc(false);
    }

    public static void printInt(int x) 
    {
        System.out.print(x);
    }

    public static void printLong(long x) 
    {
        System.out.print(x);
    }

    public static void printInternedStringRef (Object r)
    {
        System.out.print(r);
    }

    public static void printDouble (double x)
    {
        System.out.print(x);
    }
        
}
