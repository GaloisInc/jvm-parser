/*
Module           : Arrays.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/

class Arrays
{
    public static int index(int i, int[] arr) { return arr[i]; }
    public static void update(int i, int v, int[] arr) { arr[i] = v; }
    public static void update(int i, int j, int v, int[][] arr) { arr[i][j] = v; }

    public static void reverse_bytes(byte[] bs) 
    {
        for (int i = 0, j = bs.length - 1; i < j; ++i, --j)
        {
            byte t = bs[i];
            bs[i] = bs[j];
            bs[j] = t;
        }
    }
}
