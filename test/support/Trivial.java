/*
Module           : Trivial.java
Description      : Simple tests for symbolic simulation
Stability        : provisional
Point-of-contact : jstanley
*/

class Trivial
{
    public static boolean bool_f1 (boolean a, boolean b)
    {
        return (a & b);
    }

    public static int int_f2 (int a, int b)
    {
        return (a + b);
    }

    public static int int_f2_temp(int a, int b)
    {
        int rslt = 0;
        if (a > 42) {
            rslt = (a + b);
        } else {
            rslt = (a + b + 1);
        }
        return (rslt + 99);
    }

    public static byte byte_array_f3(byte[] xs)
    {
        byte sum = 0;
        for (int i = 0; i < xs.length; ++i)
            sum += xs[i];
        return sum;
    }

    public static int int_f4(int a, int b)
    {
        return (a - b);
    }

    public static int int_f5(int a, int b)
    {
        return (a / b);
    }

    public static int int_f6(int a, int b)
    {
        return (a % b);
    }

    public static long long_f1 (long a, long b)
    {
        return (a & b);
    }

    public static long long_f2 (long a, long b)
    {
        return (a + b);
    }

    public static double double_f1(double a, double b)
    {
        return (a + b + 3.0);
    }

    public static double double_f2(double a, double b)
    {
        return (a - b);
    }

    public static double double_f3(double a, double b)
    {
        return (a * b);
    }

    public static double double_f4(double a, double b)
    {
        return (a / b);
    }

    public static float float_f1(float a, float b)
    {
        return (a + b + 3.0f);
    }

    public static float float_f2(float a, float b)
    {
        return (a - b);
    }

    public static float float_f3(float a, float b)
    {
        return (a * b);
    }

    public static float float_f4(float a, float b)
    {
        return (a / b);
    }
    public static long long_array_f3 (long[] ls)
    {
        long sum = 0;
        for (int i = 0; i < ls.length; ++i)
            sum += ls[i];
        return sum;
    }

    public static long long_f5(long a, long b)
    {
        return (a / b);
    }

    public static long long_f6(long a, long b)
    {
        return (a % b);
    }

    public static long long_array_idx_f7(int idx)
    {
        int sz = 20;
        long[] arr= new long[sz];
        for (int i = 0; i < sz; ++i)
            arr[i] = 42;
        return arr[idx % 20];
    }

    public static int fork_f1(boolean b)
    {
        if (b)
            return 1;
        else
            return 0;
    }

    public static int fork_loop_f1(boolean b1)
    {
        int rslt = 0;
        for (int i = 0; i < 4; ++i)
            rslt += fork_f1(b1);
        return rslt;
    }

    public static int fork_f2(boolean b1, boolean b2 )
    {
        if (b1)
            if (b2) {
                return 3;
            } else {
                return 1;
            }
        else
            if (b2) {
                return 2;
            } else
                return 0;
    }

    public static int fork_loop_f2(boolean b1, boolean b2)
    {
        int rslt = 0;
        for (int i = 0; i < 2; ++i)
            rslt += fork_f2(b1,b2);
        return rslt;
    }

    public static void out_array(int a, int b, int[] arr)
    {
        arr[0] = a/b;
        arr[1] = a*b;
        arr[2] = a/b;
        arr[3] = (arr[0]+arr[1]+arr[2]) / 9;
    }

    public static int loop1(int k)
    {
        int rslt = 0;
        for (int i = 0; i < k; ++i)
            rslt++;
        return rslt;
    }

    public static void always_throws() throws Exception
    {
        throw new Exception("blah");
    }

    public static boolean stringCheck(String s, int expLen)
    {
        return s.length() == expLen;
    }
}
