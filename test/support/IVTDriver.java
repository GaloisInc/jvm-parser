/*
Module           : IVTDriver.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/

class IVTDriver
{
    public static void go(int[] rslt)
    {
        IVTSub x   = new IVTSub();
        rslt[0]    = x.dumpSuper();
        rslt[1]    = x.dumpSub();
    }

    public static void main(String [] args)
    {
        int[] rslt = new int[2];
        go (rslt);
    }
}
