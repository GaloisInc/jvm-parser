/*
Module           : IVTSuper.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/


public class IVTSuper 
{
    protected int ivar;
    protected IVTSuper() 
    {
        reset();
    }

    public void reset() 
    {
    }

    public int dumpSuper() 
    {
        // System.out.print("IVTSuper dump: ivar = ");
        // System.out.print(ivar);
        // System.out.print("\n");
        return ivar;
    }

    static int f_static()
    {
        return 42;
    }
}

