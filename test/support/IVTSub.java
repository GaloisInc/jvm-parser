/*
Module           : IVTSub.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/

public class IVTSub extends IVTSuper
{
    protected IVTSub() 
    {
    }

    public void reset ()
    {
        super.reset();
        ivar = f_static(); // f_static returns 42
    }

    public int dumpSub()
    {
        // System.out.print("IVTSub dump: ivar = ");
        // System.out.print(ivar);
        // System.out.print("\n");
        return ivar;
    }
    
}
