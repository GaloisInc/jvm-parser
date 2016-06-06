/*
Module           : TestMethodSpec.java
Description      :
Stability        : provisional
Point-of-contact : atomb, jhendrix
*/

class TestMethodSpec {

  public static void staticIntArrayCopy(int[] a, int[] b) {
    for (int i = 0; i != b.length; ++i) { a[i] = b[i]; }
  } 

  public static void staticLongArrayCopy(long[] a, long[] b) {
    for (int i = 0; i != b.length; ++i) { a[i] = b[i]; }
  } 

  public static void buggyBitMask(int[] a, int[] b) {
      a[0] = 349872;
      //for (int i = 0; i != b.length; ++i) { a[i] = b[i] | (1 << i); }
  }

  public int[] intArrayF;

  public void copyToIntArrayF(int[] b) {
    staticIntArrayCopy(intArrayF, b);
  }

  public void copyFromIntArrayF(int[] b) {
    staticIntArrayCopy(b, intArrayF);
  }

  public static void straightLineAssertion() {
    int x = 0;
    x++;
    x++;
    // Assert x == 2
    x++;
  }

  public static void loopAssertion() {
    int x = 10;
    int y = 0;
    // Invariant: x + y == 10;
    while(x > 0) {
      x--;
      y++;
    }
  }
}

