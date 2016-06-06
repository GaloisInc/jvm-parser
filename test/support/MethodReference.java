import java.util.function.BiFunction;

class MethodReferenceTest
{
    static int f(int a, int b) {
	return doIt(MethodReferenceTest::add, a, b);
    }

    static int add(int a, int b) {
	return a+b;
    }

    static int doIt(BiFunction<Integer,Integer,Integer> f, int a, int b) {
	return f.apply(a,b);
    }
}
