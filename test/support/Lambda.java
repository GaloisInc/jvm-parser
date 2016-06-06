class LambdaCalc
{
    interface IntegerMath {
        int operation(int a, int b);
    }

    public int operateBinary(int a, int b, IntegerMath op) {
        return op.operation(a, b);
    }

    public static void main(String... args) {

        LambdaCalc myApp = new LambdaCalc();
        IntegerMath addition = (a, b) -> a + b;
        IntegerMath subtraction = (a, b) -> a - b;
        System.out.println("40 + 2 = " +
            myApp.operateBinary(40, 2, addition));
        System.out.println("20 - 10 = " +
            myApp.operateBinary(20, 10, subtraction));
    }

    public static int add(int a, int b) {
	LambdaCalc calc = new LambdaCalc();
        IntegerMath addition = (x, y) -> x + y;
	return calc.operateBinary(a, b, addition);
    }

    public static int sub(int a, int b) {
	LambdaCalc calc = new LambdaCalc();
        IntegerMath subtraction = (x, y) -> x - y;
	return calc.operateBinary(a, b, subtraction);
    }
}
