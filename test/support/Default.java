interface Default
{
    default int three() {
	System.out.println("three");
	return 3;
    }

    static int four() {
	System.out.println("three");
	return 4;
    }
}
