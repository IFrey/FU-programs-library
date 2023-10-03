import java.util.Scanner;
import java.lang.Math;

public class Euklid_gcd {
    public static void main(String[] args){
        // Get the input from user
        Scanner scanner = new Scanner(System.in);

        System.out.print("First number: ");
        int num_one = Math.abs(scanner.nextInt());
        scanner.nextLine();

        System.out.print("Second number: ");
        int num_two = Math.abs(scanner.nextInt());
        scanner.nextLine();

        System.out.print("Return the greatest common divisor (input 1) or represent greatest common divisor as a sum of multiple of given numbers (input 2)? ");
        int operation = scanner.nextInt();
        scanner.nextLine();

        // Reposition numbers so that num_one >= num_two is true for functions
        if (operation == 1){
            if (num_one >= num_two) {
                euklid(num_one, num_two);
            } else {
                euklid(num_two, num_one);
            }
        } else {
            if (num_one >= num_two) {
                euklid_extended(num_one, num_two);
            } else {
                euklid_extended(num_two, num_one);
            }
        }
    }
    // Simple euclidian algorithm for gcd
    public static void euklid(int num_one, int num_two) {
        int c = num_one, temp;
        while (num_two != 0) {
            num_one = num_two;
            temp = num_two;
            num_two = c % num_two;
            c = temp;
        }
        System.out.println(num_one);

    }
    // extended euclidian lagorithm for gcd
    public static void euklid_extended(int a, int b) {
        int x = 0, y = 1, lastx = 1, lasty = 0, temp;

        while (b != 0) {
            // Calculate quotient q and remainder r
            int q = a / b;
            int r = a % b;

            // a = b and b = remainder
            a = b;
            b = r;

            // Calculate and store parameter for first number
            temp = x;
            x = lastx - q * x;
            lastx = temp;

            // Calculate and store parameter for second number
            temp = y;
            y = lasty - q * y;
            lasty = temp;
        }


        System.out.println("GCD " + a + ", x: " + lastx + ", y:" + lasty);
    }
}