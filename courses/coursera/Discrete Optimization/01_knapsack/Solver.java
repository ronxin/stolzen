import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Scanner;

public class Solver implements Runnable {

    protected PrintWriter out = new PrintWriter(System.out, true);
    protected Scanner scanner;

    @Override
    public void run() {
        solve().printTo(out);
    }

    public Knapsack.KnapsackResult solve() {
        return readInput().solve();
    }

    public Knapsack readInput() {
        int n = scanner.nextInt();
        int capacity = scanner.nextInt();

        Knapsack.Item[] items = new Knapsack.Item[n];
        int number = 0;
        while (scanner.hasNext()) {
            items[number] = new Knapsack.Item(number, scanner.nextInt(), scanner.nextInt());
            number++;
        }

        return new Knapsack(capacity, items);
    }

    public Solver setInput(InputStream inputStream) {
        this.scanner = new Scanner(inputStream);
        return this;
    }

    public void setOut(PrintWriter out) {
        this.out = out;
    }

    public static void main(String[] args) {
        new Solver().setInput(System.in).run();
    }

}
