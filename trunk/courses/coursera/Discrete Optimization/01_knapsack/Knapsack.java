import java.io.PrintWriter;
import java.util.BitSet;

public class Knapsack {

    protected final int capacity;
    protected final Item[] items;

    public Knapsack(int capacity, Item[] items) {
        this.capacity = capacity;
        this.items = items;
    }

    public KnapsackResult solve() {
        // return dynamic();
        return branchAndBound();
    }

    public KnapsackResult dynamic() {
        return new DynamicOptimized(capacity, items).solve();
    }

    public KnapsackResult branchAndBound() {
        return new BranchAndBounds(capacity, items).solve();
    }

    public static class KnapsackResult {
        protected final int value;
        protected final BitSet result;
        private final int size;

        public KnapsackResult(int value, BitSet result, int size) {
            this.value = value;
            this.result = result;
            this.size = size;
        }

        public void printTo(PrintWriter out) {
            out.print(value);
            out.print(' ');
            out.print(1); // indicates that the solution is optimal
            out.print('\n');
            for (int i = 0; i < size; i++) {
                out.print(result.get(i) ? 1 : 0);
                out.print(' ');
            }
            out.flush();
        }

        public int[] getResult() {
            int res[] = new int[size];
            for (int i = 0; i < size; i++) {
                if (result.get(i)) {
                    res[i] = 1;
                }
            }
            return res;
        }
    }

    public static class Item {
        protected final int number;
        protected final int value;
        protected final int weight;

        public Item(int number, int value, int weight) {
            this.number = number;
            this.value = value;
            this.weight = weight;
        }

        public double relativeValue() {
            return value / (double) weight;
        }

        @Override
        public String toString() {
            return "Item [number=" + number + ", value=" + value + ", weight=" + weight + "]";
        }
    }
}
