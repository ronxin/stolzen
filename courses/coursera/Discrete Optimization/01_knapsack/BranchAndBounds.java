import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Iterator;

public class BranchAndBounds {

    private final Knapsack.Item[] items;
    private final int capacity;
    private final int size;

    private double bestBound;

    public BranchAndBounds(int capacity, Knapsack.Item[] items) {
        this.capacity = capacity;
        this.items = preSort(items);
        this.size = items.length;
    }

    public Knapsack.KnapsackResult solve() {
        Cons list = Cons.from(items);
        bestBound = 0.0;

        double bound = calcBound(list, capacity, 0.0);
        Cons res = dfs(list.head, list.tail, 0, capacity, bound, null);

        int value = 0;
        BitSet backtrack = new BitSet(size);
        while (res != null) {
            Knapsack.Item item = res.head;
            value = value + item.value;
            backtrack.set(item.number);
            res = res.tail;
        }

        return new Knapsack.KnapsackResult(value, backtrack, size);
    }

    public Cons dfs(Knapsack.Item item, Cons tail, int value, int weight, double bound, Cons taken) {
        if (bound < bestBound) {
            return null;
        }
        if (tail == null) {
            bestBound = bound;
            return taken;
        }
        if (item.weight > weight) {
            return notTaking(tail, value, weight, bound, taken);
        }

        // taking the item
        Cons left = dfs(tail.head, tail.tail, value + item.value, weight - item.weight, bound, cons(item, taken));

        // not taking the item
        Cons right = notTaking(tail, value, weight, bound, taken);

        if (left == null) {
            return right;
        }
        return left;
    }

    private Cons notTaking(Cons tail, int value, int weight, double bound, Cons taken) {
        Cons without;
        if (taken != null) {
            Cons reverse = taken.reverse();
            without = reverse.append(tail);
        } else {
            without = tail;
        }
        double newBound = bound + calcBound(without, weight, 0.0);
        if (newBound <= bestBound) {
            return null;
        }
        Cons right = dfs(tail.head, tail.tail, value, weight, newBound, taken);
        return right;
    }

    private double calcBound(Cons list, int k, double acc) {
        if (list == null) {
            return acc;
        }

        Knapsack.Item head = list.head;
        if (head.weight <= k) {
            return calcBound(list.tail, k - head.weight, acc + head.value);
        } else {
            return acc + head.relativeValue() * k;
        }
    }

    private static Cons cons(Knapsack.Item head, Cons tail) {
        return new Cons(head, tail);
    }

    private static Knapsack.Item[] preSort(Knapsack.Item[] items) {
        Knapsack.Item[] clone = items.clone();
        Arrays.sort(clone, new Comparator<Knapsack.Item>() {
            @Override
            public int compare(Knapsack.Item o1, Knapsack.Item o2) {
                double relativeValue1 = o1.relativeValue();
                double relativeValue2 = o2.relativeValue();
                return Double.compare(relativeValue1, relativeValue2);
            }
        });
        return clone;
    }

    private static class Cons {
        final Knapsack.Item head;
        final Cons tail;

        public Cons(Knapsack.Item head, Cons tail) {
            if (head == null) {
                throw new NullPointerException();
            }
            this.head = head;
            this.tail = tail;
        }

        public static Cons from(Knapsack.Item... items) {
            return from(Arrays.asList(items).iterator());
        }

        private static Cons from(Iterator<Knapsack.Item> items) {
            if (items.hasNext()) {
                return new Cons(items.next(), from(items));
            } else {
                return null;
            }
        }

        public Cons reverse() {
            return reverse(this, null);
        }

        private static Cons reverse(Cons list, Cons acc) {
            if (list == null) {
                return acc;
            } else {
                return reverse(list.tail, cons(list.head, acc));
            }
        }

        public Cons append(Cons list2) {
            return append(this, list2);
        }

        private static Cons append(Cons list1, Cons list2) {
            if (list1 == null) {
                return list2;
            }
            return cons(list1.head, append(list1.tail, list2));
        }

        @Override
        public String toString() {
            return "[" + join(", ") + "]";
        }

        public String join(String joiner) {
            if (tail == null) {
                return head.toString();
            }

            return head.toString() + joiner + tail.join(joiner);
        }
    }

}
