import java.util.Arrays;
import java.util.HashMap;

public class MainJavaArray {

    private static final int windowSize = 200_000;
    private static final int msgCount = 1_000_000;
    private static final int msgSize = 1024;

    private static long worst = 0;

    private static byte[] createMessage(final int n) {
        final byte[] msg = new byte[msgSize];
        Arrays.fill(msg, (byte) n);
        return msg;
    }

    private static void pushMessage(final byte[][] store, final int id) {
        final long start = System.nanoTime();
        store[id % windowSize] = createMessage(id);
        final long elapsed = System.nanoTime() - start;
        if (elapsed > worst) {
            worst = elapsed;
        }
    }

    public static void main(String[] args) {
        final byte[][] store = new byte[windowSize][msgSize];
        for (int i = 0; i < msgCount; i++) {
            pushMessage(store, i);
        }
        System.out.println("Worst push time: " + (worst / 1000_000));
    }
}
