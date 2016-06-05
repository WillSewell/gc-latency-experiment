import java.util.Arrays;
import java.util.HashMap;

public class Main {

    private static final int windowSize = 200_000;
    private static final int msgCount = 1_000_000;
    private static final int msgSize = 1024;

    private static byte[] createMessage(final int n) {
        final byte[] msg = new byte[msgSize];
        Arrays.fill(msg, (byte) n);
        return msg;
    }

    private static void pushMessage(final HashMap<Integer, byte[]> map, final int id) {
        final int lowId = id - windowSize;
        map.put(id, createMessage(id));
        if (lowId >= 0) {
            map.remove(lowId);
        }
    }

    public static void main(String[] args) {
        final HashMap<Integer, byte[]> map = new HashMap<>();
        for (int i = 0; i < msgCount; i++) {
            pushMessage(map, i);
        }
    }
}
