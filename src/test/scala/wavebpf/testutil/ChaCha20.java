// https://github.com/kitsook/ChaCha20

package wavebpf.testutil;

/*
 * Quick-n-dirty standalone implementation of ChaCha 256-bit
 * <p/>
 * Created by Clarence Ho on 20150729
 * <p/>
 * References:
 * ~ http://cr.yp.to/chacha/chacha-20080128.pdf
 * ~ https://tools.ietf.org/html/draft-irtf-cfrg-chacha20-poly1305-01
 * ~ https://github.com/quartzjer/chacha20
 * ~ https://github.com/jotcmd/chacha20
 */
public class ChaCha20 {
    /*
     * Key size in byte
     */
    public static final int KEY_SIZE = 32;

    /*
     * Nonce size in byte (reference implementation)
     */
    public static final int NONCE_SIZE_REF = 8;

    /*
     * Nonce size in byte (IETF draft)
     */
    public static final int NONCE_SIZE_IETF = 12;

    private int[] matrix = new int[16];


    protected static int littleEndianToInt(byte[] bs, int i) {
        return (bs[i] & 0xff) | ((bs[i + 1] & 0xff) << 8) | ((bs[i + 2] & 0xff) << 16) | ((bs[i + 3] & 0xff) << 24);
    }

    protected static void intToLittleEndian(int n, byte[] bs, int off) {
        bs[  off] = (byte)(n       );
        bs[++off] = (byte)(n >>>  8);
        bs[++off] = (byte)(n >>> 16);
        bs[++off] = (byte)(n >>> 24);
    }

    protected static int ROTATE(int v, int c) {
        return (v << c) | (v >>> (32 - c));
    }

    protected static void quarterRound(int[] x, int a, int b, int c, int d) {
        x[a] += x[b];
        x[d] = ROTATE(x[d] ^ x[a], 16);
        x[c] += x[d];
        x[b] = ROTATE(x[b] ^ x[c], 12);
        x[a] += x[b];
        x[d] = ROTATE(x[d] ^ x[a], 8);
        x[c] += x[d];
        x[b] = ROTATE(x[b] ^ x[c], 7);
    }

    public class WrongNonceSizeException extends Exception {
        private static final long serialVersionUID = 2687731889587117531L;
    }

    public class WrongKeySizeException extends Exception {
        private static final long serialVersionUID = -290509589749955895L;
    }


    public ChaCha20(byte[] key, byte[] nonce, int counter)
            throws WrongKeySizeException, WrongNonceSizeException {

        if (key.length != KEY_SIZE) {
            throw new WrongKeySizeException();
        }

        this.matrix[ 0] = 0x61707865;
        this.matrix[ 1] = 0x3320646e;
        this.matrix[ 2] = 0x79622d32;
        this.matrix[ 3] = 0x6b206574;
        this.matrix[ 4] = littleEndianToInt(key, 0);
        this.matrix[ 5] = littleEndianToInt(key, 4);
        this.matrix[ 6] = littleEndianToInt(key, 8);
        this.matrix[ 7] = littleEndianToInt(key, 12);
        this.matrix[ 8] = littleEndianToInt(key, 16);
        this.matrix[ 9] = littleEndianToInt(key, 20);
        this.matrix[10] = littleEndianToInt(key, 24);
        this.matrix[11] = littleEndianToInt(key, 28);

        if (nonce.length == NONCE_SIZE_REF) {        // reference implementation
            this.matrix[12] = 0;
            this.matrix[13] = 0;
            this.matrix[14] = littleEndianToInt(nonce, 0);
            this.matrix[15] = littleEndianToInt(nonce, 4);

        } else if (nonce.length == NONCE_SIZE_IETF) {
            this.matrix[12] = counter;
            this.matrix[13] = littleEndianToInt(nonce, 0);
            this.matrix[14] = littleEndianToInt(nonce, 4);
            this.matrix[15] = littleEndianToInt(nonce, 8);
        } else {
            throw new WrongNonceSizeException();
        }
    }

    public void encrypt(byte[] dst, byte[] src, int len) {
        int[] x = new int[16];
        byte[] output = new byte[64];
        int i, dpos = 0, spos = 0;

        while (len > 0) {
            for (i = 16; i-- > 0; ) x[i] = this.matrix[i];
            for (i = 20; i > 0; i -= 2) {
                quarterRound(x, 0, 4,  8, 12);
                quarterRound(x, 1, 5,  9, 13);
                quarterRound(x, 2, 6, 10, 14);
                quarterRound(x, 3, 7, 11, 15);
                quarterRound(x, 0, 5, 10, 15);
                quarterRound(x, 1, 6, 11, 12);
                quarterRound(x, 2, 7,  8, 13);
                quarterRound(x, 3, 4,  9, 14);
            }
            for (i = 16; i-- > 0; ) x[i] += this.matrix[i];
            for (i = 16; i-- > 0; ) intToLittleEndian(x[i], output, 4 * i);

            // TODO: (1) check block count 32-bit vs 64-bit; (2) java int is signed!
            this.matrix[12] += 1;
            if (this.matrix[12] == 0) {
                this.matrix[13] += 1;
            }
            if (len <= 64) {
                for (i = len; i-- > 0; ) {
                    dst[i + dpos] = (byte) (src[i + spos] ^ output[i]);
                }
                return;
            }
            for (i = 64; i-- > 0; ) {
                dst[i + dpos] = (byte) (src[i + spos] ^ output[i]);
            }
            len -= 64;
            spos += 64;
            dpos += 64;
        }
    }

    public void decrypt(byte[] dst, byte[] src, int len) {
        encrypt(dst, src, len);
    }

}