import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State{
	private AtomicIntegerArray value;
    private byte maxval;

    private int[] Byte2Int(byte[] raw) {
    		int[] v = new int[raw.length];
    		for (int i = 0; i < raw.length; i++){
                v[i] = raw[i];
            }
		return v;
    }
    
    GetNSet(byte[] v) {
    		value = new AtomicIntegerArray(this.Byte2Int(v)); 
    		maxval = 127; 
    	}

	GetNSet(byte[] v, byte m) { 
		value = new AtomicIntegerArray(this.Byte2Int(v)); 
    		maxval = m; 
    	}

    public int size() { return value.length(); }

    private byte[] Int2Byte(AtomicIntegerArray raw) {
		byte[] v = new byte[raw.length()];
		for (int i = 0; i < raw.length(); i++){
            v[i] = (byte) raw.get(i);
        }
		return v;
    }
    public byte[] current() { return this.Int2Byte(value); }

    public boolean swap(int i, int j) {
		if (value.get(i) <= 0 || value.get(j) >= maxval) {
		    return false;
		}
		value.set(i, value.get(i)-1);
		value.set(j, value.get(j)+1);
		return true;
    }
}
