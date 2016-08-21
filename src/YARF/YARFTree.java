package YARF;

import gnu.trove.set.hash.TIntHashSet;


public class YARFTree extends Classifier {
	
	private YARF yarf;
	private int[] bootstrap_indices;
	private TIntHashSet oob_indices;

	public YARFTree(YARF yarf) {
		this.yarf = yarf;
	}

	public void setBoostrapIndices(int[] bootstrap_indices){
		this.bootstrap_indices = bootstrap_indices;		
	}

	public void Build() {
		// TODO Auto-generated method stub
		
	}

	public void FlushData() {
		// TODO Auto-generated method stub
		
	}

	public void setTrainingIndices(int[] bootstrap_indices) {
		this.bootstrap_indices = bootstrap_indices;
	}

	public void setOutOfBagIndices(TIntHashSet oob_indices) {
		this.oob_indices = oob_indices;
	}

	@Override
	public double Evaluate(double[] record, int num_cores) {
		//we ignore num_cores here
		
		return 0;
	}

	@Override
	public void StopBuilding() {
		// TODO Auto-generated method stub
		
	}

}
