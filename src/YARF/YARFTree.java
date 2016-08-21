package YARF;

import gnu.trove.set.hash.TIntHashSet;


public class YARFTree extends Classifier {
	
	protected YARF yarf;
	protected YARFNode root;
	protected int[] bootstrap_indices;
	protected TIntHashSet oob_indices;

	public YARFTree(YARF yarf) {
		this.yarf = yarf;
	}

	public void setBoostrapIndices(int[] bootstrap_indices){
		this.bootstrap_indices = bootstrap_indices;		
	}

	public void Build() {
		root = new YARFNode(this);
		new YarfTreeBuilder(this);
	}

	public void FlushData() {
		root.flushNodeData();
	}

	public void setTrainingIndices(int[] bootstrap_indices) {
		this.bootstrap_indices = bootstrap_indices;
	}

	public void setOutOfBagIndices(TIntHashSet oob_indices) {
		this.oob_indices = oob_indices;
	}

	@Override
	public double Evaluate(double[] record, int num_cores) {
		return root.Evaluate(record);
	}

	@Override
	public void StopBuilding() {
		// TODO Auto-generated method stub
		
	}

}
