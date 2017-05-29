package YARF;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.hash.TIntDoubleHashMap;
import gnu.trove.set.hash.TIntHashSet;

/** A very simple wrapper class for the root node of the random tree */
public class YARFTree extends Classifier {
	private static final long serialVersionUID = 2834945939944472818L;
	
	protected YARF yarf;
	protected YARFNode root;
	protected TIntArrayList bootstrap_indices;
	protected TIntArrayList other_indices;
	protected TIntHashSet oob_indices;
	protected boolean stop;
	protected boolean completed;
	protected int tree_num;
	protected YARFRandomness r;

	public YARFTree(YARF yarf, int tree_num) {
		this.yarf = yarf;
		this.tree_num = tree_num;
		r = new YARFRandomness();
	}

	public void Build() {
		//System.out.println("BUILD TREE");
		root = new YARFNode(this);
		new YARFTreeBuilder(this);
		//if the user didn't hit the brakes, mark this completed, otherwise no guarantee...
		if (!stop){
			completed = true; //once it's done, ensure the rest of the forest knows about it
			yarf.treeCompletedCallback();
		}
	}

	public void FlushData() {
		root.flushNodeData();
	}

	public void setTrainingIndices(TIntArrayList bootstrap_indices) {
		this.bootstrap_indices = bootstrap_indices;
	}

	public void setOtherIndices(TIntArrayList other_indices) {
		this.other_indices = other_indices;
	}
	
	public void setOutOfBagIndices(TIntHashSet oob_indices) {
		this.oob_indices = oob_indices;
	}
	
	public TIntDoubleHashMap evaluateOutOfBag(){
		TIntDoubleHashMap index_to_y_hat = new TIntDoubleHashMap(oob_indices.size());
		TIntIterator iterator = oob_indices.iterator();
		while (iterator.hasNext()){
			int index = iterator.next();
			index_to_y_hat.put(index, Evaluate(yarf.X.get(index)));
		}
		return index_to_y_hat;
	}
	
	public TIntDoubleHashMap evaluateOtherIndices(){
		if (other_indices == null){
			return null;
		}
		
		TIntDoubleHashMap index_to_y_hat = new TIntDoubleHashMap(other_indices.size());
		TIntIterator iterator = other_indices.iterator();
		while (iterator.hasNext()){
			int index = iterator.next();
			index_to_y_hat.put(index, Evaluate(yarf.X.get(index)));
		}
		return index_to_y_hat;
	}

	public double Evaluate(double[] x_star) {
		return root.Evaluate(x_star);
	}

	public YARFNode predictNode(double[] x_star) {
		return root.predictNode(x_star);
	}

	@Override
	public void StopBuilding() {
		stop = true;
	}
	
	public boolean completed(){
		return completed;
	}

	public int depth() {
		int[] d = {0};
		root.maxDepth(d);
		return d[0];
	}

	public int numLeaves() {
		return root.numLeaves();
	}

	public int numNodes() {
		return root.numNodes();
	}

	public void setSeed(int seed) {
		r.setSeed(seed);
	}

}
