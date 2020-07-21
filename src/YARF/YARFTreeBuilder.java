package YARF;

import java.util.*;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;

public class YARFTreeBuilder {

	private YARFTree tree;
	private YARF yarf;

	public YARFTreeBuilder(YARFTree tree) {
		this.tree = tree;
		this.yarf = tree.yarf;
		//the root node has a overall cost which we need to compute
		computeNodeCost(tree.root);
		//technically the root node was just birthed, so if there's a custom function,
		//we should run it now
		if (yarf.customFunctionAfterBirth()){
			yarf.runAfterNodeBirth(tree.root);
		}
		if (YARF.DEBUG){System.out.println("BEGIN YARF TREE BUILDING ON A MODEL " + (yarf.is_a_regression ? "regression" : "classification"));}
		//now we start building this tree via recursive splitting
		splitNode(tree.root);
	}
	//this will house the optimal split
	private class SplitDataWrap {
		
		public YARFNode lowest_left_node = null;
		public YARFNode lowest_right_node = null;
		//greedy search... set up the horses
		public double lowest_total_split_cost;
		public int lowest_cost_split_attribute = YARFNode.BAD_FLAG_int; //bad flag!
		public Double lowest_cost_split_value = Double.NaN; //bad flag!
		public boolean lowest_send_missing_data_right = false; //not necessary but for completeness...
		
		public SplitDataWrap(YARFNode node){
			lowest_total_split_cost = node.cost; //if you can't beat this... don't bother!!
		}
	}
	
	private void splitNode(YARFNode node) {
		if (tree.stop){
			return; //user hit the brakes so ditch
		}
		//if this node is too small (or whatever other reason), ditch from splitting and make it into a leaf
		if (makeNodeLeaf(node)){
			node.is_leaf = true;
			node.assignYHat();
			if (YARF.DEBUG){
				node.printNodeDebugInfo("");
			}
			return; //ditch... because we're done...
		}

		if (YARF.DEBUG){
			System.out.println("greedy search BEGINNING for node: " + node.stringLocation(true) +
					" trying to beat cost " + node.cost);
		}

		//which features can we split on in this node?
		int[] features_to_split_on = selectAttributesToTry(node);
//		if (tree.tree_num == 0){
//			System.out.println(" features_to_split_on: " + Tools.StringJoin(features_to_split_on));
//		}
		
		//randomize the order of the features to randomly select ties
		tree.r.shuffleArray(features_to_split_on);
//		if (tree.tree_num == 0){
//			System.out.println("features_to_split_on after shuffle: " + Tools.StringJoin(features_to_split_on));
//		}		

		if (YARF.DEBUG){System.out.println("features_to_split_on: " + Tools.StringJoin(features_to_split_on));}
		
		//there are two missingness options - check them in random order
		double r = tree.r.rand();
		boolean[] trueFalseRandomOrder = {r > 0.5, r <= 0.5};
		
		//set up the data
		SplitDataWrap split_data = new SplitDataWrap(node);
		
		//we will check each attribute
		for (int j : features_to_split_on){	
			tryFeature(node, j, split_data, trueFalseRandomOrder);
		}
		
		//if we have been unsuccessful in finding an advantageous split, we should let this node be a leaf and ditch
		if (split_data.lowest_total_split_cost >= node.cost){
			if (YARF.DEBUG) {
				System.out.println("greedy search unsuccessful... for node: " + node.stringLocation(true) +
						"  with lowest_total_split_cost " + split_data.lowest_total_split_cost + " and node cost " +
						node.cost + "\n\n");
			}
			node.is_leaf = true;
			node.assignYHat();
			if (YARF.DEBUG){node.printNodeDebugInfo("");}
			return;
		}
		
		if (YARF.DEBUG){System.out.println("greedy search successful!! for node: " + node.stringLocation(true)
				+ "\n previous cost: " + node.cost 
				+ "\n new cost: " + split_data.lowest_total_split_cost
				+ "\n lowest_cost_split: X_" + split_data.lowest_cost_split_attribute + " <= " + split_data.lowest_cost_split_value
				+ "\n lowest_left_node: " + split_data.lowest_left_node
				+ "\n lowest_right_node: " + split_data.lowest_right_node
				+ "\n send_missing: " + (split_data.lowest_send_missing_data_right ? "R" : "L")
				);}
		
		//otherwise we use the optimal split
		//first indicate the splitting rule in this node converting the two lucky zygotes into a fetus
		node.split_attribute = split_data.lowest_cost_split_attribute;
//		if (node.split_attribute == YARFNode.BAD_FLAG_int) {
//			System.out.println("split attr bad, lowest_total_split_cost: " + split_data.lowest_total_split_cost + " node.cost: " + node.cost);
//			Integer n = null;
//			node.split_attribute = n;
//		}
//		System.out.println("  splitValue " + yarf.getXj(lowest_cost_split_attribute)[lowest_cost_split_index]);
		node.split_value = split_data.lowest_cost_split_value;
		node.send_missing_data_right = split_data.lowest_send_missing_data_right;
		
		//then officially give birth to two children from the two fetuses
		node.left = split_data.lowest_left_node;
		node.right = split_data.lowest_right_node;
		if (yarf.customFunctionAfterBirth()){
			yarf.runAfterNodeBirth(node.left);
			yarf.runAfterNodeBirth(node.right);
		}
		
//		if (YARF.DEBUG){node.printNodeDebugInfo("");}
		
		//and now recurse and split on the new children just created
		splitNode(node.left);
		splitNode(node.right);
	}


	private void tryFeature(YARFNode node, int j, SplitDataWrap split_data, boolean[] trueFalseRandomOrder) {
		//first get the indices for this note sorted on attribute j and get the missings as well
		TIntArrayList ordered_nonmissing_indices_j = new TIntArrayList();
		TIntArrayList missing_indices_j = new TIntArrayList(); //order is not necessary here since there is no order
		//FUTURE: ordered_nonmissing_indices_j not used... need to optimize this
		yarf.sortedIndices(j, node.indices, ordered_nonmissing_indices_j, missing_indices_j);
//		if (YARF.DEBUG){System.out.println("node.indices: " + Tools.StringJoin(node.indices));}
//		if (YARF.DEBUG){System.out.println("ordered_nonmissing_indices_j: " + Tools.StringJoin(ordered_nonmissing_indices_j));}
//		if (YARF.DEBUG){System.out.println("missing_indices_j: " + Tools.StringJoin(missing_indices_j));}

		//get all possible values of x_j (not just at this node)
		double[] xj = yarf.getXj(j);
		//now get the possible split values
		//find the unique values at this node
		double[] xj_node_unique = node.uniqueXvals(j);
		if (xj_node_unique.length == 0 && !yarf.customFunctionSplitValues()) {
			return;
		}
		double[] split_vals = selectSplitValues(node, xj_node_unique, j);
		
		if (YARF.DEBUG){
			System.out.println("split_vals: " + Tools.StringJoin(split_vals));
		}

		//iterate over all the split points!
		for (int i = 0; i < split_vals.length; i++){
			trySplitVal(node, split_vals[i], xj, j, ordered_nonmissing_indices_j, missing_indices_j, split_data, trueFalseRandomOrder);
		}
		trySplitVal(node, null, xj, j, ordered_nonmissing_indices_j, missing_indices_j, split_data, trueFalseRandomOrder);
	}


	private void trySplitVal(YARFNode node, Double split_value, double[] xj, int j, TIntArrayList ordered_nonmissing_indices_j, TIntArrayList missing_indices_j, SplitDataWrap split_data, boolean[] trueFalseRandomOrder) {

		for (boolean send_missing_data_right : trueFalseRandomOrder){ //iterate within here over the direction of missingness
			//set up zygotes
			YARFNode putative_left = new YARFNode(node);
			YARFNode putative_right = new YARFNode(node);
			putative_left.indices = new TIntArrayList();
			putative_right.indices = new TIntArrayList();

			//FUTURE: this loop is obviously unneeded for each iteration
			//it is costing about 25% of total runtime
			if (split_value == null) { // dealing with non-missing indices only
				putative_left.indices.addAll(ordered_nonmissing_indices_j);
			}
			else { // dealing with non-missing indices only
				for (int index : ordered_nonmissing_indices_j.toArray()) {
					if (xj[index] <= split_value) {
						putative_left.indices.add(index);
					} else {
						putative_right.indices.add(index);
					}
				}

			}
			if (!missing_indices_j.isEmpty()){ // handle the indices from missingness L/R now
				if (send_missing_data_right){
					putative_right.indices.addAll(missing_indices_j);
				}
				else {
					putative_left.indices.addAll(missing_indices_j);
				}
			}
			//putative_left.indices = (TIntArrayList)node.indices.subList(0, i + 1);
			//putative_right.indices = (TIntArrayList)node.indices.subList(i + 1, num_split_points);


			if (putative_left.indices.isEmpty() || putative_right.indices.isEmpty()) return;

			if (YARF.DEBUG) {
				System.out.println("parent indices: " + Tools.sortAndJoin(node.indices));
				System.out.println("ordered_nonmissing: " + Tools.sortAndJoin(ordered_nonmissing_indices_j));
				System.out.println("missing: " + Tools.sortAndJoin(missing_indices_j.toArray()));
				System.out.println("left indices: " + Tools.sortAndJoin(putative_left.indices));
				System.out.println("left ys: " + Tools.StringJoin(putative_left.node_ys()));
				System.out.println("left size: " + putative_left.nodeSize());
				System.out.println("right indices: " + Tools.sortAndJoin(putative_right.indices));
				System.out.println("right ys: " + Tools.StringJoin(putative_right.node_ys()));
				System.out.println("right size: " + putative_right.nodeSize());
				System.out.println("parent size: " + node.nodeSize());
			}
//			if (putative_left.nodeSize() + putative_right.nodeSize() != node.nodeSize()) {
//				System.out.println("node size");
//				Integer q = null;
//				int i = q;
//			}
			//these are now viable splits, so we compute cost on each node and the overall cost of the split
			computeNodeCost(putative_left);
			computeNodeCost(putative_right);			
			double total_split_cost = totalChildrenCost(putative_left, putative_right);			
			if (YARF.DEBUG){System.out.println("   viable split cost = " + total_split_cost + " at split X_" + (j + 1) + " <= " + split_value + " (L_cost = " + putative_left.cost + ", R_cost = " + putative_right.cost + ")\n");}
			
			//System.out.println("total_split_cost: " + total_split_cost);
			if (total_split_cost < split_data.lowest_total_split_cost){
				if (YARF.DEBUG){System.out.println("beat with cost: " + total_split_cost + " < " + split_data.lowest_total_split_cost + " using split X_" + (j + 1) + " <= " + split_value +"\n\n");}
				split_data.lowest_total_split_cost = total_split_cost;
				split_data.lowest_cost_split_attribute = j;
				split_data.lowest_cost_split_value = split_value;
				split_data.lowest_left_node = putative_left;
				split_data.lowest_right_node = putative_right;
				split_data.lowest_send_missing_data_right = send_missing_data_right;
			}

			//(this enforces randomness of L/R missingness sending)
			if (missing_indices_j.isEmpty()){break;} //no need to check the other because it will be the same 
		}
	}


	private double[] selectSplitValues(YARFNode node, double[] xj_node_unique, int j) {
		if (yarf.customFunctionSplitValues()){
			return yarf.runSplitValues(node, j);
		}
		else {
			//sort them
			Arrays.sort(xj_node_unique);
			//now get their midpoints
			return Tools.midpointed(xj_node_unique);
		}
	}

	private double totalChildrenCost(YARFNode putative_left, YARFNode putative_right) {
		if (yarf.customFunctionBothChildrenCostCalc()){
			return yarf.runBothChildrenCost(putative_left, putative_right);
		}
		int nL = putative_left.nodeSize();
		int nR = putative_right.nodeSize();
		return (nL * putative_left.cost + nR * putative_right.cost) / (nL + nR);
	}

	private void computeNodeCost(YARFNode node) {
		if (yarf.customFunctionSingleNodeCostCalc()){
			node.cost = yarf.runSingleNodeCost(node);
		}
		else {
			//FUTURE: this call from scratch is unneeded. It is costing a big %age of runtime
			double[] ys = node.node_ys();
			if (yarf.is_a_regression){ //the cost is the SSE (across both the left and right)
				node.cost = StatToolbox.sample_sum_sq_err(ys, StatToolbox.sample_average(ys));
			}
			else { //it's a classification - the "cost" is Gini
				node.cost = StatToolbox.gini_split(ys);
//				node.cost = StatToolbox.natural_negative_entropy(ys); ////maybe use Gini instead???
			}
		}
		//System.out.println("computeNodeCost node " + node + " cost = " + node.cost + " pred = " + node.y_pred + " size = " + node.nodeSize());
	}

	private boolean makeNodeLeaf(YARFNode node) {
//		System.out.println("makeNodeLeaf nodesize = " + node.nodeSize() + " yarfnodesize = " + yarf.nodesize);
	
		if (yarf.customFunctionMakeNodeIntoLeaf()){
			return yarf.runNodesizeLegal(node);
		//the default is if it's less than the minimum node size as specified by the user at build time
		} else return node.nodeSize() < yarf.nodesize;
	}

	private int[] selectAttributesToTry(YARFNode node) {
		if (yarf.customFunctionMtry()){
			return yarf.runMtry(node);
		}
		if (YARF.DEBUG) {
			System.out.println("p: " + yarf.p);
			System.out.println("default m_try: " + yarf.defaultMtry());
		}
		int[] indices_zero_to_p_minus_1 = new int[yarf.p];
		for (int j = 0; j < yarf.p; j++){
			indices_zero_to_p_minus_1[j] = j;
		}		
		return tree.r.pickNRandomElements(indices_zero_to_p_minus_1, (yarf.mtry == 0 ? yarf.defaultMtry() : yarf.mtry));
	}

}
