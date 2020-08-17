package YARF;

import java.util.*;

import gnu.trove.list.array.TIntArrayList;

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
		
		public YARFNode bestLeftNode = null;
		public YARFNode bestRightNode = null;
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
				+ "\n bestLeftNode: " + split_data.bestLeftNode
				+ "\n bestRightNode: " + split_data.bestRightNode
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
		node.left = split_data.bestLeftNode;
		node.right = split_data.bestRightNode;
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
		TIntArrayList missing_indices_j = new TIntArrayList(); // inherently unordered
		yarf.sortedIndices(j, node.indices, ordered_nonmissing_indices_j, missing_indices_j);

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

	/**
	 * Computes the quantile of a potential split value (0 to 1) at a node amongst all non-missing elements at that node.
	 * @param splitValue the value to split on
	 * @param xj the entire feature column for feature j
	 * @param jOrderedNonmissingIndices
	 * @return
	 */
	private double computeSplitValQuantile(double splitValue, double[] xj, TIntArrayList jOrderedNonmissingIndices) {

		int countBelowSplitVal = 0;
		int countAboveSplitVal = 0;
		for (int index : jOrderedNonmissingIndices.toArray()) {
			if (xj[index] <= splitValue) {
				countBelowSplitVal++;
			} else {
				countAboveSplitVal++;
			}
		}
		return countBelowSplitVal/((double) (countBelowSplitVal + countAboveSplitVal));
	}

	/**
	 * Tries a split value and stores info for that value in the given SplitDataWrap object.
	 * @param node
	 * @param splitVal the value to split on
	 * @param xj the entire feature column for feature j
	 * @param j the column index of the feature
	 * @param jOrderedNonmissingIndices
	 * @param jMissingIndices
	 * @param splitData
	 * @param trueFalseRandomOrder
	 */
	private void trySplitVal(YARFNode node, Double splitVal, double[] xj, int j,
							 TIntArrayList jOrderedNonmissingIndices, TIntArrayList jMissingIndices,
							 SplitDataWrap splitData, boolean[] trueFalseRandomOrder) {

		if (splitVal == null) {
			trueFalseRandomOrder = new boolean[]{true}; // for the missing/non-missing split, always send missing right
		}

		for (boolean sendMissingRight : trueFalseRandomOrder){ //iterate within here over the direction of missingness
			//set up zygotes
			YARFNode putativeLeft = new YARFNode(node);
			YARFNode putativeRight = new YARFNode(node);
			putativeLeft.indices = new TIntArrayList();
			putativeRight.indices = new TIntArrayList();

			if (splitVal == null) { // split 'C' in Twala et. al. (2008) - missing vs. non-missing
				putativeLeft.indices.addAll(jOrderedNonmissingIndices);
				putativeRight.indices.addAll(jMissingIndices);
			}
			else {
				// could turn into binary search
				for (int index : jOrderedNonmissingIndices.toArray()) {
					(xj[index] <= splitVal ? putativeLeft : putativeRight).indices.add(index);
				}
                if (!jMissingIndices.isEmpty()) { // handle the indices from missingness L/R now
					(sendMissingRight ? putativeRight : putativeLeft).indices.addAll(jMissingIndices);
                }
			}

			if (putativeLeft.indices.isEmpty() || putativeRight.indices.isEmpty()) return;

			if (YARF.DEBUG) {
				System.out.println("parent indices: " + Tools.sortAndJoin(node.indices));
				System.out.println("ordered_nonmissing: " + Tools.sortAndJoin(jOrderedNonmissingIndices));
				System.out.println("missing: " + Tools.sortAndJoin(jMissingIndices.toArray()));
				System.out.println("left indices: " + Tools.sortAndJoin(putativeLeft.indices));
				System.out.println("left ys: " + Tools.StringJoin(putativeLeft.node_ys()));
				System.out.println("left size: " + putativeLeft.nodeSize());
				System.out.println("right indices: " + Tools.sortAndJoin(putativeRight.indices));
				System.out.println("right ys: " + Tools.StringJoin(putativeRight.node_ys()));
				System.out.println("right size: " + putativeRight.nodeSize());
				System.out.println("parent size: " + node.nodeSize());
			}

			//these are now viable splits, so we compute cost on each node and the overall cost of the split
			computeNodeCost(putativeLeft);
			computeNodeCost(putativeRight);
			double total_split_cost = totalChildrenCost(putativeLeft, putativeRight);
			if (YARF.DEBUG){
				System.out.println("   viable split cost = " + total_split_cost +
						" at split X_" + (j + 1)
						+ " <= " + splitVal
						+ " (L_cost = "
						+ putativeLeft.cost
						+ ", R_cost = "
						+ putativeRight.cost + ")\n");
			}

			if (total_split_cost < splitData.lowest_total_split_cost){
				if (YARF.DEBUG) {
					System.out.println("beat with cost: " + total_split_cost
							+ " < " + splitData.lowest_total_split_cost
							+ " using split X_" + (j + 1) + " <= "
							+ splitVal +"\n\n");
				}
				splitData.lowest_total_split_cost = total_split_cost;
				splitData.lowest_cost_split_attribute = j;
				splitData.lowest_cost_split_value = splitVal;
				splitData.bestLeftNode = putativeLeft;
				splitData.bestRightNode = putativeRight;

				// if no missings here and the user has specified as such, decide which way missings go as weighted coin toss
//				if (yarf.noMissingSplitRule == YARF.NO_MISSING_SPLIT_RULE.CONDITIONAL_ON_QUANTILE && jMissingIndices.isEmpty()) {
//					double splitQuantile = computeSplitValQuantile(splitVal, xj, jOrderedNonmissingIndices);
//					sendMissingRight = tree.r.rand() > splitQuantile;
//					if (YARF.DEBUG) {
//						System.out.println(
//								String.format("No missing data at this node. Quantile: %d. Sending %s",
//								splitQuantile, sendMissingRight ? "RIGHT" : "LEFT"));
//					}
//				}
				splitData.lowest_send_missing_data_right = sendMissingRight;
			}

			//(this enforces randomness of L/R missingness sending)
			if (jMissingIndices.isEmpty()) {
				break; //no need to check the other because it will be the same
			}
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
			System.out.println("p: " + yarf.p + " m_try: " + yarf.mtry);
		}
		int[] indices_zero_to_p_minus_1 = new int[yarf.p];
		for (int j = 0; j < yarf.p; j++){
			indices_zero_to_p_minus_1[j] = j;
		}		
		return tree.r.pickNRandomElements(indices_zero_to_p_minus_1, yarf.mtry);
	}

}
