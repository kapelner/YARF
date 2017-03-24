package YARF;

import java.util.Arrays;

import com.sun.java.swing.plaf.windows.WindowsBorders.ToolBarBorder;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TDoubleHashSet;
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
		public int lowest_cost_split_attribute = Integer.MIN_VALUE; //bad flag!
		public double lowest_cost_split_value = Double.NaN; //bad flag!
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
			if (YARF.DEBUG){node.printNodeDebugInfo("");}
			return; //ditch... because we're done...
		}

		if (YARF.DEBUG){System.out.println("greedy search BEGINNING for node: " + node.stringLocation(true));}
		

		
		//which features can we split on in this node?
		int[] features_to_split_on = selectAttributesToTry(node);
		
		//randomize the order of the features to randomly select ties
		Tools.shuffleArray(features_to_split_on);
		

		if (YARF.DEBUG){System.out.println("features_to_split_on: " + Tools.StringJoin(features_to_split_on));}
		
		//there are two missingness options - check them in random order
		double r = StatToolbox.rand();
		boolean[] trueFalseRandomOrder = {r > 0.5, r <= 0.5};
		
		//set up the data
		SplitDataWrap split_data = new SplitDataWrap(node);
		
		//we will check each attribute
		for (int j : features_to_split_on){	
			tryFeature(node, j, split_data, trueFalseRandomOrder);

		}
		
		

		//if we have been unsuccessful in finding an advantageous split, we should let this node be a leaf and ditch
		if (split_data.lowest_total_split_cost >= node.cost){
			if (YARF.DEBUG){System.out.println("greedy search unsuccessful... for node: " + node.stringLocation(true) + "\n\n");}
			node.is_leaf = true;
			node.assignYHat();
//			if (YARF.DEBUG){node.printNodeDebugInfo("");}
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
		TIntHashSet missing_indices_j = new TIntHashSet(); //order is not necessary here since there is no order
		//FUTURE: ordered_nonmissing_indices_j not used... need to optimize this
		yarf.sortedIndices(j, node.indices, ordered_nonmissing_indices_j, missing_indices_j);
//		if (YARF.DEBUG){System.out.println("node.indices: " + Tools.StringJoin(node.indices));}
//		if (YARF.DEBUG){System.out.println("ordered_nonmissing_indices_j: " + Tools.StringJoin(ordered_nonmissing_indices_j));}
//		if (YARF.DEBUG){System.out.println("missing_indices_j: " + Tools.StringJoin(missing_indices_j));}

		//get all possible values of x_j (not just at this node)
		double[] xj = yarf.getXj(j);
		//now get the possible split values
		double[] split_vals = selectSplitValues(node, j);
		
		if (YARF.DEBUG){
			System.out.println("split_vals: " + Tools.StringJoin(split_vals));
		}

		//iterate over all the split points!
		for (int i = 0; i < split_vals.length; i++){
			trySplitVal(node, split_vals[i], xj, j, missing_indices_j, split_data, trueFalseRandomOrder);
		}
	}


	private void trySplitVal(YARFNode node, double split_value, double[] xj, int j, TIntHashSet missing_indices_j, SplitDataWrap split_data, boolean[] trueFalseRandomOrder) {
		
		for (boolean send_missing_data_right : trueFalseRandomOrder){ //iterate within here over the direction of missingness
			//set up zygotes
			YARFNode putative_left = new YARFNode(node);
			YARFNode putative_right = new YARFNode(node);
			putative_left.indices = new TIntArrayList();
			putative_right.indices = new TIntArrayList();

			//FUTURE: this loop is obviously unneeded for each iteration
			//it is costing about 25% of total runtime
			for (int i0 = 0; i0 < node.nodeSize(); i0++){
				int index = node.indices.get(i0);
				if (xj[index] <= split_value){
					putative_left.indices.add(index);
				}
				else {
					putative_right.indices.add(index);
				}
			}
			if (YARF.DEBUG){System.out.println("left indices: " + Tools.StringJoin(putative_left.indices));}
			if (YARF.DEBUG){System.out.println("left ys: " + Tools.StringJoin(putative_left.node_ys()));}
			if (YARF.DEBUG){System.out.println("right indices: " + Tools.StringJoin(putative_right.indices));}
			if (YARF.DEBUG){System.out.println("right ys: " + Tools.StringJoin(putative_right.node_ys()));}
			
			//putative_left.indices = (TIntArrayList)node.indices.subList(0, i + 1);
			//putative_right.indices = (TIntArrayList)node.indices.subList(i + 1, num_split_points);		
			
			//handle the indices from missingness L/R now
			if (!missing_indices_j.isEmpty()){ 
				if (send_missing_data_right){
					putative_right.indices.addAll(missing_indices_j);
				}
				else {
					putative_left.indices.addAll(missing_indices_j);
				}
			}

			
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


	private double[] selectSplitValues(YARFNode node, int j) {
		if (yarf.customFunctionSplitValues()){
			return yarf.runSplitValues(node, j);
		}
		else {
			//find the unique values at this node
			double[] xj_node_unique = node.uniqueXvals(j);
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
		if (yarf.is_a_regression){ //sum of SSEs
			return putative_left.cost + putative_right.cost; 
		}
		else { //average entropy among the children
			int nL = putative_left.nodeSize();
			int nR = putative_right.nodeSize();
			return (nL * putative_left.cost + nR * putative_right.cost) / (nL + nR);			
		}
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
		} else if (node.nodeSize() < yarf.nodesize){
			return true;
		}
		return false;
	}

	private int[] selectAttributesToTry(YARFNode node) {
		if (yarf.customFunctionMtry()){
			return yarf.runMtry(node);
		}
//		return yarf.indices_zero_to_p_minus_1;
		return StatToolbox.pickNRandomElements(yarf.indices_zero_to_p_minus_1, (yarf.mtry == 0 ? yarf.defaultMtry() : yarf.mtry));
	}

}
