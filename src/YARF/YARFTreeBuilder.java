package YARF;

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
		
		//greedy search... set up the horses
		double lowest_total_split_cost = node.cost; //if you can't beat this... don't bother!!
		int lowest_cost_split_attribute = Integer.MIN_VALUE; //bad flag!
		double lowest_cost_split_value = Double.NaN; //bad flag!
		boolean lowest_send_missing_data_right = false; //not necessary but for completeness...
		
		//which features can we split on in this node?
		int[] features_to_split_on = selectAttributesToTry(node);
		
		
		//this will house the optimal split
		YARFNode lowest_left_node = null;
		YARFNode lowest_right_node = null;
		if (YARF.DEBUG){System.out.println("features_to_split_on: " + Tools.StringJoin(features_to_split_on));}
//		System.out.println("n_n: " + n_n);
		
		//two options about missingness (a) there is no missingness in this feature (b) there is
		
		//there are two missingness options - check them in random order
		double r = StatToolbox.rand();
		boolean[] trueFalseRandomOrder = {r > 0.5, r <= 0.5};
		
		//we will check each attribute
		for (int j : features_to_split_on){	
			//first get the indices for this note sorted on attribute j and get the missings as well
			TIntArrayList ordered_nonmissing_indices_j = new TIntArrayList();
			TIntHashSet missing_indices_j = new TIntHashSet(); //order is not necessary here since there is no order
			yarf.sortedIndices(j, node.indices, ordered_nonmissing_indices_j, missing_indices_j);
//			if (YARF.DEBUG){System.out.println("node.indices: " + Tools.StringJoin(node.indices));}
//			if (YARF.DEBUG){System.out.println("ordered_nonmissing_indices_j: " + Tools.StringJoin(ordered_nonmissing_indices_j));}
//			if (YARF.DEBUG){System.out.println("missing_indices_j: " + Tools.StringJoin(missing_indices_j));}

			int node_size = node.nodeSize();

			double[] xj = yarf.getXj(j);
			
			//get unique values
			double[] xj_unique_sorted_midpoints = Tools.sorted_and_midpointed(Tools.unique_values(xj));
			//get midpoints between unique values
			
//			if (YARF.DEBUG){System.out.println("xj: " + Tools.StringJoin(xj));}

			split_value_search : for (int i = 0; i < xj_unique_sorted_midpoints.length; i++){
				for (boolean send_missing_data_right : trueFalseRandomOrder){
				//iterate over all the cut points!
					double split_value = xj_unique_sorted_midpoints[i];

					//set up zygotes
					YARFNode putative_left = new YARFNode(node);
					YARFNode putative_right = new YARFNode(node);
					putative_left.indices = new TIntArrayList();
					putative_right.indices = new TIntArrayList();

					for (int i0 = 0; i0 < node_size; i0++){
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
//					if (!missing_indices_j.isEmpty()){ 
//						if (send_missing_data_right){
//							putative_right.indices.addAll(missing_indices_j);
//						}
//						else {
//							putative_left.indices.addAll(missing_indices_j);
//						}
//					}

					//we should ditch if these don't work out
//					if (makeNodeLeaf(putative_left) || makeNodeLeaf(putative_right)){
//						System.out.println(" !! makeNodeLeaf(putative_left) " + makeNodeLeaf(putative_left)
//								+ " || makeNodeLeaf(putative_right) " + makeNodeLeaf(putative_right));
						
//						if (missing_indices_j.isEmpty()){ //for efficiency only!!
//							continue split_value_search;
//						} 
//						else {
//							continue;
//						}
						
//					}
					
					//these are now viable splits, so we compute cost

					if (YARF.DEBUG){System.out.print("putative_left");}
					computeNodeCost(putative_left);
					if (YARF.DEBUG){System.out.print("putative_right");}
					computeNodeCost(putative_right);
					
					
					double total_split_cost = totalChildrenCost(putative_left, putative_right);
					
					if (YARF.DEBUG){System.out.println("   viable split cost = " + total_split_cost + " at split X_" + j + " <= " + split_value + " (L_cost = " + putative_left.cost + ", R_cost = " + putative_right.cost + ")\n");}
					
					if (node.stringLocation(false).equals("RLLL")){
						System.out.println("   viable split cost = " + total_split_cost + " at split X_" + j + " <= " + split_value + " (L_cost = " + putative_left.cost + ", R_cost = " + putative_right.cost + ")\n");
					}
					
					//System.out.println("total_split_cost: " + total_split_cost);
					if (total_split_cost < lowest_total_split_cost){
						if (YARF.DEBUG){System.out.println("beat with cost: " + total_split_cost + "<" + lowest_total_split_cost + " using split X_" + j + " <= " + split_value +"\n\n");}
						if (node.stringLocation(false).equals("RLLL")){
							System.out.println("beat with cost: " + total_split_cost + "<" + lowest_total_split_cost + " using split X_" + j + " <= " + split_value +"\n\n");
						}
						lowest_total_split_cost = total_split_cost;
						lowest_cost_split_attribute = j;
						lowest_cost_split_value = split_value;
						lowest_left_node = putative_left;
						lowest_right_node = putative_right;
						lowest_send_missing_data_right = send_missing_data_right;
					}

					//(this enforces randomness of L/R missingness sending)
					if (missing_indices_j.isEmpty()){break;} //no need to check the other because it will be the same 
				}
				
			}
		}
		
		

		//if we have been unsuccessful in finding an advantageous split, we should let this node be a leaf and ditch
		if (lowest_total_split_cost >= node.cost){
			if (YARF.DEBUG){System.out.println("greedy search unsuccessful... for node: " + node.stringLocation(true) + "\n\n");}
			node.is_leaf = true;
			node.assignYHat();
//			if (YARF.DEBUG){node.printNodeDebugInfo("");}
			return;
		}
		
		if (YARF.DEBUG){System.out.println("greedy search successful!! for node: " + node.stringLocation(true)
				+ "\n previous cost: " + node.cost 
				+ "\n new cost: " + lowest_total_split_cost
				+ "\n lowest_cost_split: X_" + lowest_cost_split_attribute + " <= " + lowest_cost_split_value
				+ "\n lowest_left_node: " + lowest_left_node
				+ "\n lowest_right_node: " + lowest_right_node
				+ "\n send_missing: " + (lowest_send_missing_data_right ? "R" : "L")
				);}
		
		//otherwise we use the optimal split
		//first indicate the splitting rule in this node converting the two lucky zygotes into a fetus
		node.split_attribute = lowest_cost_split_attribute;
//		System.out.println("  splitValue " + yarf.getXj(lowest_cost_split_attribute)[lowest_cost_split_index]);
		node.split_value = lowest_cost_split_value;
		node.send_missing_data_right = lowest_send_missing_data_right;
		
		//then officially give birth to two children from the two fetuses
		node.left = lowest_left_node;
		node.right = lowest_right_node;
		if (yarf.customFunctionAfterBirth()){
			yarf.runAfterNodeBirth(node.left);
			yarf.runAfterNodeBirth(node.right);
		}
		
//		if (YARF.DEBUG){node.printNodeDebugInfo("");}
		
		//and now recurse and split on the new children just created
		splitNode(node.left);
		splitNode(node.right);
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
			double[] ys = node.node_ys();
			if (yarf.is_a_regression){ //the cost is the SSE (across both the left and right)
				node.cost = StatToolbox.sample_sum_sq_err(ys, StatToolbox.sample_average(ys));
			}
			else { //it's a classification - the "cost" is the negative entropy which is the negative of the gain...
				node.cost = StatToolbox.natural_negative_entropy(ys); ////maybe use Gini instead???
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
		return yarf.indices_zero_to_p_minus_1;
//		return StatToolbox.pickNRandomElements(yarf.indices_zero_to_p_minus_1, yarf.defaultMtry());
	}

}
