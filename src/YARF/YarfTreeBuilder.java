package YARF;

import java.util.Arrays;

import org.apache.commons.math3.stat.StatUtils;

import gnu.trove.map.hash.TIntObjectHashMap;

public class YarfTreeBuilder {

	private YARFTree tree;
	private YARF yarf;

	public YarfTreeBuilder(YARFTree tree) {
		this.tree = tree;
		this.yarf = tree.yarf;
		
		splitNode(tree.root);
	}

	private void splitNode(YARFNode node) {
		if (tree.stop){
			return; //user hit the brakes so ditch
		}
		if (nodeTooSmall(node)){
			node.isLeaf = true;
			assignYHat(node);
			return; //ditch... because we're done...
		}
		
		//greedy search
		int n_n = node.nodeSize();
		double lowest_cost = Double.MAX_VALUE;
		
		int lowest_cost_split_attribute = Integer.MIN_VALUE; //bad flag!
		int lowest_cost_split_index = Integer.MIN_VALUE; //bad flag!
		
		int[] features_to_split_on = selectAttributesToTry(node);
		TIntObjectHashMap<int[]> ordered_indices_js = new TIntObjectHashMap<int[]>(features_to_split_on.length);
		for (int j : features_to_split_on){
			System.out.println("features_to_split_on" + Tools.StringJoin(features_to_split_on));
			int[] ordered_indices_j = yarf.sortedIndices(j, node.indices);
			for (int i_cut = 0; i_cut < n_n; i_cut++){
				double cost = computeCostOfSplitPoint(ordered_indices_j, i_cut);
				if (cost < lowest_cost){
					lowest_cost = cost;
					lowest_cost_split_attribute = j;
					lowest_cost_split_index = i_cut;
				}
			}
			ordered_indices_js.put(j, ordered_indices_j); //save it for later
		}
		
		//indicate the splitting rule in this node
		node.splitAttribute = lowest_cost_split_attribute;
		node.splitValue = yarf.getXj(lowest_cost_split_attribute)[lowest_cost_split_index];
		
		//give birth to two children
		YARFNode left = new YARFNode(node);
		YARFNode right = new YARFNode(node);
		int[] ordered_indices_j = ordered_indices_js.get(lowest_cost_split_attribute);
		left.indices = Arrays.copyOfRange(ordered_indices_j, 0, lowest_cost_split_index);
		right.indices = Arrays.copyOfRange(ordered_indices_j, lowest_cost_split_index, n_n);
		//and now split them up
		splitNode(left);
		splitNode(right);
	}

	private void assignYHat(YARFNode node) {
		if (yarf.customFunctionNodeAssignment()){
			//TODO
		}
		//sample average or sample mode
		node.y_pred = yarf.is_a_regression ? StatUtils.mean(node.node_ys()) : StatToolbox.sample_mode(node.node_ys());
	}

	private double computeCostOfSplitPoint(int[] ordered_indices_j, int i_cut) {
		//get the indices in the potential left and right children
		int n =  ordered_indices_j.length;
		int[] left_indices = Arrays.copyOfRange(ordered_indices_j, 0, i_cut);
		int[] right_indices = Arrays.copyOfRange(ordered_indices_j, i_cut, n);		
		int nL = left_indices.length;
		int nR = right_indices.length;
		if (yarf.customFunctionCostCalc()){
			//TODO
			return 0;
		}
		else {
			double [] ysL = Tools.subArr(tree.yarf.y, left_indices);
			double [] ysR = Tools.subArr(tree.yarf.y, right_indices);
			
			if (yarf.is_a_regression){ //the cost is the SSE (across both the left and right)
				return StatToolbox.sample_sum_sq_err(ysL) + StatToolbox.sample_sum_sq_err(ysR);
			}
			else { //it's a classification - the "cost" is the sample weighted average of negative entropy among left and right
				return nL / (double)n * StatToolbox.natural_negative_entropy(ysL) + 
						nR / (double)n * StatToolbox.natural_negative_entropy(ysR);
			}
		}
	}

	private boolean nodeTooSmall(YARFNode node) {
		if (yarf.customFunctionNodesize()){
			//TODO
			return false;
		} else if (node.nodeSize() <= yarf.nodesize){
			return true;
		}
		return false;
	}

	private int[] selectAttributesToTry(YARFNode node) {
		if (yarf.customFunctionMtry()){
			//TODO
		}
		int mtry = Math.max(1, (int)Math.floor(yarf.is_a_regression ? (yarf.p / 3) : Math.sqrt(yarf.p))); //at least it's 1!!!
		return StatToolbox.pickNRandomElements(yarf.indicies_one_to_p_min_1, mtry);
	}

}
