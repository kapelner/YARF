package YARF;

import java.util.Arrays;

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
			return; //user hit the brakes
		}
		if (nodeTooSmall(node)){
			node.isLeaf = true;
			return; //ditch... because we're done...
		}
		
		//greedy search
		int n_n = node.nodeSize();
		double lowest_cost = Double.MAX_VALUE;
		
		int lowest_cost_split_attribute = Integer.MIN_VALUE; //bad flag!
		double lowest_cost_split_point = Double.NaN; //bad flag!
		int lowest_cost_split_index = Integer.MIN_VALUE; //bad flag!
		
		int[] features_to_split_on = selectAttributesToTry(node);
		TIntObjectHashMap<int[]> ordered_indices_js = new TIntObjectHashMap<int[]>(features_to_split_on.length);
		for (int j : features_to_split_on){
			int[] ordered_indices_j = yarf.sortedIndices(j, node.indices);
			for (int i_n = 0; i_n < n_n; i_n++){
				double cost = computeCostOfSplitPoint(ordered_indices_j, i_n);
				if (cost < lowest_cost){
					lowest_cost = cost;
					lowest_cost_split_attribute = j;
					lowest_cost_split_point = yarf.getXj(j)[i_n];
					lowest_cost_split_index = i_n;
				}
			}
			ordered_indices_js.put(j, ordered_indices_j); //save it for later
		}
		
		//indicate the splitting rule in this node
		node.splitAttribute = lowest_cost_split_attribute;
		node.splitValue = lowest_cost_split_point;
		
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

	private double computeCostOfSplitPoint(int[] ordered_indices_j, int i_n) {
		if (yarf.customFunctionCostCalc()){
			//TODO
		}
		else if (yarf.is_a_regression){ //the cost is the SSE (across both the left and right)
			
		}
		else { //it's a classification
			//
		}
		return 0;
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
		int mtry = (int)Math.floor(yarf.is_a_regression ? (yarf.p / 3) : Math.sqrt(yarf.p));
		//http://lemire.me/blog/2013/08/16/picking-n-distinct-numbers-at-random-how-to-do-it-fast/
		return StatToolbox.pickNRandomElements(node.indices, mtry);
	}

}
