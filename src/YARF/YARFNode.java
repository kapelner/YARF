package YARF;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import OpenSourceExtensions.UnorderedPair;
import gnu.trove.list.array.TIntArrayList;

public class YARFNode implements Cloneable {
	
	/** Setting this to true will print out debug information at the node level during Gibbs sampling */
	public static final boolean DEBUG_NODES = false;
	
	/** a flag that represents an invalid double value */
	protected static final double BAD_FLAG_double = -Double.MAX_VALUE;
	/** a flag that represents an invalid integer value */
	protected static final int BAD_FLAG_int = -Integer.MAX_VALUE;
	/** the parent node of this node */
	protected YARFNode parent;
	/** the left daughter node */
	protected YARFNode left;
	/** the right daughter node */
	protected YARFNode right;
	/** the in-sample cost associated with this node */
	protected double cost = BAD_FLAG_double;
	
	/** the generation of this node from the top node (root note has generation = 0 by definition) */
	public int depth = 0;
	/** is this node a terminal node? */
	public boolean is_leaf = false;
	/** the attribute this node makes a decision on */
	public int split_attribute = BAD_FLAG_int;
	/** the value this node makes a decision on */
	public double split_value = BAD_FLAG_double;
	/** send missing data to the right? */ 
	public boolean send_missing_data_right;
	/** if this is a leaf node, then the result of the prediction for regression, otherwise null */
	public double y_pred = BAD_FLAG_double;
	/** the indices in this node */
	public TIntArrayList indices;
	/** the number of training observations in this node */
	private Integer node_size;
	/** what tree is this node in? */
	private YARFTree tree;

	//convenience functions for looking at the data in this node
	private double[] node_ys;
	private ArrayList<double[]> node_Xs;
	private ArrayList<double[]> node_X_others;

	
	
	/**
	 * Picks a random direction for missing data to flow down the tree from this node. As of
	 * now, this is a 50-50 coin flip left:right.
	 * 
	 * @return	True / false is returned
	 */
	public static boolean pickRandomDirectionForMissingData() {
		return StatToolbox.rand() < 0.5 ? false : true;
	}
	
	public YARFNode(YARFTree tree){
		this.tree = tree;
		indices = tree.bootstrap_indices;
	}	
	
	/**
	 * Creates a new node
	 * 
	 * @param parent		The parent of this node
	 * @param bart		The BART model this node belongs to
	 */
	public YARFNode(YARFNode parent){
		this.parent = parent;
		tree = parent.tree;
		
		if (parent != null){
			depth = parent.depth + 1;
		}
	}
	
	public void assignYHat() {
		//System.out.println("assignYHat");
		if (tree.yarf.customFunctionNodeAssignment()){
			//System.out.println("yarf.customFunctionNodeAssignment");
			y_pred = tree.yarf.runNodeAssignment(this);
		}
		else {
			if (tree.yarf.is_a_regression){ //the default is the sample average
				y_pred = StatToolbox.sample_average(node_ys());
			}
			else { //and for a classification, it's just the modal value among the y's
				y_pred = StatToolbox.sample_mode(node_ys());
			}
		}
	}

	/**
	 * Evaluate a record recursively accounting for split rules and the presence of missing data
	 * 
	 * @param record		The record which to evaluate in this tree
	 * @return				The returned prediction from the terminal node that this tree structure maps the record to
	 */
	public double Evaluate(double[] record) {
		YARFNode evalNode = this;
		while (true){
			if (evalNode.is_leaf){
				return evalNode.y_pred;
			}
			//all split rules are less than or equals (this is merely a convention)
			//handle missing data first
			if (Classifier.isMissing(record[evalNode.split_attribute])){
				evalNode = evalNode.send_missing_data_right ? evalNode.right : evalNode.left;
			}			
			else if (record[evalNode.split_attribute] <= evalNode.split_value){
				evalNode = evalNode.left;
			}
			else {
				evalNode = evalNode.right;
			}
		}
	}
	
	/** Remove all the data in this node and its children recursively to save memory */
	public void flushNodeData() {
		indices = null;
		node_ys = null;
		
		if (this.left != null)
			this.left.flushNodeData();
		if (this.right != null)
			this.right.flushNodeData();
	}
	
	public int nodeSize(){
		if (node_size == null){
			node_size = indices.size();
		}
		return node_size;
	}
	
	/**
	 * How many terminal nodes are below this node?
	 * 
	 * @return	The number of terminal nodes
	 */
	public int numLeaves(){
		if (this.is_leaf){
			return 1;
		}
		else {
			return this.left.numLeaves() + this.right.numLeaves();
		}
	}
	
	/**
	 * Find the total number of nodes (internal and terminal) recursively below this node
	 * 
	 * @return	The number of nodes
	 */
	public int numNodesAndLeaves() {
		if (this.is_leaf){
			return 1;
		}
		else {
			return 1 + this.left.numNodesAndLeaves() + this.right.numNodesAndLeaves();
		}
	}
	/**
	 * In debugging, print a string that codes this node's location in the entire tree
	 * 	
	 * @param show_parent	Show a character if this node is a stump
	 * @return				The coded string
	 */
	public String stringLocation(boolean show_parent) {
		if (this.parent == null){
			return show_parent ? "P" : "";
		}
		else if (this.parent.left == this){
			return this.parent.stringLocation(false) + "L";
		}
		else if (this.parent.right == this){
			return this.parent.stringLocation(false) + "R";
		}
		else {
			return this.parent.stringLocation(false) + "?";
		}
	}
	
//	public void printTree(){
//		printNodeDebugInfo("");
//		if (left != null){
//			left.printTree();
//		}
//		if (right != null){
//			right.printTree();
//		}
//	}
	

	/**
	 * Prints debug information about this node, its parent and its immediate children
	 * 
	 * @param title		A string to print within this message
	 */
	public void printNodeDebugInfo(String title) {		
		System.out.println("\n" + title + " node debug info for " + this.stringLocation(true) + (is_leaf ? " (LEAF) " : " (INTERNAL NODE) ") + " d = " + depth);
		System.out.println("-----------------------------------------");
		System.out.println("n_eta = " + nodeSize());
		System.out.println("y_pred = " + (y_pred == BAD_FLAG_double ? "BLANK" : y_pred));
		System.out.println("cost = " + (cost == BAD_FLAG_double ? "BLANK" : cost));
		System.out.println("parent = " + parent + " this = " + this);
		System.out.println(" left = " + left + " right = " + right);
		
		if (this.parent != null){
			System.out.println("----- PARENT RULE:   X_" + parent.split_attribute + " <= " + parent.split_value + " & M -> " + (parent.send_missing_data_right ? "R" : "L") + " ------");
			//get vals of this x currently here
			double[] x_dot_j = tree.yarf.getXj(parent.split_attribute);
			double[] x_dot_j_node = new double[this.nodeSize()];
			for (int i = 0; i < this.nodeSize(); i++){
				x_dot_j_node[i] = x_dot_j[indices.get(i)];
			}
			//Arrays.sort(x_dot_j_node);
			System.out.println("   all X_" + parent.split_attribute + " values here: [" + Tools.StringJoin(x_dot_j_node) + "]");
		}
		
		if (split_attribute != BAD_FLAG_int){ //rule is unassigned... or it's a leaf...

			
			if (!is_leaf){
				System.out.println("----- RULE:   X_" + split_attribute + " <= " + split_value + " & M -> " + (send_missing_data_right ? "R" : "L") + " ------");
				//get vals of this x currently here
	//			System.out.println("parent: " + parent);
	//			System.out.println("tree: " + tree);
	//			System.out.println("tree.yarf: " + tree.yarf);
				double[] x_dot_j = tree.yarf.getXj(split_attribute);
				double[] x_dot_j_node = new double[nodeSize()];
				for (int i = 0; i < nodeSize(); i++){
					x_dot_j_node[i] = x_dot_j[indices.get(i)];
				}
				//Arrays.sort(x_dot_j_node);
				System.out.println("   all X_" + split_attribute + " values here: [" + Tools.StringJoin(x_dot_j_node) + "]");
			}
		}
		System.out.println("responses: (size " + node_ys().length + ") [" + Tools.StringJoin(node_ys()) + "]" +  " sse = " + StatToolbox.sample_sum_sq_err(node_ys()));
		System.out.println("indicies: (size " + nodeSize() + ") [" + Tools.StringJoin(indices) + "]");
//		if (Arrays.equals(yhats, new double[yhats.length])){
//			System.out.println("y_hat_vec: (size " + yhats.length + ") [ BLANK ]");
//		}
//		else {
//			System.out.println("y_hat_vec: (size " + yhats.length + ") [" + Tools.StringJoin(bart.un_transform_y_and_round(yhats)) + "]");
//		}
		System.out.println("-----------------------------------------\n\n\n");
	}

	public double[] node_ys(){
		if (node_ys == null){
//			System.out.println("tree " + tree);
//			System.out.println("tree.yarf " + tree.yarf);
//			System.out.println("tree.yarf.y " + tree.yarf.y + " indices " + indices);
			node_ys = Tools.subArr(tree.yarf.y, indices);
		}
		return node_ys;
	}
	
	public ArrayList<double[]> node_Xs(){
		if (node_Xs == null){
			node_Xs = new ArrayList<double[]>(nodeSize());
			for (int i = 0; i < nodeSize(); i++){
				node_Xs.add(tree.yarf.X.get(indices.get(i)));
			}
		}
		return node_Xs;
	}	
	
	public ArrayList<double[]> node_X_others(){
		if (node_X_others == null){
			node_X_others = new ArrayList<double[]>(nodeSize());
			for (int i = 0; i < nodeSize(); i++){
				node_X_others.add(tree.yarf.Xother.get(indices.get(i)));
			}
		}
		return node_Xs;
	}

	public int[] attributeSplitCounts() {
		int[] attribute_split_counts = new int[tree.yarf.p];
		attributeSplitCountsInner(attribute_split_counts);
		return attribute_split_counts;
	}
	
	public void attributeSplitCountsInner(int[] counts) {
		if (this.is_leaf){
			return;
		}
		counts[this.split_attribute]++;
		left.attributeSplitCountsInner(counts);
		right.attributeSplitCountsInner(counts);
	}
	
	/**
	 * A wrapper to find all interactions recursively by checking all splits underneath this node
	 *  
	 * @param set_of_interaction_pairs	A running list of interaction pairs
	 */
	public void findInteractions(HashSet<UnorderedPair<Integer>> set_of_interaction_pairs) {		
		if (this.is_leaf){
			return;
		}
		//add all pairs for which this split at this node interacts
		findSplitAttributesUsedUnderneath(this.split_attribute, set_of_interaction_pairs);
		//recurse further to all the children
		this.left.findInteractions(set_of_interaction_pairs);
		this.right.findInteractions(set_of_interaction_pairs);
		
	}

	/**
	 * Finds interactions recursively for one node's attribute by checking all splits underneath this node
	 * 
	 * @param interacted_attribute			The attribute in the top node that is being interacted with split rules in the daughter nodes
	 * @param set_of_interaction_pairs		A running list of interaction pairs
	 */
	private void findSplitAttributesUsedUnderneath(int interacted_attribute, HashSet<UnorderedPair<Integer>> set_of_interaction_pairs) {
		if (this.is_leaf){
			return;
		}
		//add new pair
		if (!this.left.is_leaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.left.split_attribute));
		}
		if (!this.right.is_leaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.right.split_attribute));
		}
		//now recurse
		this.left.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
		this.right.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
	}	
}
