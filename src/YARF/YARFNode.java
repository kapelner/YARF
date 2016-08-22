package YARF;

import java.util.HashSet;

import OpenSourceExtensions.UnorderedPair;

public class YARFNode implements Cloneable {
	
	/** Setting this to true will print out debug information at the node level during Gibbs sampling */
	public static final boolean DEBUG_NODES = false;
	
	/** a flag that represents an invalid double value */
	protected static final double BAD_FLAG_double = -Double.MAX_VALUE;
	/** a flag that represents an invalid integer value */
	protected static final int BAD_FLAG_int = -Integer.MAX_VALUE;
	/** the parent node of this node */
	private YARFNode parent;
	/** the left daughter node */
	private YARFNode left;
	/** the right daughter node */
	private YARFNode right;
	
	/** the generation of this node from the top node (root note has generation = 0 by definition) */
	public int depth;
	/** is this node a terminal node? */
	public boolean isLeaf;
	/** the attribute this node makes a decision on */
	public int splitAttribute;
	/** the value this node makes a decision on */
	public double splitValue;
	/** send missing data to the right? */ 
	public boolean sendMissingDataRight;
	/** if this is a leaf node, then the result of the prediction for regression, otherwise null */
	public double y_pred = BAD_FLAG_double;
	/** the indices in this node */
	public int[] indices;

	private YARFTree tree;

	private double[] node_ys;
	
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
		
		if (parent != null){
			depth = parent.depth + 1;
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
			if (evalNode.isLeaf){
				return evalNode.y_pred;
			}
			//all split rules are less than or equals (this is merely a convention)
			//it's a convention that makes sense - if X_.j is binary, and the split values can only be 0/1
			//then it MUST be <= so both values can be considered
			//handle missing data first
			if (Classifier.isMissing(record[evalNode.splitAttribute])){
				evalNode = evalNode.sendMissingDataRight ? evalNode.right : evalNode.left;
			}			
			else if (record[evalNode.splitAttribute] <= evalNode.splitValue){
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
		return indices.length;
	}
	
	/**
	 * How many terminal nodes are below this node?
	 * 
	 * @return	The number of terminal nodes
	 */
	public int numLeaves(){
		if (this.isLeaf){
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
		if (this.isLeaf){
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
	
	public double[] node_ys(){
		if (node_ys == null){
			node_ys = Tools.subArr(tree.yarf.y, indices);
		}
		return node_ys;
	}

	public int[] attributeSplitCounts() {
		int[] attribute_split_counts = new int[tree.yarf.p];
		attributeSplitCountsInner(attribute_split_counts);
		return attribute_split_counts;
	}
	
	public void attributeSplitCountsInner(int[] counts) {
		if (this.isLeaf){
			return;
		}
		counts[this.splitAttribute]++;
		left.attributeSplitCountsInner(counts);
		right.attributeSplitCountsInner(counts);
	}
	
	/**
	 * A wrapper to find all interactions recursively by checking all splits underneath this node
	 *  
	 * @param set_of_interaction_pairs	A running list of interaction pairs
	 */
	public void findInteractions(HashSet<UnorderedPair<Integer>> set_of_interaction_pairs) {		
		if (this.isLeaf){
			return;
		}
		//add all pairs for which this split at this node interacts
		findSplitAttributesUsedUnderneath(this.splitAttribute, set_of_interaction_pairs);
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
		if (this.isLeaf){
			return;
		}
		//add new pair
		if (!this.left.isLeaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.left.splitAttribute));
		}
		if (!this.right.isLeaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.right.splitAttribute));
		}
		//now recurse
		this.left.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
		this.right.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
	}	
}
