package YARF;

import gnu.trove.list.array.TDoubleArrayList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.set.hash.TDoubleHashSet;
import gnu.trove.set.hash.TIntHashSet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.math3.stat.StatUtils;

import OpenSourceExtensions.UnorderedPair;


/**
 * Builds a YARF model in parallel
 * 
 * @author Adam Kapelner
 */
public class YARF extends YARFCustomFunctions implements Serializable {
	private static final long serialVersionUID = -6984205353140981153L;

	/** debug mode -- prints lots of messages that are useful */
	public static final boolean DEBUG = false;
	
	/** the number of CPU cores to use in YARF operations */
//	protected int num_cores;
	/** the number of trees in this YARF model */
	protected int num_trees;
	/** is this a regression problem? (or a classification problem) */
	protected boolean is_a_regression;
	/** time of completion */
	protected long tf;
	/** is the model stopped by the user? */
	private boolean stopped;
	
	/** locks on the sorters */
	private transient Object[] sorter_locks;
	
	/** an array of the raw training data by COLUMN i.e. consisting of xj = [x1j, ..., xnj] with the last entry being [y1, ..., yn] */ 
	private transient TIntObjectHashMap<double[]> X_by_col;

	/** other data which may be useful for custom functions */
	protected transient ArrayList<double[]> Xother;
	/** feature names in other data */
	protected String[] other_data_names;

	private YARFTree[] yarf_trees;
	private transient int[][] bootstrap_indices;
	private transient int[][] other_indices;

	private Integer default_mtry;	

	private Double null_model_cost;

	//convenient pre-computed data to have around
//	protected transient TIntObjectHashMap<int[]> all_attribute_sorts;
	private transient TIntHashSet indices_one_to_n;
//	protected transient int[] indices_zero_to_p_minus_1;
	
	
	
	//if we use RF algorithm defaults, here they are
	protected int mtry;
	protected int nodesize;

	/** should we hang the system until the model is fully constructed? */
	private boolean wait;
	/** an object that isolates randomness in the algorithm for debugging purposes */
	private YARFRandomness r;
	/** the random seed */
	private Integer seed;
	/** should we iteratively look at oob? */
	private boolean iteratively_calc_oob;
	/** should we build until convergence? */
	private boolean stop_at_convergence;
	/** if we are building until convergence, what tolerance do we stop at? */
	private double tolerance;

	private TDoubleArrayList oob_cost_by_iteration;
	private TDoubleArrayList oob_costs_changes;
	/** If the user wishes to wait until convergence, this records that it converged */
	private boolean converged;

	private ExecutorService yarf_tree_grow_threadpool;



	public static void main(String[] args){
		YARF yarf = new YARF();
		yarf.setWait(true);
		yarf.setNumTrees(1);
		yarf.setNodesize(2);
		yarf.setPredType("classification");
		yarf.setNode_assignment_function_str("" 
				+ "function assignYhatToNode(node){"
				+ "  var ys = Java.from(node.node_ys());"
				+ "  var avg = 0;"
				+ "  for (i = 0; i < ys.length; i++){"
				+ "    avg += ys[i];"
				+ "  }"
				+ "  return sample_avg(ys);"
				+ "}");
		yarf.setShared_scripts_str(""
				+ "function sample_avg(arr){print('in sample avg');"
				+ "	var sum = 0.0;"
				+ "	for (i = 0; i < arr.length; i++){"
				+ " 	sum += arr[i];"
				+ " }"
				+ "	return sum / arr.length;"
				+ "}");
		
//		yarf.cost_single_node_calc_function_str = ""
//				+ "function nodeCost(node){"
//				+ "  node.assignYHat();"
//				+ "	var ys = Java.from(node.node_ys());"
//				+ "  print('ys l'); print(ys.length); print(ys);"
//				+ "  print('y_pred'); print(node.y_pred);"
//				+ "	var sae = 0.0;"
//				+ "	for (i = 0; i < ys.length; i++){"
//				+ "		sae += Math.abs(ys[i] - node.y_pred);"
//				+ "	}"
//				+ "return sae;"
//				+ "}";
		
		
		int n = 30;
		int p = 5;
		int[] indices_t = new int[n];
		yarf.X = new ArrayList<double[]>(n);
		yarf.y = new double[n];
		for (int i = 0; i < n; i++){
			indices_t[i] = i;
			double[] x_i = new double[p];
			for (int j = 0; j < p; j++){
				x_i[j] = Math.random();
				if (x_i[j] < 0.1){
					x_i[j] = MISSING_VALUE;
				}
			}
			yarf.X.add(x_i);
			yarf.y[i] = Math.random();
		}
		
//		int n = 15;
//		int[] indices_t = new int[n];
//		//int[] indices_t = {1,1,2,3,3,5,7,7,8,9};
//		yarf.X = new ArrayList<double[]>(n);
//		yarf.y = new double[n];
//		for (int i = 0; i < n; i++){
//			indices_t[i] = i;
//			double[] x_i = {i};
//			if (i == 1 || i == 2 || i == 8){
//				x_i[0] = Classifier.MISSING_VALUE;
//			}
//			yarf.X.add(x_i);
//			yarf.y[i] = i < 9 ? 0 : 1;
//		}
		//yarf.y[9] = -100;

		yarf.finalizeTrainingData();
		yarf.addBootstrapIndices(indices_t, 0);
		yarf.Build();
		
	}
	
	public YARF(){
		r = new YARFRandomness();
	}
	
	/**
	 * Adds an observation / record to the "other" data array. The
	 * observation is converted to doubles and the entries that are 
	 * unrecognized are converted to {@link #MISSING_VALUE}'s.
	 * 
	 * @param x_i	The observation / record to be added as a String array.
	 */
	public void addOtherDataRow(String[] x_i){
		//initialize data matrix if it hasn't been initialized already
		if (Xother == null){
			Xother = new ArrayList<double[]>();
		}
		
		//now add the new record
		final double[] record = new double[x_i.length];
		for (int i = 0; i < x_i.length; i++){
			try {
				record[i] = Double.parseDouble(x_i[i]);
			}
			catch (NumberFormatException e){
				record[i] = MISSING_VALUE;
//				System.out.println("missing value at record #" + X_y.size() + " attribute #" + i);
			}
		}				
		Xother.add(record);		
	}
	
	public void setOtherDataNames(String[] other_data_names){
		this.other_data_names = other_data_names;
	}
	
	public void setNumTrees(int num_trees){
		this.num_trees = num_trees;
	}
	
	public void setPredType(String pred_type){
		if (pred_type.equals("regression")){
			is_a_regression = true;
		}
	}
	
	public void setMTry(int mtry){
		this.mtry = mtry;
	}
	
	public void setNodesize(int nodesize){
		this.nodesize = nodesize;
	}
	
	
	public int progress(){
		int progress = 0;
		for (YARFTree tree : yarf_trees){
			progress += (tree.completed ? 1 : 0);
		}
		return progress;
	}
	
	public void setBootstrapAndOutOfBagIndices(int t){
		//System.out.println("setBootstrapAndOutOfBagIndices t = " + t);
		//make a copy
		yarf_trees[t].setTrainingIndices(new TIntArrayList(bootstrap_indices[t]));
		if (other_indices[t] != null){ //the other indices is a optional setting
			yarf_trees[t].setOtherIndices(new TIntArrayList(other_indices[t]));
		}
		//now get oob indices - it begins as the full thing then we subtract 
		//out the bootstrap indices of the tree and the "other" indices (if they exist)
		TIntHashSet oob_indices = new TIntHashSet(indices_one_to_n);
		oob_indices.removeAll(bootstrap_indices[t]);
		if (other_indices[t] != null){ //the other indices is a optional setting
			oob_indices.removeAll(other_indices[t]);
		}
		yarf_trees[t].setOutOfBagIndices(oob_indices);
	}
	
	public double[] predictOutOfBag(int num_cores){
		//first get the trees that have this observation out of bag
		final HashMap<Integer, ArrayList<Integer>> index_to_oob_on_trees = new HashMap<Integer, ArrayList<Integer>>(n);
		for (int i = 0; i < n; i++){
			ArrayList<Integer> trees_oob = new ArrayList<Integer>();
			for (int t = 0; t < num_trees; t++){
				//if the tree is done being built and it has this observation oob, collect it
				if (yarf_trees[t].completed && yarf_trees[t].oob_indices.contains(i)){
					trees_oob.add(t);
				}
			}
			index_to_oob_on_trees.put(i, trees_oob);
		}	
		
		final double[] y_hat_oobs = new double[n];
		
		ExecutorService evaluator_pool = Executors.newFixedThreadPool(num_cores);
		for (int i = 0; i < n; i++){
			final int i_f = i;
	    	evaluator_pool.execute(new Runnable(){
				public void run() {
					ArrayList<Integer> trees_oob = index_to_oob_on_trees.get(i_f);
					if (trees_oob.isEmpty()){
						y_hat_oobs[i_f] = Double.NaN; //no information for the user...
					}
					else {
						double[] y_preds_trees_oob_only = new double[trees_oob.size()];
						double[] x_i = X.get(i_f);
						for (int t = 0; t < trees_oob.size(); t++){
							y_preds_trees_oob_only[t] = yarf_trees[trees_oob.get(t)].Evaluate(x_i);
						}
						y_hat_oobs[i_f] = nodeAssignmentAggregation(y_preds_trees_oob_only);
					}
				}
			});
		}
		evaluator_pool.shutdown();
		try {
			evaluator_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS); //infinity
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		return y_hat_oobs;
	}

	/** This function builds the forest by building all the trees */
	public void Build(int num_cores) {

		//System.err.println("inside YARF");
//		all_attribute_sorts = new TIntObjectHashMap<int[]>(p);
		
		yarf_trees = new YARFTree[num_trees];
		final YARF yarf = this;
		for (int t = 0; t < num_trees; t++){ //they all must be initialized first before any are built
			yarf_trees[t] = new YARFTree(yarf, t);
			setBootstrapAndOutOfBagIndices(t);
		}
		
		oob_cost_by_iteration = new TDoubleArrayList();
		oob_costs_changes = new TDoubleArrayList();
		//run a build on all threads
		long t0 = System.currentTimeMillis();
		//System.err.println("inside 2");
		yarf_tree_grow_threadpool = Executors.newFixedThreadPool(num_cores);
		for (int t = 0; t < num_trees; t++){
			final int tf = t;
	    	yarf_tree_grow_threadpool.execute(new Runnable(){
				public void run() {	
					if (stopped){ //user hit the brakes or the model converged, so ditch
						return;
					}
					if (seed != null){ //i.e. someone set the seed
						yarf_trees[tf].setSeed(seed);
					}
					if (DEBUG){
						System.out.println("now building tree " + (tf + 1) + "/" + num_trees + " in the YARF model... bootstrap indicies: " + 
							Tools.StringJoin(yarf_trees[tf].bootstrap_indices));
					}
					yarf_trees[tf].Build();
				}
			});
		}
		yarf_tree_grow_threadpool.shutdown();
		Thread await_completion = new Thread(){
			public void run(){
				try {
					yarf_tree_grow_threadpool.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS); //infinity
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				tf = System.currentTimeMillis();
			}
		};
		await_completion.start();
		//only halt if user wishes
		if (wait){
			try {
				await_completion.join();
			} catch (InterruptedException e) {}
		}

		
		if (DEBUG){
			System.out.println("done building YARF in " + ((System.currentTimeMillis() - t0) / 1000.0) + " sec \n");
		}
	}


	/**
	 * Return the number of times each of the attributes were used during the construction of the trees
	 * 
	 * @param type	Either "splits" or "trees" ("splits" means total number and "trees" means sum of binary values of whether or not it has appeared in the tree)
	 * @return		The counts for all Gibbs samples further indexed by the attribute 1, ..., p
	 */
	public int[] getCountsForAllAttribute(final String type) {	
		int[] variable_counts = new int[p];
		
		for (YARFTree tree : yarf_trees){
			if (tree.completed){
				if (type.equals("splits")){
					variable_counts = Tools.add_arrays(variable_counts, tree.root.attributeSplitCounts());
				}
				else if (type.equals("trees")){
					variable_counts = Tools.binary_add_arrays(variable_counts, tree.root.attributeSplitCounts());
				}				
			}
		}		
		return variable_counts;
	}
	
	public int[][] getInteractionCounts(){
		int[][] interaction_count_matrix = new int[p][p];		
			
		for (YARFTree tree : yarf_trees){
			if (tree.completed){
				//get the set of pairs of interactions
				HashSet<UnorderedPair<Integer>> set_of_interaction_pairs = new HashSet<UnorderedPair<Integer>>(p * p);
				//find all interactions
				tree.root.findInteractions(set_of_interaction_pairs);
				//now tabulate these interactions in our count matrix
				for (UnorderedPair<Integer> pair : set_of_interaction_pairs){
					interaction_count_matrix[pair.getFirst()][pair.getSecond()]++; 
				}
			}
		}
	
		return interaction_count_matrix;
	}
	
	public void pruneTree(int t){
		if (yarf_trees[t].completed){
			yarf_trees[t].root.prune();
		}
	}
	
	public void illustrateTree(int t, 
			int max_depth,
			int[] background_color, 
			int[] line_color, 
			int[] text_color, 
			String font_family,
			int font_size, 
			int margin_in_px, 
			double character_width_in_px,
			int length_in_px_per_half_split,
			int depth_in_px_per_split,
			String file_format,
			boolean use_real_names,
			String title){
		
		YARFTree tree = yarf_trees[t];
		if (tree.completed){
//			System.out.println("illustrateTree " + tree);
			new YARFTreeIllustrate(this,
					tree.root, 
					max_depth,
					background_color, 
					line_color,  
					text_color, 
					font_family,
					font_size, 
					margin_in_px, 
					character_width_in_px, 
					length_in_px_per_half_split, 
					depth_in_px_per_split,
					file_format,
					use_real_names,
					title);
		}
		else {
			System.err.println("Tree #" + t + " not completed yet.");
		}
	}

	/** Flush all unnecessary data from the Gibbs chains to conserve RAM */
	protected void FlushData() {
		for (int t = 0; t < num_trees; t++){
			yarf_trees[t].FlushData();
		}
	}
	
	
	/**
	 * After the classifier has been built, new records can be evaluated / predicted
	 * (implemented by a daughter class)
	 * 
	 * @param records					A n* x p matrix of n* observations to be evaluated / predicted.
	 * @param num_cores_evaluate		The number of processor cores to be used during the evaluation / prediction
	 * @return							The predictions
	 */
	public double[] Evaluate(double[][] records, int num_cores_evaluate){
		int n_star = records.length;
//		System.out.println("Evaluate nstar: " + n_star);
		final double[] y_hats = new double[n_star];
		
		//speedup for the dumb user
		if (num_cores_evaluate == 1){			
			for (int i = 0; i < n_star; i++){
				y_hats[i] = Evaluate(records[i]);
			}
			return y_hats;
		}
		
		ExecutorService tree_eval_pool = Executors.newFixedThreadPool(num_cores_evaluate);
		for (int i = 0; i < n_star; i++){
			final int i_f = i;
			tree_eval_pool.execute(new Runnable(){
				public void run() {
					try {
						y_hats[i_f] = Evaluate(records[i_f]);
					} catch (ArrayIndexOutOfBoundsException e){
						tree_eval_pool.shutdownNow();
						e.printStackTrace();
					}
				}
			});
		}
		tree_eval_pool.shutdown();
		try {	         
			tree_eval_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS); //effectively infinity
	    } catch (InterruptedException ignored){}
		
		return y_hats;	
	}
	
	/**
	 * After the classifier has been built, we can return the leaf nodes where records end up
	 * 
	 * @param records					A n* x p matrix of n* observations to be evaluated / predicted.
	 * @param num_cores_evaluate		The number of processor cores to be used during the evaluation / prediction
	 * @return							The predictions
	 */
	public YARFNode[][] predictNodes(double[][] records, int num_cores_evaluate){
		int n_star = records.length;
		final YARFNode[][] nodes = new YARFNode[n_star][num_trees];
		
		//speedup for the dumb user
		if (num_cores_evaluate == 1){			
			for (int i = 0; i < n_star; i++){
				nodes[i] = predictNode(records[i]);
			}
			return nodes;
		}
		
		ExecutorService tree_eval_pool = Executors.newFixedThreadPool(num_cores_evaluate);
		for (int i = 0; i < n_star; i++){
			final int i_f = i;
			tree_eval_pool.execute(new Runnable(){
				public void run() {
					try {
						nodes[i_f] = predictNode(records[i_f]);
					} catch (ArrayIndexOutOfBoundsException e){
						tree_eval_pool.shutdownNow();
						e.printStackTrace();
					}
				}
			});
		}
		tree_eval_pool.shutdown();
		try {	         
			tree_eval_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS); //effectively infinity
	    } catch (InterruptedException ignored){}
		
		return nodes;	
	}	

	private YARFNode[] predictNode(double[] record) {
		YARFNode[] nodes = new YARFNode[num_trees];
		for (int t = 0; t < num_trees; t++){
			nodes[t] = yarf_trees[t].predictNode(record);
		}
		return nodes;
	}

	/**
	 * The default BART evaluation of a new observations is done via sample average of the 
	 * posterior predictions. Other functions can be used here such as median, mode, etc. 
	 * Default is to use one CPU core.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 */
	public double Evaluate(double[] record) {	
		return nodeAssignmentAggregation(allNodeAssignments(record));
	}	
	
	private double nodeAssignmentAggregation(double[] y_preds){
		if (customFunctionAggregation()){
			return runAggregation(y_preds, this);
		}
		if (is_a_regression){
			return StatUtils.mean(y_preds); //the sample average
		}
		return r.random_sample_mode(y_preds); //most likely class
	}
	
	public double[] allNodeAssignments(double[] record){
		int num_trees_currently = progress();
		//if they're all done, use a fast method
		if (num_trees_currently == num_trees){
			double[] y_preds = new double[num_trees_currently];
			for (int t = 0; t < num_trees; t++){
				y_preds[t] = yarf_trees[t].Evaluate(record);	
			}
			return y_preds;	
		}
		//if they're not all done, use a slow method - evaluate only on trees that are completed and aggregate
		else {
			TDoubleHashSet y_preds = new TDoubleHashSet(num_trees_currently);
			for (int t = 0; t < num_trees; t++){
				YARFTree tree = yarf_trees[t];
				if (tree.completed){
					y_preds.add(tree.Evaluate(record));
				}
			}
			return y_preds.toArray();
		}
	}
//	
//	/**
//	 * Find depth of all trees
//	 * 
//	 * @return	Depths of all trees
//	 */
//	public int[] getDepthsForTrees(){
//		int[] depths = new int[num_trees];
//		for (int t = 0; t < num_trees; t++){
//			depths[t] = yarf_trees[t].maxDepth();
//		}
//		return depths;	
//	}	
//	
//	/**
//	 * Find num leaves of all trees
//	 * 
//	 * @return	Num Leaves of all trees
//	 */
//	public int[] geNumLeavesForTrees(){
//		int[] depths = new int[num_trees];
//		for (int t = 0; t < num_trees; t++){
//			depths[t] = yarf_trees[t].numLeaves();
//		}
//		return depths;	
//	}
	
	/**
	 * After burn in, return the number of total nodes (internal plus terminal) of each tree for each Gibbs sample
	 * 
	 * @param thread_num	which CPU core (which Gibbs chain) to return results for
	 * @return				for each Gibbs chain return a vector of number of nodes for all <code>num_trees</code> chains
	 */
	public int[][] getNumNodesAndLeavesForTrees(){
		return null;
	}

	public void addBootstrapIndices(int[] indices_t, int t){
		//System.out.println("addBootstrapIndices t = " + t + " ind = " + indices_t);
		bootstrap_indices[t] = indices_t;
	}
	
	public void addOtherIndices(int[] indices_t, int t){
		//System.out.println("addOtherIndices t = " + t + " ind = " + indices_t);
		other_indices[t] = indices_t;
	}
	
	public void finalizeTrainingData(){
		super.finalizeTrainingData();
		//initialize other data that requires data to be finalized
		X_by_col = new TIntObjectHashMap<double[]>(p);
		bootstrap_indices = new int[num_trees][];
		other_indices = new int[num_trees][];
		//System.out.println("bil" + bootstrap_indices.length);
		indices_one_to_n = new TIntHashSet();
		for (int i = 0; i < n; i++){
			indices_one_to_n.add(i);
		}
		//System.out.println("indices_one_to_n" + indices_one_to_n);


		sorter_locks = new Object[p];
		for (int j = 0; j < p; j++){
			sorter_locks[j] = new Object();
		}
		//System.out.println("sorter_locks" + sorter_locks);
	}
	
	
	/**
	 * Given a training data set indexed by row, this produces a training
	 * data set indexed by column
	 * 
	 * @param j		The feature to get
	 * @return		The nx1 vector of that feature
	 */
	protected double[] getXj(int j) {
//		System.out.println("getXJ " + j + "n" + n + "p" + p);
		double[] x_dot_j = X_by_col.get(j);
		if (x_dot_j == null){ //gotta build it
//			synchronized(X_by_col){ //don't wanna build it twice so sync it
				x_dot_j = new double[n];
				for (int i = 0; i < n; i++){
//					System.out.println("getXJ " + j + " i " + i);
					x_dot_j[i] = X.get(i)[j];
				}
				X_by_col.put(j, x_dot_j);
//			}	
		}
		return x_dot_j;
	 }
	
//	private TIntObjectHashMap<BitSet> feature_to_missingness;
//	
//	protected boolean missingnessExistsInXj(int j, int[] indices_to_check){
//		BitSet missingness_indices = feature_to_missingness.get(j);
//		if (feature_to_missingness == null){
//			for (int i = 0; i < n; i++){
//				
//			}
//		}
//
//		return false;
//	}
	
//	protected boolean missingnessExistsInXj(int j){
//		return missingnessExistsInXj(j, this.indices_one_to_n._set);
//	}
	
//	private TIntObjectHashMap<TDoubleIntHashMap> feature_to_split_point_to_indices;


	
//	protected TDoubleIntHashMap splitPointToCutoffSortedIndex(int j){
//		TDoubleIntHashMap split_point_to_cutoff_index = feature_to_split_point_to_indices.get(j);
//		if (split_point_to_cutoff_index != null){
//			return split_point_to_cutoff_index;
//		}
//		
//		//do the work and cache it
//		split_point_to_cutoff_index = new TDoubleIntHashMap(n);
//		
//		//we need to get x values we can split on
//		double[] xj = getXj(j);
//		int[] sorted_indices = getSortedIndicesForAnAttribute(j);
//		for (int i = 0; i < n; i++){
//			double val = xj[sorted_indices[i]];
//			split_point_to_cutoff_index.put(val, sorted_indices[i]);
//		}
//		
//		feature_to_split_point_to_indices.put(j, split_point_to_cutoff_index);
//		return split_point_to_cutoff_index;
//	}
	
	protected void sortedIndices(int j, TIntArrayList sub_indices, TIntArrayList ordered_nonmissing_indices_j, TIntArrayList missing_indices_j){
		//we only do the sorting ONCE per attribute... this ensures this runs with a minimal 
		//amount of thread butting
//		synchronized(sorter_locks[j]){
//			int[] indices_sorted_j = all_attribute_sorts.get(j);
//			if (indices_sorted_j == null){ //we need to build it
//				indices_sorted_j = getSortedIndicesForAnAttribute(j);
//				all_attribute_sorts.put(j, indices_sorted_j);
//			}			
//		}
		//////the above code is not used anywhere.... why???

//		TIntHashSet sub_indices_set = new TIntHashSet(sub_indices);
//		if (sub_indices_set.size() != sub_indices.size()) {
//			System.out.println("Duplicates in sub_indices");
//			System.out.println(Tools.StringJoin(sub_indices));
//			Integer n = null;
//			int k = n;
//		}
		//System.out.println("sortedIndices j = " + j + " indices_sorted_j = " + Tools.StringJoin(indices_sorted_j));
		int n_sub = sub_indices.size();
		//System.out.println("sortedIndices j = " + j + " sub_indices = " + Tools.StringJoin(sub_indices));
		ArrayList<SortPair> non_missing_pairs = new ArrayList<SortPair>(n_sub);
		double[] x_j = getXj(j);
		for (int i_s = 0; i_s < n_sub; i_s++){
			int sub_ind = sub_indices.get(i_s);
			if (isMissing(x_j[sub_ind])){
				missing_indices_j.add(sub_ind);
			}
			else {
				non_missing_pairs.add(new SortPair(sub_ind, x_j[sub_ind]));
			}	
		}
		Collections.sort(non_missing_pairs);
		
		ordered_nonmissing_indices_j.ensureCapacity(non_missing_pairs.size());
		for (int i_s = 0; i_s < non_missing_pairs.size(); i_s++){
			ordered_nonmissing_indices_j.add(non_missing_pairs.get(i_s).ind);
		}

//		for (int i : ordered_nonmissing_indices_j.toArray()) {
//			if (missing_indices_j.contains(i)) {
//				System.out.println("Duplicate indices between missing/non-missing");
//				System.out.println(Tools.StringJoin(missing_indices_j));
//				Integer n = null;
//				int k = n;
//			}
//		}
		TIntHashSet s = new TIntHashSet(ordered_nonmissing_indices_j);
//		if (s.size() != ordered_nonmissing_indices_j.size()) {
//			System.out.println("Duplicates in ordered_nonmissing");
//			System.out.println(Tools.StringJoin(ordered_nonmissing_indices_j));
//			Integer n = null;
//			int k = n;
//		}
//		TIntHashSet sub_indices_set = new TIntHashSet(sub_indices);
//		if (s.size() + missing_indices_j.size() != sub_indices_set.size()) {
//			System.out.println("Missing indices between the two");
//			System.out.println("s.size: " + s.size() + " missing size: " + missing_indices_j.size() + " n_sub: " + n_sub);
//			System.out.println(Tools.StringJoin(s));
//			System.out.println(Tools.StringJoin(missing_indices_j));
//			Integer n = null;
//			int k = n;
//		}
		//System.out.println("sortedIndices j = " + j + " ordered_nonmissing_indices_j = " + Tools.StringJoin(ordered_nonmissing_indices_j));
	}
	
	
	private class SortPair implements Comparable<SortPair>{
	  public int ind;
	  public double value;

	  public SortPair(int ind, double value){
		  this.ind = ind;
		  this.value = value;
	  }

	  @Override 
	  public int compareTo(SortPair o){
	    return Double.compare(value, o.value);
	  }
	}
	
	//Java is about annoying as they come... why isn't this implemented????
//	private int[] getSortedIndicesForAnAttribute(int j) {
//		double[] xj = getXj(j);
//		ArrayList<SortPair> temp = new ArrayList<SortPair>(n);
//		for (int i = 0; i < n; i++){
//			temp.add(new SortPair(i, xj[i]));
//		}
//		Collections.sort(temp);
//		int[] indices = new int[n];
//		for (int i = 0; i < n; i++){
//			indices[i] = temp.get(i).ind;
//		}		
//		return indices;
//	}

	//this is the default mtry as specified by Breiman
	public int defaultMtry(){
		if (default_mtry == null){
			default_mtry = Math.max(1, (int)Math.floor(is_a_regression ? (p / (double)3) : Math.sqrt(p))); //at least it's 1!!!
		}
//		System.out.println("default_mtry: " + default_mtry);
		return default_mtry;
	}
	
	public void setWait(boolean wait){
		this.wait = wait;
	}

	
	public long getCompletionTime(){
		return tf;
	}

	public boolean stopped(){
		return stopped;
	}
	
	public void StopBuilding() {
		stopped = true;
		for (int t = 0; t < num_trees; t++){
			yarf_trees[t].StopBuilding();
		}
	}
	
	public YARFTree[] getCompletedTrees(){
		return (YARFTree[]) Arrays.stream(yarf_trees).filter(t -> t.completed).toArray();
	}
	
	public int[] getNumLeaves(){
		int[] num_leaves = new int[num_trees];
		for (int t = 0; t < num_trees; t++){
			num_leaves[t] = yarf_trees[t].numLeaves();
		}
		return num_leaves;		
	}
	
	public YARFNode getNode(int t, String location){
		location = location.toLowerCase();
		YARFNode node = yarf_trees[t].root;
		for (int i = 0; i < location.length(); i++){
			if (location.charAt(i) == 'l'){
				node = node.left;
			} else if (location.charAt(i) == 'r'){
				node = node.right;
			} // The "r" for root does nothing
		}
		return node;
	}
	
	public int[] getNumNodes(){
		int[] num_nodes = new int[num_trees];
		for (int t = 0; t < num_trees; t++){
			num_nodes[t] = yarf_trees[t].numNodes();
		}
		return num_nodes;
	}
	
	public int[] getMaxDepths(){
		int[] depths = new int[num_trees];
		for (int t = 0; t < num_trees; t++){
			depths[t] = yarf_trees[t].depth();
		}
		return depths;		
	}
	
	public double nullModelCost(){
		if (null_model_cost == null){
			if (is_a_regression){ //the cost is the SSE (across both the left and right)
				null_model_cost = StatToolbox.sample_sum_sq_err(y, StatToolbox.sample_average(y));
			}
			else { //it's a classification - the "cost" is Gini
				null_model_cost = StatToolbox.gini_split(y);
			}
		}
		return null_model_cost;
	}
	

	public void setSeed(int seed){
		this.seed = seed;
		r.setSeed(seed);
	}

	public void stopAtConvergence(){
		stop_at_convergence = true;
	}
	
	public void iterativelyCalcOob(){
		iteratively_calc_oob = true;
	}
	
	public void stopIterativelyCalcOob(){
		iteratively_calc_oob = false;
	}
	
	public void setTolerance(double tolerance){
		this.tolerance = tolerance;
	}

	public void treeCompletedCallback() {
		if (stop_at_convergence || iteratively_calc_oob){
			oob_cost_by_iteration.add(calcOOBCost());
		}
		if (stop_at_convergence){
//			System.out.println("costs: " + Tools.StringJoin(oob_cost_by_iteration));
			calcOOBCostChanges();
//			System.out.println("deltas: " + Tools.StringJoin(oob_costs_changes));
			assessConvergenceAndStopIfSo();
		}
	}

	private void calcOOBCostChanges() {
		if (oob_cost_by_iteration.size() > 1){
			oob_costs_changes.add(
				oob_cost_by_iteration.get(oob_cost_by_iteration.size() - 1) 
				- 
				oob_cost_by_iteration.get(oob_cost_by_iteration.size() - 2)
			);
		}		
	}
	
	public double[] OOBCostsByIteration(){
		return oob_cost_by_iteration.toArray();
	}

	private double calcOOBCost() {
		double[] y_hats = predictOutOfBag(1); //since this is run within one thread, this should be done in serial, hence the "1"
//		System.out.println("y_hats: " + Tools.StringJoin(y_hats));
//		System.out.println("y: " + Tools.StringJoin(y));
		
		if (customOutOfBagCosts()){
			return StatUtils.sum(customOutOfBagCostCalc(y, y_hats));
		}
		if (is_a_regression){
			return StatToolbox.sampleSumSqErrIgnoreNans(y, y_hats) / nullModelCost(); //1 - R^2 = SSE / SST
		}
		return StatToolbox.misclassificationErrorIgnoreNans(y, y_hats);	
	}	

	
	public double[] customOutOfBagCostCalc(double[] y, double[] y_oob){
		int n_oob = y_oob.length;
		double[] costs = new double[n_oob];
		for (int i = 0; i < n_oob; i++){
			//no need for an error check for oob_cost_calculation_str here as it's done in R
			if (!Double.isNaN(y_oob[i])){
				costs[i] = runOobCostCalculation(y_oob[i], y[i]);
				if (costs[i] == YARFNode.BAD_FLAG_double){
					break;
				}
			}
		}
		return costs;
	}
	
	private void assessConvergenceAndStopIfSo() {
		//first determine the first vacillation point
		int t0 = Integer.MAX_VALUE;
		int l = oob_costs_changes.size();
		for (int t = 0; t < l; t++){
			if (oob_costs_changes.get(t) > 0){
				t0 = t;
				break;
			}
		}
		//ditch if we haven't reached a vacillation point
		if (t0 == Integer.MAX_VALUE){
			return;
		}
		
		double[] vacillations = oob_costs_changes.subList(t0, l).toArray();
		double vacillations_avg = StatUtils.mean(vacillations);
		double vacillations_var = StatUtils.variance(vacillations);
		if (vacillations_var == 0){
			return;
		}
		double moe = Math.sqrt(vacillations_var) / Math.sqrt(l - t0);
		//now see if v-bar +- s / sqrt(n) is inside the tolerance window

//		System.out.println("vacillations: " + Tools.StringJoin(vacillations));
//		System.out.println("vacillations_avg: " + vacillations_avg + " vacillations_sd: " + Math.sqrt(vacillations_var) + " n = " + vacillations.length);
//		System.out.println("CI: [" + (vacillations_avg - moe) + ", "  + (vacillations_avg + moe) + "]");
//		System.out.println("tolerance: [" + (-tolerance) + ", "  + (tolerance) + "]");
		
		if ((vacillations_avg - moe > -tolerance) && (vacillations_avg + moe < tolerance)){ //i.e. convergence
			//we're done
			synchronized (this){
				if (!stopped){
					System.out.println(
						"YARF model converged in " + 
						(((ThreadPoolExecutor)yarf_tree_grow_threadpool).getPoolSize() > 1 ? "approximately " : "") + 
						progress() + 
						" trees."
					);
					StopBuilding();
					converged = true;
				}				
			}
		}		
	}
	
	public boolean converged(){
		return converged;
	}
    
    public String[][] proximity(double[][] X, int num_cores){
//    	System.out.println("run proximity function with " + num_cores + " cores!");
        int n = X.length;
        final String[][] out = new String[n][num_trees];
        
		ExecutorService proximity_info_getter_pool = Executors.newFixedThreadPool(num_cores);
		for (int t = 0; t < num_trees; t++){
			final int tf = t;
	    	proximity_info_getter_pool.execute(new Runnable(){
				public void run() {
//					System.out.println("tree " + tf + " thread: " + Thread.currentThread().getId());
					try {
						YARFNode root = yarf_trees[tf].root;
			            for (int i = 0; i < n; i++){
			                out[i][tf] = root.prox_info(X[i]);
			            }
					} catch (Exception e){
						e.printStackTrace();
						proximity_info_getter_pool.shutdownNow();
					}
				}
	    	});
		}		
		proximity_info_getter_pool.shutdown();
		try {
			proximity_info_getter_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS); //infinity
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
        return out;
    }

	@Override
	public void Build() {
		Build(1);
	}
	
}
