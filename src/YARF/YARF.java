package YARF;


import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.set.hash.TIntHashSet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;


/**
 * This class handles the parallelization of many Gibbs chains over many CPU cores
 * to create one BART regression model. It also handles all operations on the completed model.
 * 
 * @author Adam Kapelner
 */
public class YARF extends Classifier implements Serializable {
	private static final long serialVersionUID = -6984205353140981153L;
	
	/** the number of CPU cores to build many different trees in a YARF model */
	protected int num_cores = 1; //default
	/** the number of trees in this RF model on all Gibbs chains */
	protected int num_trees = 500; //default


	private YARFTree[] yarf_trees;
	private int[][] bootstrap_indices;
	private TIntHashSet indicies_one_to_n;
	private int mtry;
	private int nodesize;
	
	protected TIntObjectHashMap<int[]> all_attribute_sorts;

	protected ArrayList<Integer> indicies_one_to_p;
	
	private ScriptEngine nashorn_js_engine;
    private Compilable compilingEngine;
	private String shared_funs;
	private Invocable mtry_fun;
	private Invocable nodesize_fun;
	private Invocable cost_calc_fun;
	private Invocable node_assign_fun;
	
	public YARF(){}
	
	//init
	//set num cores, seed, verbose, etc
	//add data
	//set data feature names
	//add "other" data
	//set "other" data feature names
	//set num trees
	//init trees
	//give bootstrap samples (indices)
	//load all custom functions
	//BUILD
	
	
	public void setNumCores(int num_cores){
		this.num_cores = num_cores;
	}
	
	public void setNumTrees(int num_trees){
		this.num_trees = num_trees;
	}
	
	public void setMTry(int mtry){
		this.mtry = mtry;
	}
	
	public void setNodesize(int nodesize){
		this.nodesize = nodesize;
	}
	
	//all JS functions stuff --- shared must be set FIRST!!!!
	public void setSharedFunctions(String shared_funs) throws ScriptException{
		this.shared_funs = shared_funs;
	}
	
	private Invocable stringToInvokableCompiledFunction(String fun) throws ScriptException{
        //lazy load for this stuff
		if (nashorn_js_engine == null){
			nashorn_js_engine = new ScriptEngineManager().getEngineByName("nashorn");
		}
		if (compilingEngine == null){
			compilingEngine = (Compilable) nashorn_js_engine;
		}
		
		String fun_and_shared_libraries = fun + "\n\n" + shared_funs;
		CompiledScript cscript = compilingEngine.compile(fun_and_shared_libraries); 
        cscript.eval(nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE));
        return (Invocable)cscript.getEngine();
	}
	
	public void setMTryFunction(String mtry_fun) throws ScriptException{
        this.mtry_fun = stringToInvokableCompiledFunction(mtry_fun);
	}
	
	public void setNodesizeFunction(String nodesize_fun) throws ScriptException{
		this.nodesize_fun = stringToInvokableCompiledFunction(nodesize_fun);
	}
	
	public void setCostCalcFunction(String cost_calc_fun) throws ScriptException{
		this.cost_calc_fun = stringToInvokableCompiledFunction(cost_calc_fun);
	}
	
	public void setNodeAssignFunction(String node_assign_fun) throws ScriptException{
		this.node_assign_fun = stringToInvokableCompiledFunction(node_assign_fun);
	}
	
	public boolean customFunctionMtry(){
		return mtry_fun != null;
	}
	
	public void initTrees(){
		yarf_trees = new YARFTree[num_trees];
		for (int t = 0; t < num_trees; t++){
			yarf_trees[t] = new YARFTree(this);
			setBootstrapAndOutOfBagIndices(t);
		}
	}
	
	public void setBootstrapAndOutOfBagIndices(int t){
		//make a copy
		yarf_trees[t].setTrainingIndices(bootstrap_indices[t]);
		//now get oob indices - it begins as the full thing then we subtract 
		//out the bootstrap indices of the tree
		TIntHashSet oob_indices = new TIntHashSet(indicies_one_to_n);
		oob_indices.removeAll(bootstrap_indices[t]);
		yarf_trees[t].setOutOfBagIndices(oob_indices);
	}
	
//	public int[] getSortedIndicesAtAttribute(int j, int sub_indices){
//		synchronized(all_attribute_sorts){
//			int[] all_indices = all_attribute_sorts.get(j);
//			if (all_indices == null){ //lazy create for this attribute
//				double[] xj = new double[n];
//				for (int i = 0; i < n; i++){
//					xj[i] = X.get(i)[j];
//				}
//			}			
//		}
//
//	}

	/** This function builds the forest by building all the trees */
	public void Build() {

		all_attribute_sorts = new TIntObjectHashMap<int[]>(p);
		initTrees();
		//run a build on all threads
		long t0 = System.currentTimeMillis();
		if (verbose){
			System.out.println("building YARF " + (mem_cache_for_speed ? "with" : "without") + " mem-cache speedup...");
		}

		ExecutorService tree_grow_pool = Executors.newFixedThreadPool(num_cores);
		for (int t = 0; t < num_trees; t++){
			final int tf = t;
	    	tree_grow_pool.execute(new Runnable(){
				public void run() {
					yarf_trees[tf].Build();
				}
			});
		}
		tree_grow_pool.shutdown();
		try {	         
	         tree_grow_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS); //effectively infinity
	    } catch (InterruptedException ignored){}	
		
		if (verbose){
			System.out.println("done building YARF in " + ((System.currentTimeMillis() - t0) / 1000.0) + " sec \n");
		}
	}


	/**
	 * Return the number of times each of the attributes were used during the construction of the sum-of-trees
	 * by Gibbs sample.
	 * 
	 * @param type	Either "splits" or "trees" ("splits" means total number and "trees" means sum of binary values of whether or not it has appeared in the tree)
	 * @return		The counts for all Gibbs samples further indexed by the attribute 1, ..., p
	 */
//	public int[][] getCountsForAllAttribute(final String type) {
//		final int[][] variable_counts_all_gibbs = new int[num_gibbs_total_iterations - num_gibbs_burn_in][p];		
//		
//		for (int g = 0; g < num_gibbs_total_iterations - num_gibbs_burn_in; g++){
//			final bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
//			int[] variable_counts_one_gibbs = new int[p];
//			for (bartMachineTreeNode tree : trees){	
//				if (type.equals("splits")){
//					variable_counts_one_gibbs = Tools.add_arrays(variable_counts_one_gibbs, tree.attributeSplitCounts());
//				}
//				else if (type.equals("trees")){
//					variable_counts_one_gibbs = Tools.binary_add_arrays(variable_counts_one_gibbs, tree.attributeSplitCounts());
//				}				
//				
//			}
//			variable_counts_all_gibbs[g] = variable_counts_one_gibbs;
//		}		
//		return variable_counts_all_gibbs;
//	}
	
	/**
	 * Return the proportion of times each of the attributes were used (count over total number of splits) 
	 * during the construction of the sum-of-trees by Gibbs sample.
	 * 
	 * @param type	Either "splits" or "trees" ("splits" means total number and "trees" means sum of binary values of whether or not it has appeared in the tree)
	 * @return		The proportion of splits for all Gibbs samples further indexed by the attribute 1, ..., p
	 */
//	public double[] getAttributeProps(final String type) {
//		int[][] variable_counts_all_gibbs = getCountsForAllAttribute(type);
//		double[] attribute_counts = new double[p];
//		for (int g = 0; g < num_gibbs_total_iterations - num_gibbs_burn_in; g++){
//			attribute_counts = Tools.add_arrays(attribute_counts, variable_counts_all_gibbs[g]);
//		}
//		Tools.normalize_array(attribute_counts); //will turn it into proportions
//		return attribute_counts;
//	}
	
	/**
	 * For all Gibbs samples after burn in, calculate the set of interaction counts (consider a split on x_j 
	 * and a daughter node splits on x_k and that would be considered an "interaction")
	 * 
	 * @return	A matrix of size p x p where the row is top split and the column is a bottom split. It is recommended to triangularize the matrix after ignoring the order.
	 */
//	public int[][] getInteractionCounts(){
//		int[][] interaction_count_matrix = new int[p][p];
//		
//		for (int g = 0; g < gibbs_samples_of_bart_trees_after_burn_in.length; g++){
//			bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
//			
//			for (bartMachineTreeNode tree : trees){
//				//get the set of pairs of interactions
//				HashSet<UnorderedPair<Integer>> set_of_interaction_pairs = new HashSet<UnorderedPair<Integer>>(p * p);
//				//find all interactions
//				tree.findInteractions(set_of_interaction_pairs);
//				//now tabulate these interactions in our count matrix
//				for (UnorderedPair<Integer> pair : set_of_interaction_pairs){
//					interaction_count_matrix[pair.getFirst()][pair.getSecond()]++; 
//				}
//			}	
//		}
//		
//		return interaction_count_matrix;
//	}

	/** Flush all unnecessary data from the Gibbs chains to conserve RAM */
	protected void FlushData() {
		for (int t = 0; t < num_cores; t++){
			yarf_trees[t].FlushData();
		}
	}

	/**
	 * The default BART evaluation of a new observations is done via sample average of the 
	 * posterior predictions. Other functions can be used here such as median, mode, etc. 
	 * Default is to use one CPU core.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 */
	public double Evaluate(double[] record) {	
		return 0;
	}	
	
	/**
	 * The default BART evaluation of a new observations is done via sample average of the 
	 * posterior predictions. Other functions can be used here such as median, mode, etc.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 */
	public double Evaluate(double[] record, int num_cores_evaluate) {		
		return 0;
	}		
	
	/**
	 * After burn in, find the depth (greatest generation of a terminal node) of each tree for each Gibbs sample
	 * 
	 * @param thread_num	which CPU core (which Gibbs chain) to return results for
	 * @return				for each Gibbs chain return a vector of depths for all <code>num_trees</code> chains
	 */
	public int[] getDepthsForTrees(int thread_num){
		return null;
	}	
	
	/**
	 * After burn in, return the number of total nodes (internal plus terminal) of each tree for each Gibbs sample
	 * 
	 * @param thread_num	which CPU core (which Gibbs chain) to return results for
	 * @return				for each Gibbs chain return a vector of number of nodes for all <code>num_trees</code> chains
	 */
	public int[][] getNumNodesAndLeavesForTrees(){
		return null;
	}
	
	public void addBootstrapIndices(int[] indices_t, int tree){
		bootstrap_indices[tree] = indices_t;
	}
	
	public static <E> List<E> pickNRandomElements(List<E> list, int subset_size) {
	    int length = list.size();

	    if (length < subset_size) return null;

	    //We don't need to shuffle the whole list
	    for (int i = length - 1; i >= length - subset_size; --i){
	        Collections.swap(list, i , StatToolbox.randInt(i + 1));
	    }
	    return list.subList(length - subset_size, length);
	}
	
	public void finalizeTrainingData(){
		super.finalizeTrainingData();
		bootstrap_indices = new int[num_trees][n];
		indicies_one_to_n = new TIntHashSet();
		for (int i = 0; i < n; i++){
			indicies_one_to_n.add(i);
		}
		indicies_one_to_p = new ArrayList<Integer>();
		for (int j = 0; j < p; j++){
			indicies_one_to_p.add(j);
		}
	}
	
	/** Must be implemented, but does nothing */
	public void StopBuilding() {}

	@Override
	public Classifier clone() {
		// TODO Auto-generated method stub
		return null;
	}	
}
