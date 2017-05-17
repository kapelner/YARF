package YARF;

import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public abstract class YARFCustomFunctions extends Classifier {
	private static final long serialVersionUID = 2930694920484546709L;
	
	//all custom scripts and the functions they must include (as a string)
	private String mtry_function_str;
	public static final String MTryScriptFunctionName = "tryVars";
	private String make_node_into_leaf_function_str;
	public static final String SplitValuesScriptFunctionName = "tryVals";
	private String split_values_function_str;
	public static final String NodesizeLegalScriptFunctionName = "makeNodeIntoLeaf";
	private String cost_single_node_calc_function_str;
	public static final String SingleNodeCostScriptFunctionName = "nodeCost";
	private String cost_both_children_calc_function_str;
	public static final String TotalNodeCostScriptFunctionName = "totalChildrenCost";
	private String node_assignment_function_str;
	public static final String AssignYhatToNodeScriptFunctionName = "assignYhatToNode";
	private String after_node_birth_function_str;
	public static final String AfterNodeBirthScriptFunctionName = "nodeAfterNodeBirth";
	private String aggregation_function_str;
	public static final String AggregationScriptFunctionName = "aggregateYhatsIntoOneYhat";
	private String prune_if_function_str;
	public static final String PruneIfScriptFunctionName = "pruneIf";
	private String oob_cost_calculation_str;
	public static final String OobCostScriptFunctionName = "oobCost";
	private String print_at_split_node_str;
	public static final String PrintAtSplitNodeScriptFunctionName = "printAtSplitNode";
	private String print_at_leaf_str;
	public static final String PrintAtLeafScriptFunctionName = "printAtLeaf";
	private String shared_scripts_str;
	
	//everything that has to do with scripts that's transient / lazy-loaded
	private transient ScriptEngine nashorn_js_engine;
    private transient Compilable compilingEngine;
    //all invocables
    private transient Invocable mtry_fun;
    private transient Invocable split_vals_fun;
    private transient Invocable make_node_into_leaf_fun;
    private transient Invocable cost_single_node_calc_fun;
    private transient Invocable cost_both_children_calc_fun;
    private transient Invocable node_assignment_fun;
    private transient Invocable after_node_birth_fun;
    private transient Invocable aggregation_fun;
    private transient Invocable prune_if_fun;
    private transient Invocable print_at_split_node_fun;
    private transient Invocable print_at_leaf_fun;
    private transient Invocable oob_cost_calculation_fun;

	private Invocable stringToInvokableCompiledFunction(String script_as_string, String function_name) {

//		System.out.println("stringToInvokableCompiledFunction");
        //lazy load for this stuff for serialization to work properly
		if (nashorn_js_engine == null){
			nashorn_js_engine = new ScriptEngineManager().getEngineByName("nashorn");
		}
//		System.out.println("got nashorn: " + nashorn_js_engine);
		if (compilingEngine == null){
			compilingEngine = (Compilable) nashorn_js_engine;
		}
//		System.out.println("got compilingEngine: " + compilingEngine);
		if (shared_scripts_str != null){
			try { //try the shared scripts first so if it fails... the user knows where to look
//				System.out.println("try compile shared_scripts_str: " + shared_scripts_str);
				compilingEngine.compile(shared_scripts_str);
			} catch (ScriptException e) {
				StopBuilding();
				System.err.println("There was a problem compiling the shared script:");
				e.printStackTrace();
			}
//			System.out.println("compiled shared scripts");
		} else {
			shared_scripts_str = "";
		}
		CompiledScript cscript = null;
		try {
			cscript = compilingEngine.compile(script_as_string + shared_scripts_str);
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem compiling the script with the \"" + function_name + "\" function:");
			e.printStackTrace();
		}
		try {
			cscript.eval(nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE));
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem adding the scope to the script with the \"" + function_name + "\" function:");
			e.printStackTrace();
		}
        return (Invocable)cscript.getEngine();
	}
	
	public int[] runMtry(YARFNode node){
		if (mtry_fun == null){
			mtry_fun = stringToInvokableCompiledFunction(split_values_function_str, MTryScriptFunctionName);	
		}
		try {
			return  (int[]) mtry_fun.invokeFunction(MTryScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your mtry script must include the function \"" + MTryScriptFunctionName + "(node)\" and return an array of integers.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + MTryScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return null;
	}	

	public double[] runSplitValues(YARFNode node, int j){
		if (split_vals_fun == null){
			split_vals_fun = stringToInvokableCompiledFunction(split_values_function_str, SplitValuesScriptFunctionName);	
		}
		try {
			return  (double[]) split_vals_fun.invokeFunction(SplitValuesScriptFunctionName, node, j);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your mtry script must include the function \"" + SplitValuesScriptFunctionName + "(node)\" and return an array of doubles.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + SplitValuesScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return null;
	}
	
	public boolean runNodesizeLegal(YARFNode node){
		if (make_node_into_leaf_fun == null){
			make_node_into_leaf_fun = stringToInvokableCompiledFunction(make_node_into_leaf_function_str, NodesizeLegalScriptFunctionName);	
		}
		try {
			return (boolean)make_node_into_leaf_fun.invokeFunction(NodesizeLegalScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your nodesize legal script must include the function \"" + NodesizeLegalScriptFunctionName + "(node)\" and return a boolean where true means the node under consideration is legal.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + NodesizeLegalScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return false;
	}
	
	public double runSingleNodeCost(YARFNode node){
		if (cost_single_node_calc_fun == null){
			cost_single_node_calc_fun = stringToInvokableCompiledFunction(cost_single_node_calc_function_str, SingleNodeCostScriptFunctionName);	
		}
		try {
			return (double)cost_single_node_calc_fun.invokeFunction(SingleNodeCostScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your node cost script must include the function \"" + SingleNodeCostScriptFunctionName + "(node)\" and return the cost as a double.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + SingleNodeCostScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return YARFNode.BAD_FLAG_double;
	}
		
	public double runBothChildrenCost(YARFNode leftNode, YARFNode rightNode){
		if (cost_both_children_calc_fun == null){
			cost_both_children_calc_fun = stringToInvokableCompiledFunction(cost_both_children_calc_function_str, TotalNodeCostScriptFunctionName);	
		}
		try {
			return (double)cost_both_children_calc_fun.invokeFunction(TotalNodeCostScriptFunctionName, leftNode, rightNode);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your total cost script must include the function \"" + TotalNodeCostScriptFunctionName + "(leftNode, rightNode)\" and return the total cost of both nodes as a double.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + TotalNodeCostScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return YARFNode.BAD_FLAG_double;
	}
	
	public double runNodeAssignment(YARFNode node){
		if (node_assignment_fun == null){
			node_assignment_fun = stringToInvokableCompiledFunction(node_assignment_function_str, AssignYhatToNodeScriptFunctionName);
			System.err.println("compile node_assignment_function_str:\n" + node_assignment_function_str + "\n======");
		}
		try {
			return (double)node_assignment_fun.invokeFunction(AssignYhatToNodeScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your node assign script must include the function \"" + AssignYhatToNodeScriptFunctionName + "(node)\" and return the node assignment (the predicted yhat) as a double.");
			System.err.println("node_assignment_function_str:\n" + node_assignment_function_str);
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + AssignYhatToNodeScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return YARFNode.BAD_FLAG_double;
	}
	
	public void runAfterNodeBirth(YARFNode node){
		if (after_node_birth_fun == null){
			after_node_birth_fun = stringToInvokableCompiledFunction(after_node_birth_function_str, AfterNodeBirthScriptFunctionName);
		}
		try {
			after_node_birth_fun.invokeFunction(AfterNodeBirthScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your after node birth script must include the function \"" + AfterNodeBirthScriptFunctionName + "(node)\" and return nothing.");
			System.err.println("after_node_birth_fun:\n" + after_node_birth_function_str);
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + AfterNodeBirthScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
	}
	
	public double runAggregation(double[] y_hats, YARF yarf){
		if (aggregation_fun == null){
			aggregation_fun = stringToInvokableCompiledFunction(aggregation_function_str, AggregationScriptFunctionName);	
		}
		try {
			return (double)aggregation_fun.invokeFunction(AggregationScriptFunctionName, y_hats, yarf);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your aggregation script must include the function \"" + AggregationScriptFunctionName + "(y_hats, yarf_trees)\" and return the aggregated prediction as a double.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + AggregationScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return YARFNode.BAD_FLAG_double;
	}
	
	public boolean runPruneIf(YARFNode node){
		if (prune_if_fun == null){
			prune_if_fun = stringToInvokableCompiledFunction(prune_if_function_str, PruneIfScriptFunctionName);	
		}
		try {
			return (boolean)prune_if_fun.invokeFunction(PruneIfScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your prune if script must include the function \"" + PruneIfScriptFunctionName + "(node)\" and return a boolean.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + PruneIfScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return false;
	}
	
	public double runOobCostCalculation(double y_hat, double y){
		if (oob_cost_calculation_fun == null){
			oob_cost_calculation_fun = stringToInvokableCompiledFunction(oob_cost_calculation_str, OobCostScriptFunctionName);	
		}
		try {
			return (double)oob_cost_calculation_fun.invokeFunction(OobCostScriptFunctionName, y_hat, y);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your oob cost calculation script must include the function \"" + OobCostScriptFunctionName + "(y_hat, y)\" and return the aggregated prediction as a double.");
			e.printStackTrace();
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + OobCostScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return YARFNode.BAD_FLAG_double;		
	}
	
	public String runPrintAtSplitNode(YARFNode node){
//		System.out.println("runPrintAtSplitNode" + node.stringLocation(true));
		if (print_at_split_node_fun == null){
			print_at_split_node_fun = stringToInvokableCompiledFunction(print_at_split_node_str, PrintAtSplitNodeScriptFunctionName);	
		}
//		System.out.println("compiled");
		try {
			return (String)print_at_split_node_fun.invokeFunction(PrintAtSplitNodeScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your print at split node script must include the function \"" + PrintAtSplitNodeScriptFunctionName + "(node)\" and return a String.");
			e.printStackTrace();
		} catch (ScriptException e) {
			System.out.println("ScriptException");
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + PrintAtSplitNodeScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return null;
	}
	
	public String runPrintAtLeafNode(YARFNode node){
//		System.out.println("runPrintAtLeafNode" + node.stringLocation(true));
		if (print_at_leaf_fun == null){
			print_at_leaf_fun = stringToInvokableCompiledFunction(print_at_leaf_str, PrintAtLeafScriptFunctionName);	
		}
//		System.out.println("compiled");
		try {
			return (String)print_at_leaf_fun.invokeFunction(PrintAtLeafScriptFunctionName, node);
		} catch (NoSuchMethodException e) {
			StopBuilding();
			System.err.println("Your print at leaf node must include the function \"" + PrintAtLeafScriptFunctionName + "(node)\" and return a String.");
			e.printStackTrace();
		} catch (ScriptException e) {
			System.out.println("ScriptException");
			StopBuilding();
			System.err.println("There was a problem evaluating your \"" + PrintAtLeafScriptFunctionName + "\" function:");
			e.printStackTrace();		
		}
		return null;
	}
	

	public boolean customFunctionMtry(){
		return mtry_function_str != null;
	}

	public boolean customFunctionSplitValues(){
		return split_values_function_str != null;
	}
	
	public boolean customFunctionMakeNodeIntoLeaf(){
		return make_node_into_leaf_function_str != null;
	}
	
	public boolean customFunctionSingleNodeCostCalc(){
		return cost_single_node_calc_function_str != null;
	}
	
	public boolean customFunctionBothChildrenCostCalc(){
		return cost_both_children_calc_function_str != null;
	}
	
	public boolean customFunctionNodeAssignment(){
		return node_assignment_function_str != null;
	}
	
	public boolean customFunctionAfterBirth(){
		return after_node_birth_function_str != null;
	}
	
	public boolean customFunctionAggregation(){
		return aggregation_function_str != null;
	}
	
	public boolean customFunctionOobCostCalculation(){
		return oob_cost_calculation_str != null;
	}
	
	public boolean customFunctionPrintAtSplitNode(){
//		System.out.println("customFunctionPrintAtSplitNode " +  print_at_split_node_str);
		return print_at_split_node_str != null;
	}
	
	public boolean customFunctionPrintAtLeafNode(){
		return print_at_leaf_str != null;
	}
	
	public double[] customOutOfBagCosts(double[] y_oob, double[] y){
		int n_oob = y_oob.length;
		double[] costs = new double[n_oob];
		for (int i = 0; i < n_oob; i++){
			//no need for an error check for oob_cost_calculation_str here as it's done in R
			costs[i] = runOobCostCalculation(y_oob[i], y[i]);
		}
		return costs;
	}	
	

	public String getShared_scripts_str() {
		return shared_scripts_str;
	}

	public void setShared_scripts_str(String shared_scripts_str) {
		this.shared_scripts_str = shared_scripts_str;
	}

	public String getMtry_function_str() {
		return mtry_function_str;
	}

	public void setMtry_function_str(String mtry_function_str) {
		this.mtry_function_str = mtry_function_str;
	}
	
	public String getSplit_values_function_str() {
		return split_values_function_str;
	}

	public void setSplit_values_function_str(String split_values_function_str) {
		this.split_values_function_str = split_values_function_str;
	}	

	public String getCost_single_node_calc_function_str() {
		return cost_single_node_calc_function_str;
	}

	public void setCost_single_node_calc_function_str(
			String cost_single_node_calc_function_str) {
		this.cost_single_node_calc_function_str = cost_single_node_calc_function_str;
	}

	public String getCost_both_children_calc_function_str() {
		return cost_both_children_calc_function_str;
	}

	public void setCost_both_children_calc_function_str(
			String cost_both_children_calc_function_str) {
		this.cost_both_children_calc_function_str = cost_both_children_calc_function_str;
	}

	public String getAggregation_function_str() {
		return aggregation_function_str;
	}

	public void setAggregation_function_str(String aggregation_function_str) {
		this.aggregation_function_str = aggregation_function_str;
	}

	public String getMake_node_into_leaf_function_str() {
		return make_node_into_leaf_function_str;
	}

	public void setMake_node_into_leaf_function_str(String make_node_into_leaf_function_str) {
		this.make_node_into_leaf_function_str = make_node_into_leaf_function_str;
	}

	public String getNode_assignment_function_str() {
		return node_assignment_function_str;
	}

	public void setNode_assignment_function_str(String node_assignment_function_str) {
		this.node_assignment_function_str = node_assignment_function_str;
	}
	
	public String getAfter_node_birth_function_str() {
		return after_node_birth_function_str;
	}

	public void setAfter_node_birth_function_str(String after_node_assignment_function_str) {
		this.after_node_birth_function_str = after_node_assignment_function_str;
	}

	public String getPrune_if_function_str() {
		return prune_if_function_str;
	}

	public void setPrune_if_function_str(String prune_if_function_str) {
		this.prune_if_function_str = prune_if_function_str;
	}
	
	public String getOob_cost_calculation_str() {
		return oob_cost_calculation_str;
	}

	public void setOob_cost_calculation_str(String oob_cost_calculation_str) {
		this.oob_cost_calculation_str = oob_cost_calculation_str;
		//the function itself should be reset here
		oob_cost_calculation_fun = null;
	}
	public String getPrint_at_split_node_str() {
		return print_at_split_node_str;
	}

	public void setPrint_at_split_node_str(String print_at_split_node_str) {
//		System.out.println("begin setPrint_at_split_node_str");
		this.print_at_split_node_str = print_at_split_node_str;
		//the function itself should be reset here
		print_at_split_node_fun = null;
//		System.out.println("after setPrint_at_split_node_str");
	}

	public String getPrint_at_leaf_str() {
		return print_at_leaf_str;
	}

	public void setPrint_at_leaf_str(String print_at_leaf_str) {
		this.print_at_leaf_str = print_at_leaf_str;
		//the function itself should be reset here
		print_at_leaf_fun = null;
	}
}
