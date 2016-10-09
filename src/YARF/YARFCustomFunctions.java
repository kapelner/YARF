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
	private String shared_scripts_str;
	
	//everything that has to do with scripts that's transient / lazy-loaded
	private transient ScriptEngine nashorn_js_engine;
    private transient Compilable compilingEngine;
    //all invocables
    private transient Invocable mtry_fun;
    private transient Invocable make_node_into_leaf_fun;
    private transient Invocable cost_single_node_calc_fun;
    private transient Invocable cost_both_children_calc_fun;
    private transient Invocable node_assignment_fun;
    private transient Invocable after_node_birth_fun;
    private transient Invocable aggregation_fun;
    private transient Invocable prune_if_fun;

	private Invocable stringToInvokableCompiledFunction(String script_as_string, String function_name) {

        //lazy load for this stuff for serialization to work properly
		if (nashorn_js_engine == null){
			nashorn_js_engine = new ScriptEngineManager().getEngineByName("nashorn");
		}
		if (compilingEngine == null){
			compilingEngine = (Compilable) nashorn_js_engine;
		}
		try { //try the shared scripts first so if it fails... the user knows where to look
			compilingEngine.compile(shared_scripts_str);
		} catch (ScriptException e) {
			StopBuilding();
			System.err.println("There was a problem compiling the shared script:");
			e.printStackTrace();
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
			mtry_fun = stringToInvokableCompiledFunction(mtry_function_str, MTryScriptFunctionName);	
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
			return (double)cost_both_children_calc_fun.invokeFunction("splitCost", leftNode, rightNode);
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
			//System.out.println("compile node_assignment_function_str:\n" + node_assignment_function_str + "\n======");
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
	
	public double runAfterNodeBirth(YARFNode node){
		if (after_node_birth_fun == null){
			after_node_birth_fun = stringToInvokableCompiledFunction(after_node_birth_function_str, AfterNodeBirthScriptFunctionName);
		}
		try {
			return (double)after_node_birth_fun.invokeFunction(AfterNodeBirthScriptFunctionName, node);
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
		return YARFNode.BAD_FLAG_double;
	}
	
	public double runAggregation(double[] y_hats, YARFTree[] yarf_trees){
		if (aggregation_fun == null){
			aggregation_fun = stringToInvokableCompiledFunction(aggregation_function_str, AggregationScriptFunctionName);	
		}
		try {
			return (double)aggregation_fun.invokeFunction(AggregationScriptFunctionName, y_hats, yarf_trees);
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
	
	
	
	
	
	public boolean customFunctionMtry(){
		return mtry_function_str != null;
	}
	
	public boolean customFunctionNodesize(){
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

	public void setMake_node_into_leaf_function_str(String nodesize_function_str) {
		this.make_node_into_leaf_function_str = nodesize_function_str;
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
}
