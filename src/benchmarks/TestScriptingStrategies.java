package benchmarks;

import java.io.FileNotFoundException;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class TestScriptingStrategies {

	public static void main(String[] args) throws ScriptException, FileNotFoundException, NoSuchMethodException{
		ScriptEngine nashorn_js_engine = new ScriptEngineManager().getEngineByName("nashorn");
        Compilable compilingEngine;
        CompiledScript cscript;
        Invocable invocable;
        long t0;
        long tf;         

        t0 = System.currentTimeMillis();
        compilingEngine = (Compilable) nashorn_js_engine;
        cscript = compilingEngine.compile("function foo(a,b){return a+b}"); 
        cscript.eval(nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE));
        invocable = (Invocable) cscript.getEngine();
        for (int i = 0; i < 1000000; i++){
        	benchmarkInvokeFunctionContextPreset(invocable);
        }
        tf = System.currentTimeMillis();
        System.out.println("time benchmarkInvokeFunctionContextPreset = " + (tf - t0));
        
        t0 = System.currentTimeMillis(); 
        compilingEngine = (Compilable) nashorn_js_engine; 
        cscript = compilingEngine.compile("function foo(a, b){return a+b}"); 
        invocable = (Invocable) cscript.getEngine();
        for (int i = 0; i < 1000000; i++){
            benchmarkInvokeFunctionSettingContext(invocable, cscript, nashorn_js_engine);
        }
        tf = System.currentTimeMillis();        
        System.out.println("time benchmarkInvokeFunctionSettingContext = " + (tf - t0));
        
        t0 = System.currentTimeMillis();
        for (int i = 0; i < 1000000; i++){
            benchmarkEvalNoCompiling(nashorn_js_engine);
        }
        tf = System.currentTimeMillis();
        System.out.println("time benchmarkEvalNoCompiling = " + (tf - t0));  


        cscript = compilingEngine.compile("a + b"); 
        t0 = System.currentTimeMillis();
        Bindings bindings = nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE);
        for (int i = 0; i < 1000000; i++){
            benchmarkEvalWithCompiling(cscript, bindings);
        }
        tf = System.currentTimeMillis();        
        System.out.println("time benchmarkEvalWithCompiling = " + (tf - t0)); 
        
	}
	
	private static void benchmarkEvalWithCompiling(CompiledScript cscript, Bindings bindings) throws ScriptException {
		double a = 5;
		double b = 3;
		bindings.put("a", a);
		bindings.put("b", b);
		@SuppressWarnings("unused")
		double res = (double)cscript.eval(bindings);
	}

	private static void benchmarkInvokeFunctionContextPreset(Invocable invocable) throws NoSuchMethodException, ScriptException {
		double a = 5;
		double b = 3;
		@SuppressWarnings("unused")
		double res = (double)invocable.invokeFunction("foo", a, b);
	}

	private static void benchmarkEvalNoCompiling(ScriptEngine nashorn_js_engine) throws ScriptException {
		double a = 5;
		double b = 3;
		@SuppressWarnings("unused")
		double res = (double)nashorn_js_engine.eval("foo(" + a + "," + b + ")");
	}
	

	private static void benchmarkInvokeFunctionSettingContext(Invocable invocable, CompiledScript cscript, ScriptEngine nashorn_js_engine) throws NoSuchMethodException, ScriptException {
		
		cscript.eval(nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE));
		double a = 5;
		double b = 3;
		@SuppressWarnings("unused")
		double res = (double)invocable.invokeFunction("foo", a, b);
	}
}


/*
time benchmarkInvokeFunctionSettingContext = 6857
time benchmarkInvokeFunctionContextPreset = 117
time benchmarkEvalNoCompiling = 7270
time benchmarkEvalWithCompiling = 6914
*/