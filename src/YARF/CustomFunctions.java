package YARF;

import java.io.FileNotFoundException;
import java.io.FileReader;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class CustomFunctions {

	public static void main(String[] args) throws ScriptException, FileNotFoundException, NoSuchMethodException{
		ScriptEngine nashorn_js_engine = new ScriptEngineManager().getEngineByName("nashorn");
		int a = 5;
		int b = 3;
		
        Compilable compilingEngine = (Compilable) nashorn_js_engine;
        CompiledScript cscript = compilingEngine.compile(new FileReader("script.js"));
//        CompiledScript cscript = compilingEngine.compile("function foo(a,b){a+b}");
        Bindings scriptParams = nashorn_js_engine.getBindings(ScriptContext.ENGINE_SCOPE);
//        scriptParams.put("a", a);
//        scriptParams.put("b", b);
//        double res = (double)cscript.eval(scriptParams);
//        cscript.eval(scriptParams);
        cscript.eval(scriptParams);
        Invocable invocable = (Invocable) cscript.getEngine();
        
        
//		nashorn_js_engine.eval(new FileReader("script.js"));
//        double res = (double)((Invocable)nashorn_js_engine).invokeFunction("foo", a, b);
        
//        engine.eval(new FileReader("script.js"));
//        double res = (double)nashorn_js_engine.eval("foo(" + a + "," + b + ")");
        
//      Invocable invocable = (Invocable) script.getEngine();
//      double res = (double)((Invocable)engine).invokeFunction("foo", a, b);
      
        double res = (double)invocable.invokeFunction("foo", a, b);
        
//        double res = 0;
        System.out.println("res = " + res);
        
//		engine.eval();		
	}
}
