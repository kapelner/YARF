package YARF;

import java.io.FileNotFoundException;
import java.io.FileReader;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class CustomFunctions {

	public static void main(String[] args) throws ScriptException, FileNotFoundException{
		ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
		int a = 5;
		int b = 3;
		engine.eval(new FileReader("script.js"));
	}
}
