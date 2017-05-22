package YARF;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

/**
 * The base class for all machine learning / statistical-learning
 * algorithms. Extend this class to add your own implementation.
 * 
 * @author Adam Kapelner
 */
public abstract class Classifier implements Serializable{
	private static final long serialVersionUID = 2471771656791064460L;

	/** Are we on a Windows machine (sometimes this matters) */
	public static final boolean ON_WINDOWS = System.getProperty("os.name").toLowerCase().indexOf("win") >= 0;
	
	/** The way we represent missing values from within our implementation */
	public static final double MISSING_VALUE = Double.NaN;	
	
	/** should we print select messages to the screen */
	protected boolean verbose = true;
	/**
	 * Is this value missing?
	 * 
	 * @param x	The value we wish to check if it is missing
	 * @return	True if the value is missing
	 */
	public static boolean isMissing(double x){
		return Double.isNaN(x);
	}
	
	/** an array of the raw training data by ROW i.e. consisting of xi = [xi1, ..., xiM, yi] */
	protected transient ArrayList<double[]> X;
	protected transient double[] y;
	/** the number of records in the training set */
	public int n;
	/** the number of features / predictors in the training set */
	public int p;
	/** in sample evaluation */
//	protected double[] residuals;
	protected String[] feature_names;
	protected int[] missingnessAmongFeatures;
	protected PrintStream printStreamToFile;

	
	/** A dummy constructor which keeps <code>Serializable</code> happy */
	public Classifier(){}
		
	/**
	 * Adds an observation / record to the training data array. The
	 * observation is converted to doubles and the entries that are 
	 * unrecognized are converted to {@link #MISSING_VALUE}'s.
	 * 
	 * @param x_i	The observation / record to be added as a String array.
	 */
	public void addTrainingDataRow(String[] x_i){
		//initialize data matrix if it hasn't been initialized already
		if (X == null){
			X = new ArrayList<double[]>();
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
		X.add(record);		
	}
	
	/**
	 * Ditto except for 1-d training matrices
	 * 
	 * @param x_i
	 */
	public void addTrainingDataRow(String x_i){
		String[] x_i_vec = {x_i};
		addTrainingDataRow(x_i_vec);	
	}
	
	public void addTrainingDataResponse(double[] y){
		this.y = y;
	}


	public void setTrainingDataNames(String feature_name){
		String[] feature_names = {feature_name};
		setTrainingDataNames(feature_names);
	}
	
	public void setTrainingDataNames(String[] feature_names){
		this.feature_names = feature_names;
	}
	
	
	/**
	 * This method finalizes the training data after all
	 * records have been added via the method {@link #addTrainingDataRow}.	
	 */
	public void finalizeTrainingData(){
		n = X.size();
		p = X.get(0).length;
		
		//reformulate the data by column for convenience
		
		//check for missingness
		missingnessAmongFeatures = new int[p];
		for (int i = 0; i < n; i++){
			double[] x_i = X.get(i);
			for (int j = 0; j < p; j++){
				if (isMissing(x_i[j])){
					missingnessAmongFeatures[j]++;
				}
			}
		}
	}


	
	/** build the machine learning classifier (implemented by a daughter class), you must {@link #setData(ArrayList) set the data} first */
	public abstract void Build();
	
	/**
	 * Useful for debugging only. Undocumented.
	 * 
	 * @see {@link https://blogs.oracle.com/nickstephen/entry/java_redirecting_system_out_and
	 */
	public void suppressOrWriteToDebugLog(){
		File file = new File("yarf.log");
		try {
			printStreamToFile = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)), true);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	    System.setOut(printStreamToFile);	
	    System.setErr(printStreamToFile);
	    //System.out.println("test");
	}
	
	/** deletes all data that's unneeded at this point in runtime in order to save memory */
	protected abstract void FlushData();

	
	/**
	 * A wrapper for {@link #Evaluate(double[], int)} where one processor core is used
	 * 
	 * @param record		The observation to be evaluated / predicted
	 * @return				The prediction
	 */
	public abstract double Evaluate(double[] record);
	
	/**
	 * Given a data record, return the Y (response) value i.e. take the last index
	 * 
	 * @param record		the data record
	 * @return				the data record's response value (or class)
	 */
	public double getResponseFromRecord(double[] record){
		return record[p];
	}

	/** Stop the classifier during its building phase */
	public abstract void StopBuilding();

	
	/** Useful for debugging. Undocumented */
	public void dumpDataToFile(String optional_title){
		PrintWriter out=null;
		try {
			out = new PrintWriter(new BufferedWriter(new FileWriter("data_out" + (optional_title == null ? "" : optional_title) + ".csv")));
		} catch (IOException e) {
			System.out.println("cannot be edited in CSV appending");
		}
		
		//print fileheader
		for (int j = 0; j < p; j++){
			out.print("," + j);
		}
		out.print(",y");
		out.print("\n");
		//now print the data
		for (int i = 0; i < n; i++){
			double[] record = X.get(i);
			for (int j = 0; j <= p; j++){
				out.print("," + record[j]);
			}
			out.print("," + y[i]);
			out.print("\n");
		}
		out.close();		
	}
	

	public void setVerbose(boolean verbose){
		this.verbose = verbose;
	}

	
	public void writeStdOutToLogFile(){
		try {
		  Logger.getLogger("").addHandler(new StreamHandler()); //turn off std out
		  suppressOrWriteToDebugLog();
		}
		catch (Error e){
			System.out.println("Logger and or suppressOrWriteToDebugLog FAILING\n");
		}    
 	}
}
