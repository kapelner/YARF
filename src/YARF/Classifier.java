package YARF;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.logging.FileHandler;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import CustomLogging.*;

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
	/** ? */
	protected boolean mem_cache_for_speed = true;
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
	protected double[] residuals;

	protected String[] feature_names;

	private String unique_name;

	protected int[] missingnessAmongFeatures;	

	
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
	
	public void addTrainingDataResponse(double[] y){
		this.y = y;
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
		//also handle the logging
        LogManager logManager = LogManager.getLogManager();
        logManager.reset();

        // create log file, no limit on size
        FileHandler fileHandler = null;
		try {
			fileHandler = new FileHandler(unique_name + ".log", Integer.MAX_VALUE, 1, false);
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
        fileHandler.setFormatter(new SuperSimpleFormatter());
        Logger.getLogger("").addHandler(fileHandler);
        
        
        // now rebind stdout/stderr to logger
        Logger logger = Logger.getLogger("stdout");         
        LoggingOutputStream  los = new LoggingOutputStream(logger, StdOutErrLevel.STDOUT);
        System.setOut(new PrintStream(los, true));
        logger = Logger.getLogger("stderr");                                    
        los = new LoggingOutputStream(logger, StdOutErrLevel.STDERR);            
        System.setErr(new PrintStream(los, true)); 		
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
	
//	/** a variable that represents the different error calculation types */
//	public static enum ErrorTypes {L1, L2, MISCLASSIFICATION};
//	
//	/**
//	 * Calculates the in-sample error using the specified loss function
//	 * 
//	 * @param num_cores_evaluate 	The number of processor cores to use
//	 */		
//	private void calculateInSampleResiduals(int num_cores_evaluate){
//		long t0 = System.currentTimeMillis();
//		System.out.print("calculating in-sample residuals...");
//		residuals = new double[n];
//		for (int i = 0; i < n; i++){
//			double[] record = X.get(i);
//			double yhat = Evaluate(record, num_cores_evaluate);
//			residuals[i] = y[i] - yhat;
////			System.out.println("y: " + y + " yhat: " + yhat);
//		}
//		long t1 = System.currentTimeMillis();
//		System.out.print("done in " + ((t1 - t0) / 1000.0) + " sec \n");
//	}
	
//	/**
//	 * Calculates the in-sample error based on a specified error metric
//	 * 
//	 * @param type_of_error_rate	The error metric to use to compute loss
//	 * @param num_cores_evaluate	The number of processor cores to use
//	 * @return						The in-sample loss as a sum total across all training observations
//	 */
//	public double calculateInSampleLoss(ErrorTypes type_of_error_rate, int num_cores_evaluate){	
//		if (residuals == null){
//			calculateInSampleResiduals(num_cores_evaluate);
//		}
//		
//		double loss = 0;
//		System.out.print("calculateInSampleLoss for " + type_of_error_rate + "...");
//		for (int i = 0; i < n; i++){
//			switch (type_of_error_rate){
//				case L1:
//					loss += Math.abs(residuals[i]);
//					break;
//				case L2:
//					loss += Math.pow(residuals[i], 2);
//					break;
//				case MISCLASSIFICATION:
//					loss += (residuals[i] == 0 ? 0 : 1);
//					break;
//			}
//		}
//		System.out.print("done\n");
////		System.out.println("in_sample_residuals: " + Tools.StringJoin(residuals));
//		return loss;
//	}
	
	public void setUniqueName(String unique_name) {
		this.unique_name = unique_name;
	}
	

	public void setVerbose(boolean verbose){
		this.verbose = verbose;
	}

	public void setSeed(int seed){
		StatToolbox.setSeed(seed);
	}
	
	public void setMemCacheForSpeed(boolean mem_cache_for_speed){
		this.mem_cache_for_speed = mem_cache_for_speed;
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
