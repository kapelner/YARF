package YARF;

import gnu.trove.list.array.TDoubleArrayList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.hash.TDoubleIntHashMap;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Random;

import org.apache.commons.math3.stat.StatUtils;

/**
 * This is a class where we're going to put all sorts of useful functions
 * as a utility-style class
 * 
 * @author Adam Kapelner
 */
public class StatToolbox {
	
	/** A convenience for a Random object */
	private static final Random R = new Random();
	/** A flag that indicates an illegal value or failed operation */
	public static final double ILLEGAL_FLAG = -999999999;	

	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(double[] y){
		double y_bar = 0;
		for (int i = 0; i < y.length; i++){
			y_bar += y[i];
		}
		return y_bar / (double)y.length;
	}

	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(TDoubleArrayList y){
		double y_bar = 0;
		for (int i = 0; i < y.size(); i++){
			y_bar += y.get(i);
		}
		return y_bar / (double)y.size();
	}	
	
	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(int[] y){
		double y_bar = 0;
		for (int i = 0; i < y.length; i++){
			y_bar += y[i];
		}
		return y_bar / (double)y.length;
	}

	/**
	 * Compute the sample median of a vector of data
	 * 
	 * @param arr	The vector of data values
	 * @return		The sample median
	 */
	public static double sample_median(double[] arr) {
		int n = arr.length;
		Arrays.sort(arr);
		if (n % 2 == 0){
			double a = arr[n / 2];
			double b = arr[n / 2 - 1];
			return (a + b) / 2;
		}
		else {
			return arr[(n - 1) / 2];
		}
		
	}
	
	/**
	 * Compute the sample standard deviation of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample standard deviation
	 */
	public static final double sample_standard_deviation(int[] y){
		double y_bar = sample_average(y);
		double sum_sqd_deviations = 0;
		for (int i = 0; i < y.length; i++){
			sum_sqd_deviations += Math.pow(y[i] - y_bar, 2);
		}
		return Math.sqrt(sum_sqd_deviations / ((double)y.length - 1));		
	}
	
	/**
	 * Compute the sample standard deviation of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample standard deviation
	 */
	public static final double sample_standard_deviation(double[] y){
		return Math.sqrt(sample_variance(y));
	}	
	
	/**
	 * Compute the sample variance of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample variance
	 */
	public static final double sample_variance(double[] y){
		return sample_sum_sq_err(y) / ((double)y.length - 1);		
	}	
	


	public static double sample_sum_sq_err(double[] ys, double y_center_point) {
		double sum_sqd_deviations = 0;
		for (int i = 0; i < ys.length; i++){
			sum_sqd_deviations += Math.pow(ys[i] - y_center_point, 2);
		}
		return sum_sqd_deviations;
	}
	
	/**
	 * Compute the sum of squared error (the squared deviation from the sample average) of a vector of data
	 * 
	 * @param ys	The vector of data values
	 * @return		The sum of squared error
	 */	
	public static final double sample_sum_sq_err(double[] ys){
		double y_bar = sample_average(ys);
		double sum_sqd_deviations = 0;
		for (int i = 0; i < ys.length; i++){
			sum_sqd_deviations += Math.pow(ys[i] - y_bar, 2);
		}
		return sum_sqd_deviations;
	}

	/**
	 * Compute the sample minimum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample minimum
	 */
	public static double sample_minimum(int[] y) {
		int min = Integer.MAX_VALUE;
		for (int y_i : y){
			if (y_i < min){
				min = y_i;
			}
		}
		return min;
	}

	/**
	 * Compute the sample maximum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample maximum
	 */
	public static double sample_maximum(int[] y) {
		int max = Integer.MIN_VALUE;
		for (int y_i : y){
			if (y_i > max){
				max = y_i;
			}
		}
		return max;		
	}

	/**
	 * Compute the sample minimum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample minimum
	 */
	public static double sample_minimum(double[] y){
		double min = Double.MAX_VALUE;
		for (double y_i : y){
			if (y_i < min){
				min = y_i;
			}
		}
		return min;		
	}
	
	/**
	 * Compute the sample maximum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample maximum
	 */
	public static double sample_maximum(double[] y){
		double max = Double.NEGATIVE_INFINITY;
		for (double y_i : y){
			if (y_i > max){
				max = y_i;
			}
		}
		return max;			
	}
	
	/**
	 * Given an array, return the index of the maximum value
	 * 
	 * @param y		The vector of data value
	 * @return		The index of the greatest value in the array
	 */
	public static int FindMaxIndex(int[] y){
		int index = 0;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < y.length; i++){
			if (y[i] > max){
				max = y[i];
				index = i;
			}				
		}
		return index;
	}

	/**
	 * Sample from a multinomial distribution
	 * 
	 * @param vals		The integer values of the labels in this multinomial distribution
	 * @param probs		The probabilities with which to sample the labels (must be the same length of the vals)
	 * @return			The integer label of the value that was drawn from this multinomial distribution
	 */
	public static int multinomial_sample(TIntArrayList vals, double[] probs) {
		double r = StatToolbox.rand();
		double cum_prob = 0;
		int index = 0;
		if (r < probs[0]){
			return vals.get(0);
		}
		while (true){			
			cum_prob += probs[index];
			if (r > cum_prob && r < cum_prob + probs[index + 1]){
				return vals.get(index + 1);
			}
			index++;
		}
	}

	/**
	 * Set the seed of the random number generator
	 * 
	 * @param seed	The seed
	 */
	public static void setSeed(long seed) {
		R.setSeed(seed);
	}
	
	/** 
	 * A convenience method for a random object
	 * 
	 * @return	A random number drawn from a uniform distribution bounded between 0 and 1.
	 */
	public static double rand(){
		return R.nextDouble();
	}	
	
	public static int randInt(int i){
		return R.nextInt(i);
	}
	
	//http://lemire.me/blog/2013/08/16/picking-n-distinct-numbers-at-random-how-to-do-it-fast/
	public static int[] pickNRandomElements(int[] arr, int subset_size) {
		BitSet bs = new BitSet(arr.length);
        int cardinality = 0;
        while(cardinality < subset_size) {
        	int v = randInt(arr.length);
        	if (!bs.get(v)) {
        		bs.set(v);
        		cardinality++;
        	}
        }
        return bs.stream().toArray();
	}
	
	public static double sample_mode(double[] arr){
		double[] modes = StatUtils.mode(arr); //there could be multiple modes
		return modes[StatToolbox.randInt(modes.length)]; //return one at random in the spirit of "random forests"
	}

	public static double natural_negative_entropy(double[] data) {
		double n = (double)data.length;
		TDoubleIntHashMap y_freqs = class_freqs(data);
		double prop_entropy = 0;
		for (int class_freq : y_freqs.values()){
			if (class_freq > 0){
				double p_hat = class_freq / n;			
				prop_entropy += p_hat * Math.log(p_hat);
			}
		}
		return prop_entropy;
	}

	public static TDoubleIntHashMap class_freqs(double[] data) {
		TDoubleIntHashMap class_proportions = new TDoubleIntHashMap();
		for (double d : data){			
			if (!class_proportions.containsKey(d)){
				class_proportions.put(d, 1);
			} 
			else {
				class_proportions.put(d, class_proportions.get(d) + 1);
			}
		}
		return class_proportions;
	}
}
