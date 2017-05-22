package YARF;

import java.util.Random;

import org.apache.commons.math3.stat.StatUtils;

import gnu.trove.list.array.TIntArrayList;

public class YARFRandomness {
	
	/** A convenience for a Random object */
	private final Random R = new Random();
	public int num_int_ops = 0;
	public int num_double_ops = 0;
//	private int tree_num;

//	public YARFRandomness() {
//		tree_num = -1;
//	}
	
//	public YARFRandomness(int tree_num) {
//		this.tree_num = tree_num;
//	}

	/**
	 * Set the seed of the random number generator
	 * 
	 * @param seed	The seed
	 */
	public void setSeed(long seed) {
		R.setSeed(seed);
	}
	
	/** 
	 * A convenience method for a random object
	 * 
	 * @return	A random number drawn from a uniform distribution bounded between 0 and 1.
	 */
	public double rand(){
//		num_double_ops++;
//		double rand_double = R.nextDouble();
//		if (tree_num == 0){
//			System.out.println(" rand double #" + num_double_ops + ": " + rand_double);
//		}
//		return rand_double;
		return R.nextDouble();
	}	
	
	public int randInt(int i){
//		num_int_ops++;
//		int rand_int = R.nextInt(i);
//		if (tree_num == 0){
//			System.out.println(" rand int #" + num_int_ops + ": " + rand_int);
//		}
//		return rand_int;
		return R.nextInt(i);
	}
	
	public void shuffleArray(int[] array)
	{
	    int index, temp;
	    for (int i = array.length - 1; i > 0; i--)
	    {
	        index = randInt(i + 1);
	        temp = array[index];
	        array[index] = array[i];
	        array[i] = temp;
	    }
	}
	
	public void shuffleArray(double[] array)
	{
	    int index;
	    double temp;
	    for (int i = array.length - 1; i > 0; i--)
	    {
	        index = randInt(i + 1);
	        temp = array[index];
	        array[index] = array[i];
	        array[i] = temp;
	    }
	}
	
	//http://lemire.me/blog/2013/08/16/picking-n-distinct-numbers-at-random-how-to-do-it-fast/
	public int[] pickNRandomElements(int[] arr, int s) {
//		if (tree_num == 0){
//			System.out.println(" pickNRandomElements: " + Tools.StringJoin(arr) + " subset_size: " + s);
//		}
		
		shuffleArray(arr);
		int[] random_subset = new int[s];
		for (int i = 0; i < s; i++){ //take the first s elements
			random_subset[i] = arr[i];
		}
        return random_subset;

	}
	
	public double random_sample_mode(double[] arr){
		double[] modes = StatUtils.mode(arr); //there could be multiple modes
		return modes[randInt(modes.length)]; //return one at random in the spirit of "random forests"
	}
	

	/**
	 * Sample from a multinomial distribution
	 * 
	 * @param vals		The integer values of the labels in this multinomial distribution
	 * @param probs		The probabilities with which to sample the labels (must be the same length of the vals)
	 * @return			The integer label of the value that was drawn from this multinomial distribution
	 */
	public int multinomial_sample(TIntArrayList vals, double[] probs) {
		double r = rand();
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
}
