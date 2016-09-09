

function sample_avg(arr){
  	var sum = 0.0;
  	for (i = 0; i < arr.length; i++){
    	sum += arr[i];
  	}
  	return sum / arr.length;	
}


function median(arr){
    arr.sort(function(a, b) {return a - b;});
    var half = Math.floor(arr.length / 2);
    if (arr.length % 2)
      med = arr[half];
    else
      med = (arr[half - 1] + arr[half]) / 2.0;
    //print("arr: ", arr)
    //print("half: ", half)
    //print("median: ", median)
    return med; 
}
