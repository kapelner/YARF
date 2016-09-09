function assignYhatToNode(node){
    return median(Java.from(node.node_ys()));
}

function median(arr){
    arr.sort(function(a, b) {return a - b;});
    var half = Math.floor(arr.length / 2);
    if (arr.length % 2)
      median = arr[half];
    else
      median = (arr[half - 1] + arr[half]) / 2.0;
    //print("arr: ", arr)
    //print("half: ", half)
    //print("median: ", median)
    return median; 
}