function assignYhatToNode(node){
  var ys = Java.from(node.node_ys());
   //print("ys = ", ys);
  var avg = 0;
  for (i = 0; i < ys.length; i++){
    avg += ys[i];
  }
  return avg / ys.length + 0.0;
}