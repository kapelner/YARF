function assignYhatToNode(node){
    var ys = node.node_ys;
    ys.sort(function(a, b) {return a - b;});
    var half = Math.floor(ys / 2);
    if (ys % 2)
      return ys[half];
    else
      return (ys[half - 1] + ys[half]) / 2.0;
}