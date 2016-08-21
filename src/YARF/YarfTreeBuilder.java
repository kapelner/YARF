package YARF;

public class YarfTreeBuilder {

	private YARFTree tree;
	private YARF yarf;

	public YarfTreeBuilder(YARFTree tree) {
		this.tree = tree;
		this.yarf = tree.yarf;
		
		splitNode(tree.root);
	}

	private void splitNode(YARFNode node) {
		if (nodeTooSmall(node)){
			return;
		}
		
		for (int j : selectAttributesToTry(node)){
			
		}
	}

	private boolean nodeTooSmall(YARFNode node) {
		// TODO Auto-generated method stub
		return false;
	}

	private int[] selectAttributesToTry(YARFNode node) {
		if ()
	}

}
