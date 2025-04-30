package YARF;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.NumberFormat;

import javax.imageio.ImageIO;

/**
 * This class builds illustrations of a tree. Since it is only used
 * as a debugging feature, it is undocumented
 * 
 * @author Adam Kapelner
 */
public class YARFTreeIllustrate {

	/** useful to have around for queries */
	private YARF yarf;
	
	/** customization parameters set by the user */
	private String font_family;
	private Color background_color;
	private Color line_color;
	private Color text_color;
	private int font_size;
	private int margin_in_px;
	private double character_width_in_px;
	private int length_in_px_per_half_split;
	private int depth_in_px_per_split;
	
	public static NumberFormat two_digit_format = NumberFormat.getInstance();
	static {
		two_digit_format.setMaximumFractionDigits(2);
		two_digit_format.setMinimumFractionDigits(2);
		two_digit_format.setGroupingUsed(false);
	}
	
	/** the illustration image */
	private transient BufferedImage canvas;
	/** useful info for drawing */
	private int depth_in_num_splits;
	private int image_type;

	private boolean use_real_names;

	public static BufferedImage crop(BufferedImage image, int margin_in_px, int background_color_rgb) {
		int x1 = image.getWidth();
		int x2 = 0;
		int y1 = image.getHeight();
		int y2 = 0;
		
		for (int i = 0; i < image.getWidth(); i++) {
			for (int j = 0; j < image.getHeight(); j++) {
				if (image.getRGB(i, j) != background_color_rgb) {
					if (i < x1) {
						x1 = i;
					}
					if (i > x2) {
						x2 = i;
					}
					if (j < y1) {
						y1 = j;
					}
					if (j > y2) {
						y2 = j;
					}					
				}
			}
		}
		//handle the margins
		x1 = Math.max(0, x1 - margin_in_px);
		x2 = Math.min(image.getWidth(), x2 + margin_in_px);
		y1 = Math.max(0, y1 - margin_in_px);
		y2 = Math.min(image.getHeight(), y2 + margin_in_px);
		//finally crop it
		return image.getSubimage(x1, y1, x2 - x1, y2 - y1);
	}

	public YARFTreeIllustrate(YARF yarf,
			YARFNode root, 
			Integer max_depth,
			int[] background_color, 
			int[] line_color, 
			int[] text_color, 
			String font_family,
			int font_size, 
			int margin_in_px, 
			double character_width_in_px,
			int length_in_px_per_half_split,
			int depth_in_px_per_split,
			String file_format,
			boolean use_real_names,
			String title) {
		
		this.yarf = yarf;
		this.background_color = new Color(background_color[0], background_color[1], background_color[2]);
		this.line_color = new Color(line_color[0], line_color[1], line_color[2]);
		this.text_color = new Color(text_color[0], text_color[1], text_color[2]);
		this.font_family = font_family;
		this.font_size = font_size;
		this.margin_in_px = margin_in_px;
		this.character_width_in_px = character_width_in_px;
		this.length_in_px_per_half_split = length_in_px_per_half_split;
		this.depth_in_px_per_split = depth_in_px_per_split;
		this.use_real_names = use_real_names;
		
		depth_in_num_splits = max_depth <= 0 ? root.maxDepth() : Math.min(max_depth, root.maxDepth());
//		System.out.println("YARFTreeIllustrate with depth: " + depth_in_num_splits);
		
		initializeCanvas();
		if (root.is_leaf) {
			System.out.println("YARFTreeIllustrate root.is_leaf");
			drawLeaf(root, canvas.getWidth() / 2, margin_in_px);
		} else {
			//recursively draw all splits, start drawing on top and horizontally in the middle
			drawSplit(root, canvas.getWidth() / 2, margin_in_px);			
		}


		//write to file
		saveCanvasAsImageFile(title, file_format);
	}

	private void saveCanvasAsImageFile(String title, String file_format) {		
		try {
			File f = new File(title + "." + file_format);
			//render the cropped image
			ImageIO.write(crop(canvas, margin_in_px, background_color.getRGB()), file_format, f);
//			System.out.println("after write: " + f.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void initializeCanvas() {
		int w = 2 * margin_in_px + length_in_px_per_half_split * 2 * (int)Math.pow(2, depth_in_num_splits) * 2;
		int h = 2 * margin_in_px + depth_in_num_splits * depth_in_px_per_split;
		image_type = BufferedImage.TYPE_INT_RGB;
		canvas = new BufferedImage(w, h, image_type);
		//set the background
		for (int i = 0; i < w; i++){
			for (int j = 0; j < h; j++){
				canvas.setRGB(i, j, background_color.getRGB());
			}		
		}
	}
	
	private Graphics getAndSetupCanvasGraphics() {
//		System.out.println("drawSplit at " + node.stringLocation(true));
		Graphics g = canvas.getGraphics();
		//now set up canvas for drawing the foreground
		g.setFont(new Font(font_family, Font.PLAIN, font_size));
		g.setColor(text_color);
		return g;
	}

	private void drawSplit(YARFNode node, int x, int y) {
		Graphics g = getAndSetupCanvasGraphics();
		if (YARF.DEBUG) {
			System.out.println("Drawing split for node " + node.stringLocation());
		}
		if (node.depth >= depth_in_num_splits){
			if (YARF.DEBUG) {
				System.out.println("max depth (" + depth_in_num_splits + ") reached with depth " + node.depth + " aborting");
			}
			return;
		}


		String rule_and_n = "";
		//paint a leaf node
		if (node.is_leaf){
			drawLeaf(node, x, y);
		}
		//paint a split node
		else if (node.split_value == null) {
			int attr = node.split_attribute;
			rule_and_n = "M_" +
					(use_real_names ? yarf.feature_names[attr] : (attr + 1)) +
							"-> (" + node.nodeSize() + ") " +
							(node.y_pred != YARFNode.BAD_FLAG_double ? two_digit_format.format(node.y_pred) : "");

		}
		else if (node.split_attribute != YARFNode.BAD_FLAG_int && node.split_value != YARFNode.BAD_FLAG_double) {
			int attr = node.split_attribute;
			double val = node.split_value;
			rule_and_n =
					(use_real_names ? yarf.feature_names[attr] : ("X_" + (attr + 1))) +
					" <= " +
					two_digit_format.format(val) +
					(node.send_missing_data_right ? " M->" : " <-M") +
					" (" + node.nodeSize() + ") " +
					(node.y_pred != YARFNode.BAD_FLAG_double ? two_digit_format.format(node.y_pred) : "");
		}
		else if (node.split_attribute == YARFNode.BAD_FLAG_int) {
			rule_and_n = "j_is_null" +
					(node.send_missing_data_right ? " M->" : " <-M") +
					" (" + node.nodeSize() + ") " +
					(node.y_pred != YARFNode.BAD_FLAG_double ? two_digit_format.format(node.y_pred) : "");
		}
		else {
			int attr = node.split_attribute;
			rule_and_n = "M_" +
					(use_real_names ? yarf.feature_names[attr] : (attr + 1)) +
							(node.send_missing_data_right ? " ->" : " <-") +
							" (" + node.nodeSize() + ") " +
							(node.y_pred != YARFNode.BAD_FLAG_double ? two_digit_format.format(node.y_pred) : "");
		}
		int draw_x = (int)Math.round(x - rule_and_n.length() / 2.0 * character_width_in_px);
		g.drawString(rule_and_n, draw_x, y - font_size / 2);
		if (yarf.customFunctionPrintAtSplitNode()){
			drawStringWithBreaklines(g, "\n" + yarf.runPrintAtSplitNode(node), draw_x, y);
		}
		//now we have to recurse to draw the left and right
		g.setColor(line_color);
		int x_offset = length_in_px_per_half_split * (int)Math.pow(2, depth_in_num_splits - node.depth);
		if (node.left != null){
			g.drawLine(x, y, x - x_offset, y);
			g.drawLine(x - x_offset, y, x - x_offset, y + depth_in_px_per_split);
			drawSplit(node.left, x - x_offset, y + depth_in_px_per_split);
		}
		if (node.right != null){
			g.drawLine(x, y, x + x_offset, y);
			g.drawLine(x + x_offset, y, x + x_offset, y + depth_in_px_per_split);
			drawSplit(node.right, x + x_offset, y + depth_in_px_per_split);
		}
	}
	
	private void drawLeaf(YARFNode node, int x, int y) {
		if (YARF.DEBUG) {
			System.out.println("Drawing leaf");
		}
		String pred = two_digit_format.format(node.y_pred);//;		
		int draw_x = (int)Math.round(x - pred.length() / 2.0 * character_width_in_px);
		if (yarf.customFunctionPrintAtLeafNode()){
			drawStringWithBreaklines(getAndSetupCanvasGraphics(), "\n" + yarf.runPrintAtLeafNode(node), draw_x, y);
		} else {
			getAndSetupCanvasGraphics().drawString("Leaf: " + pred + " (" + node.nodeSize() + ") ", draw_x, y + font_size);
		}
	}
	
    private void drawStringWithBreaklines(Graphics g, String text, int x, int y) {
        for (String line : text.split("\n")){
            g.drawString(line, x, y += g.getFontMetrics().getHeight());
        }
    }
}
