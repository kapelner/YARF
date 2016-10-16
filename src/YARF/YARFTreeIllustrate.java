package YARF;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FilenameFilter;
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
	
	public static NumberFormat one_digit_format = NumberFormat.getInstance();
	public static NumberFormat two_digit_format = NumberFormat.getInstance();
	static {
		one_digit_format.setMaximumFractionDigits(1);
		one_digit_format.setGroupingUsed(false);
		two_digit_format.setMaximumFractionDigits(2);
		two_digit_format.setGroupingUsed(false);
	}
	
	/** the illustration image */
	private transient BufferedImage canvas;
	/** useful info for drawing */
	private int depth_in_num_splits;
	private int image_type;

	public YARFTreeIllustrate(YARFNode root, 
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
			String title) {
		
		this.background_color = new Color(background_color[0], background_color[1], background_color[2]);
		this.line_color = new Color(line_color[0], line_color[1], line_color[2]);
		this.text_color = new Color(text_color[0], text_color[1], text_color[2]);
		this.font_family = font_family;
		this.font_size = font_size;
		this.margin_in_px = margin_in_px;
		this.character_width_in_px = character_width_in_px;
		this.length_in_px_per_half_split = length_in_px_per_half_split;
		this.depth_in_px_per_split = depth_in_px_per_split;
		
		depth_in_num_splits = max_depth <= 0 ? root.maxDepth() : Math.min(max_depth, root.maxDepth());
		
		initializeCanvas();
		//recursively draw all splits, start drawing on top and horizontally in the middle
		drawSplit(root, canvas.getWidth() / 2, margin_in_px);
		//write to file
		saveImageFile(canvas, title);
	}
	
//	private BufferedImage cropCanvas() {
//		int x_min = Integer.MAX_VALUE;
//		int x_max = Integer.MIN_VALUE;
//		
//		//first get min and max
//		for (int i = 0; i < canvas.getWidth(); i++){
//			for (int j = 0; j < canvas.getHeight(); j++){
//				if (canvas.getRGB(i, j) == line_color.getRGB()){
//					if (i > x_max){
//						x_max = i;
//					}
//					if (i < x_min){
//						x_min = i;
//					}
//				}
//			}
//		}
//		//create the new image
//		int new_width = x_max - x_min + 2 * margin_in_px;
//		BufferedImage new_canvas = new BufferedImage(new_width, canvas.getHeight(), image_type);
//		//first do the background
//		for (int i = 0; i < new_width; i++){
//			for (int j = 0; j < canvas.getHeight(); j++){
//				new_canvas.setRGB(i, j, background_color.getRGB());
//			}		
//		}
//		//now copy the old into the new
//		for (int i = x_min; i <= x_max; i++){
//			for (int j = 0; j < canvas.getHeight(); j++){
//				new_canvas.setRGB(i - x_min + margin_in_px, j, canvas.getRGB(i, j));
//			}
//		}
//		//and send it to be saved
//		return new_canvas;
//	}
	
	/**
	 * This {@link java.io.FilenameFilter file filter} returns
	 * only image files of type "jpg", "tif", "tiff, and "bmp"
	 *
	 */
	public static class ImageFileFilter implements FilenameFilter{
		/**
		 * Given a file, returns true if it is an image
		 * 
		 * @param dir		the directory the file is located in
		 * @param name		the file itself
		 * @return			whether or not the file is an image
		 */
		public boolean accept(File dir, String name) {
			String[] fileparts=name.split("\\.");
			if (fileparts.length >= 2){
				String ext=fileparts[fileparts.length - 1].toLowerCase();
				if (ext.equals("jpg") || ext.equals("tif") || ext.equals("tiff") || ext.equals("TIFF") || ext.equals("bmp") || ext.equals("png"))
					return true;
				else 
					return false;
			}
			else return false;
		}		
	}

	private void saveImageFile(BufferedImage image, String title) {
		try {
			ImageIO.write(image, "PNG", new File(title + ".png"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void initializeCanvas() {
		int w = 2 * margin_in_px + length_in_px_per_half_split * 2 * (int)Math.pow(2, depth_in_num_splits) * 2;
		int h = 2 * margin_in_px + depth_in_num_splits * depth_in_px_per_split;
		initializeCanvas(w, h);
	}
	
	private void initializeCanvas(int w, int h) {
//		if ((line_color.equals(Color.WHITE) && background_color.equals(Color.BLACK)) || (line_color.equals(Color.BLACK) && background_color.equals(Color.WHITE))){
//			image_type = BufferedImage.TYPE_BYTE_BINARY;
//		}
//		else {
			image_type = BufferedImage.TYPE_INT_RGB;
//		}
		canvas = new BufferedImage(w, h, image_type);
		//first do the background
		for (int i = 0; i < w; i++){
			for (int j = 0; j < h; j++){
				canvas.setRGB(i, j, background_color.getRGB());
			}		
		}

	}	

	private void drawSplit(YARFNode node, int x, int y) {
		Graphics g = canvas.getGraphics();
		//now set up canvas for drawing the foreground
		g.setFont(new Font(font_family, Font.PLAIN, font_size));
		g.setColor(text_color);
		
		if (node.is_leaf && node.y_pred != YARFNode.BAD_FLAG_double){
			String pred = two_digit_format.format(node.y_pred);//;
			int draw_x = (int)Math.round(x - pred.length() / 2.0 * character_width_in_px);
			g.drawString(pred + " (" + node.nodeSize() + ") ", draw_x, y + font_size);
		}
		
		if (node.depth >= depth_in_num_splits){
			return;
		}		
		
		if (node.split_attribute != YARFNode.BAD_FLAG_int && node.split_value != YARFNode.BAD_FLAG_double) {
			int attr = node.split_attribute;
			double val = node.split_value;
			String rule_and_n = "X_" + (attr + 1) + " < " + two_digit_format.format(val) + 
					(node.send_missing_data_right ? " M>" : " <M") +
					" (" + node.nodeSize() + ") " + 
					(node.y_pred != YARFNode.BAD_FLAG_double ? two_digit_format.format(node.y_pred) : "");
			int draw_x = (int)Math.round(x - rule_and_n.length() / 2.0 * character_width_in_px);
			g.drawString(rule_and_n, draw_x, y - font_size / 2);

		}
		//now we have to draw the left and right
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
}
