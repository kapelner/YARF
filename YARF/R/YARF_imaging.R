#' Illustrates trees to PNG files in the local directory.
#'  
#' @param yarf_mod						The YARF model whose trees are to be illustrated 
#' @param trees 						A vector of the tree indices (a subset of 1...num_trees)
#' @param background_rgb_color 			A vector with 3 elements in {0, 1, ..., 255} specifying the RGB color 
#' 										of the background. Default is \code{c(255, 255, 255)} for white.
#' @param line_rgb_color 				A vector with 3 elements in {0, 1, ..., 255} specifying the RGB color 
#' 										of the lines. Default is \code{c(100, 100, 100)} for grey.
#' @param text_rgb_color				A vector with 3 elements in {0, 1, ..., 255} specifying the RGB color 
#' 										of the text. Default is \code{c(0, 0, 0)} for black.
#' @param font_family 					The font family of the text in the illustration. This string is passed in 
#' 										as the first argument in a new \code{java.awt.Font} object. You can find 
#' 										a list of legal arguments in the Java 8 documentation. Default is \code{arial}.
#' @param font_size 					The font size in the illustrations in pixels. Default is \code{12}.
#' @param margin_in_px 					The margin in pixels of the entire illustration (equal on all sides). Default 
#' 										is \code{70}.
#' @param character_width_in_px 		Estimate as to the size of the character width in pixels. Differs based on fonts.
#' @param length_in_px_per_half_split 	The length of half of a split in pixels. Default is \code{20}.
#' @param depth_in_px_per_split 		The length of the depth of a split in pixels. Default is \code{100}.
#' @param title 						The name of the file. Note that "_00t.png" will be added where "t"
#' 										is the tree index. Default is "yarf_mod_tree".
#' 
#' @author Adam Kapelner
#' @export
illustrate_trees = function(yarf_mod,
		trees = NULL,
		background_rgb_color = c(255, 255, 255),
		line_rgb_color = c(100, 100, 100),
		text_rgb_color = c(0, 0, 0),
		font_family = "Arial",
		font_size = 12,
		margin_in_px = 70,
		character_width_in_px = 4.2,
		length_in_px_per_half_split = 20,
		depth_in_px_per_split = 100,
		title = "yarf_mod_tree"
	){
	
	if (is.null(trees)){
		trees = 1 : yarf_mod$num_trees
	}
	num_tree_digits = nchar(as.character(yarf_mod$num_trees))
	current_dir = getwd()
	
	for (t in trees){
		.jcall(yarf_mod$java_YARF, "V", "illustrateTree",
			as.integer(t - 1), #Java indexes from 0 not 1
			as.integer(background_rgb_color), 
			as.integer(line_rgb_color),  
			as.integer(text_rgb_color), 
			font_family,
			as.integer(font_size), 
			as.integer(margin_in_px), 
			character_width_in_px,
			as.integer(length_in_px_per_half_split),
			as.integer(depth_in_px_per_split),
			paste(title, "_", str_pad(t, num_tree_digits, pad = "0"), sep = "")
		)
	}
}

