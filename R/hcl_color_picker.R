#' Graphical User Interface to Pick Colors in HCL Space
#' 
#' The app visualizes colors either along the hue-chroma plane for a given luminance value or along the
#' luminance-chroma plane for a given hue. Colors can be entered by specifying the hue (H), chroma (C),
#' and luminance (L) values via sliders, by entering an RGB hex code, or by clicking on a color in the
#' hue-chroma or luminance-chroma plane. It is also possible to select individual colors and add them
#' to a palette for comparison and future reference. 
#'
#' \code{choose_color} is a convenience alias for \code{hcl_color_picker} to
#' go along with \code{\link{choose_palette}}. Another alias is \code{hclcolorpicker}.
#' 
#' @param shiny.trace logical: used for debugging the shiny interface.
#' @author Claus O. Wilke, Reto Stauffer, Achim Zeileis
#' @seealso \code{\link{choose_palette}}
#' @keywords misc
#'
#' @return \code{hclcolorpicker} invisibly returns a vector of colors choosen.
#'    If no colors have been selected \code{NULL} will be returned.
#' @examples
#' \dontrun{
#' hcl_color_picker()
#' }
#' @export
hcl_color_picker <- function(shiny.trace = FALSE) {
   # Requirements for shiny application
   stopifnot(requireNamespace("shiny"), requireNamespace("shinyjs"))
   appDir <- system.file("hclcolorpicker", package = "colorspace")
   if (appDir == "")
      stop("Could not find hclcolorpicker app directory. Try re-installing `colorspace`.", call. = FALSE)
   # Start shiny
   options(shiny.trace = shiny.trace)
   pal <- shiny::runApp(appDir, display.mode = "normal", quiet = TRUE )
   return(pal)
}


#' @rdname hcl_color_picker
#' @export
choose_color <- function(shiny.trace = FALSE)
    hcl_color_picker(shiny.trace = shiny.trace)

#' @rdname hcl_color_picker
#' @usage NULL
#' @export
hclcolorpicker <- function(shiny.trace = FALSE)
    hcl_color_picker(shiny.trace = shiny.trace)

