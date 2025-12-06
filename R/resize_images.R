#' Resize Images
#'
#' Saves images to PNG with a specified width. As input it accepts (SVG, PDF, EPS, JPG, JPEG, TIF, TIFF, BMP, PNG)
#' Saves to subdirectory '/resized' within input folder
#'
#' @param folder Character string. Path to the folder containing image files.
#' @param width Numeric vector. Target width(s) in pixels for the output PNG files.
#'   Can be a single value (recycled for all files) or a vector matching the number
#'   of files found.
#'
#' @return Invisibly returns TRUE on success.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Searches for image files with extensions: svg, pdf, eps, jpg, jpeg, tif, tiff, bmp, png
#'   \item Creates a "resized" subfolder in the target directory if it doesn't exist
#'   \item Converts each file to PNG format at the specified width(s)
#'   \item Saves output files as: \code{originalname_width.png} in the resized subfolder
#' }
#'
#' Supported input formats:
#' \itemize{
#'   \item Vector graphics: SVG, PDF, EPS (rasterized using rsvg/magick)
#'   \item Raster images: JPG, JPEG, TIF, TIFF, BMP, PNG
#' }
#'
#' @note
#' Dependencies required: \code{rsvg}, \code{magick}, and \code{tools} (base R).
#' SVG files are rasterized using \code{rsvg::rsvg()}, while PDF/EPS and other
#' formats are handled by \code{magick::image_read()}.
#'
#' @examples
#' \dontrun{
#' # Resize all images in a folder to 800px width
#' resize_images("path/to/images", width = 800)
#'
#' # Resize images to different widths
#' resize_images("path/to/images", width = c(800, 1200, 600))
#' }
#'
#' @export
resize_images <- function(folder, width) {

  # Dependencies: rsvg, magick, tools

  

  # Ensure output subfolder exists

  outdir <- file.path(folder, "resized")

  if (!dir.exists(outdir)) dir.create(outdir)

  

  # Find files

  files <- list.files(

    folder,

    pattern = "\\.(svg|pdf|eps|jpg|jpeg|tif|tiff|bmp|png)$",

    ignore.case  = TRUE,

    full.names   = TRUE

  )

  

  

  

  

  if (length(files) == 0) stop("No supported image/figure files found.")

  

  # Recycle width if scalar

  if (length(width) == 1) {

    width <- rep(width, length(files))

  } else if (length(width) != length(files)) {

    # Print error message in red
    message.col("Error", col = "red", font = 2)
    message.col("The folder contains ", length(files), " images, but ", length(width), " widths were provided.", col = "red")
    
    # List files with counter
    for (j in seq_along(files)) {
      message.col("  ", j, ". ", basename(files[j]), col = "red")
    }
    
    stop("Width vector length must match number of files or be scalar.")

  }

  

  for (i in seq_along(files)) {

    f <- files[i]

    ext <- tolower(tools::file_ext(f))

    

    # Read + rasterize SVG/PDF/EPS

    if (ext == "svg") {

      img <- magick::image_read(rsvg::rsvg(f))

    } else if (ext %in% c("pdf", "eps")) {

      img <- magick::image_read(f)

    } else {

      img <- magick::image_read(f)

    }

    

    # Resize

    resized <- magick::image_resize(img, paste0(width[i], "x"))

    

    # Output file in resized/

    out <- file.path(

      outdir,

      paste0(tools::file_path_sans_ext(basename(f)), "_", width[i], ".png")

    )

    

    magick::image_write(resized, out, format = "png")
    
    # Print progress message in blue
      new_name <- basename(out)
      message.col("Resized '", new_name, "'", col = "blue")

  }

  

  invisible(TRUE)

}

