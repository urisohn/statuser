#' Resize Images
#'
#' Saves images to PNG with a specified width. As input it accepts (SVG, PDF, EPS, JPG, JPEG, TIF, TIFF, BMP, PNG)
#' Saves to subdirectory '/resized' within input folder (or same directory as file if input is a single file)
#'
#' @param path Character string. Path to a folder containing image files, or path to a single image file.
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
#' \donttest{
#' # Create a temporary PNG file and resize it
#' tmp_png <- tempfile(fileext = ".png")
#' grDevices::png(tmp_png, width = 400, height = 300)
#' old_par <- graphics::par(no.readonly = TRUE)
#' graphics::par(mar = c(2, 2, 1, 1))
#' graphics::plot(1:2, 1:2, type = "n")
#' grDevices::dev.off()
#' graphics::par(old_par)
#' resize_images(tmp_png, width = 80)
#' }
#'
#' @export
resize_images <- function(path, width) {

  # Dependencies: rsvg, magick, tools

  

  # Check if input is a file or folder

  if (file.exists(path) && !dir.exists(path)) {

    # Input is a single file

    files <- path

    # Output directory is the same directory as the file

    outdir <- file.path(dirname(path), "resized")

  } else if (dir.exists(path)) {

    # Input is a folder

    # Find files in folder

    files <- list.files(

      path,

      pattern = "\\.(svg|pdf|eps|jpg|jpeg|tif|tiff|bmp|png)$",

      ignore.case  = TRUE,

      full.names   = TRUE

    )

    

    # Output directory is subfolder within input folder

    outdir <- file.path(path, "resized")

  } else {

    stop("Path does not exist or is not a valid file or folder.")

  }

  

  # Ensure output subfolder exists

  if (!dir.exists(outdir)) dir.create(outdir)

  

  if (length(files) == 0) stop("No supported image/figure files found.")

  

  # Recycle width if scalar

  if (length(width) == 1) {

    width <- rep(width, length(files))

  } else if (length(width) != length(files)) {

    # Print error message in red
    message2("Error", col = "red", font = 2)
    message2("Found ", length(files), " image(s), but ", length(width), " width(s) were provided.", col = "red")
    
    # List files with counter
    for (j in seq_along(files)) {
      message2("  ", j, ". ", basename(files[j]), col = "red")
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
      message2("Resized '", new_name, "'", col = "blue")

  }

  

  invisible(TRUE)

}

