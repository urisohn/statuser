#' Convert CSV file to SQL INSERT statements
#' 
#' Reads a CSV file and generates SQL statements to insert all rows. Optionally
#' can also generate a CREATE TABLE statement. The function automatically infers
#' column types (REAL for numeric, DATE for date strings matching YYYY-MM-DD
#' format, TEXT otherwise).
#' 
#' @param input Character string. Path to the input CSV file.
#' @param output Character string. Path to the output SQL file where the
#'   statements will be written.
#' @param create_table Logical. If \code{TRUE}, includes a CREATE TABLE
#'   statement before the INSERT statements. Default is \code{FALSE}.
#' 
#' @return Invisibly returns NULL. The function writes SQL statements to the
#'   specified output file.
#' 
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the CSV file using \code{read.csv()} with
#'     \code{stringsAsFactors = FALSE}
#'   \item Infers SQL column types:
#'     \itemize{
#'       \item Numeric columns become \code{REAL}
#'       \item Date columns (matching YYYY-MM-DD format) become \code{DATE}
#'       \item All other columns become \code{TEXT}
#'     }
#'   \item If \code{create_table = TRUE}, generates a \code{CREATE TABLE}
#'     statement using the base filename (without extension) as the table name
#'   \item Generates \code{INSERT INTO} statements for each row
#'   \item Writes all SQL statements to the output file
#' }
#' 
#' Single quotes in text values are escaped by doubling them (SQL standard).
#' Numeric values are inserted without quotes, while text and date values are
#' wrapped in single quotes.
#' 
#' @examples
#' # Convert a CSV file to SQL (INSERT statements only)
#' tmp_csv <- tempfile(fileext = ".csv")
#' tmp_sql <- tempfile(fileext = ".sql")
#' write.csv(
#'   data.frame(id = 1:2, value = c("a", "b"), date = c("2024-01-01", "2024-02-02")),
#'   tmp_csv,
#'   row.names = FALSE
#' )
#' convert_to_sql(tmp_csv, tmp_sql)
#' 
#' # Convert a CSV file to SQL with CREATE TABLE statement
#' convert_to_sql(tmp_csv, tmp_sql, create_table = TRUE)
#' 
#' @export
convert_to_sql <- function(input, output, create_table = FALSE) {
  
  # --- load CSV ---
  df <- read.csv(input, stringsAsFactors = FALSE)
  
  # table name from file
  table_name <- tools::file_path_sans_ext(basename(input))
  
  # --- infer SQL types ---
  infer_type <- function(x) {
    if (is.numeric(x)) return("REAL")
    if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) return("DATE")
    return("TEXT")
  }
  
  types <- sapply(df, infer_type)
  
  # --- CREATE TABLE (optional) ---
  create_stmt <- NULL
  if (create_table) {
    fields <- paste0("  `", names(types), "` ", types, collapse = ",\n")
    create_stmt <- paste0("CREATE TABLE `", table_name, "` (\n", fields, "\n);\n\n")
  }
  
  # --- INSERT rows ---
  escape <- function(x) gsub("'", "''", x)
  
  rows <- apply(df, 1, function(r) {
    vals <- sapply(seq_along(r), function(i) {
      v <- r[i]
      # detect numeric based on inferred type
      if (types[i] == "REAL") v else paste0("'", escape(v), "'")
    })
    paste0("INSERT INTO `", table_name, "` VALUES (", paste(vals, collapse=", "), ");")
  })
  
  # --- write to output file ---
  if (create_table) {
    writeLines(c(create_stmt, rows), output)
  } else {
    writeLines(rows, output)
  }
}

