#' Convert CSV file to SQL INSERT statements
#' 
#' Reads a CSV file and generates SQL statements to create a table and insert
#' all rows. The function automatically infers column types (REAL for numeric,
#' DATE for date strings matching YYYY-MM-DD format, TEXT otherwise) and
#' generates appropriate CREATE TABLE and INSERT statements.
#' 
#' @param input Character string. Path to the input CSV file.
#' @param output Character string. Path to the output SQL file where the
#'   statements will be written.
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
#'   \item Generates a \code{CREATE TABLE} statement using the base filename
#'     (without extension) as the table name
#'   \item Generates \code{INSERT INTO} statements for each row
#'   \item Writes all SQL statements to the output file
#' }
#' 
#' Single quotes in text values are escaped by doubling them (SQL standard).
#' Numeric values are inserted without quotes, while text and date values are
#' wrapped in single quotes.
#' 
#' @examples
#' \dontrun{
#' # Convert a CSV file to SQL
#' convert_to_sql("data.csv", "data.sql")
#' 
#' # The output file will contain:
#' # CREATE TABLE `data` (
#' #   `column1` TEXT,
#' #   `column2` REAL,
#' #   `column3` DATE
#' # );
#' # 
#' # INSERT INTO `data` VALUES ('value1', 123.45, '2024-01-01');
#' # INSERT INTO `data` VALUES ('value2', 67.89, '2024-02-02');
#' # ...
#' }
#' 
#' @export
convert_to_sql <- function(input, output) {
  
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
  
  # --- CREATE TABLE ---
  fields <- paste0("  `", names(types), "` ", types, collapse = ",\n")
  create_stmt <- paste0("CREATE TABLE `", table_name, "` (\n", fields, "\n);\n\n")
  
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
  writeLines(c(create_stmt, rows), output)
}

