# Debug test - run this after sourcing your files

# Create test data
set.seed(12)
n <- 100
x2 <- rep(c("men","women"), n)
x3 <- sample(1:3, replace=TRUE, size=2*n)

# Create the table
result <- table2(x2, x3, prop='all')

# Check the attributes
cat("\n=== DEBUGGING ===\n")
cat("Class of result:", class(result), "\n")
cat("is_proportion attribute:", attr(result, "is_proportion"), "\n")
cat("freq_table attribute is null?:", is.null(attr(result, "freq_table")), "\n")

# If freq_table exists, show its structure
freq_tab <- attr(result, "freq_table")
if (!is.null(freq_tab)) {
  cat("\nFrequency table found!\n")
  cat("Dimensions:", dim(freq_tab), "\n")
  cat("Class:", class(freq_tab), "\n")
  print(freq_tab)
} else {
  cat("\nNO FREQUENCY TABLE STORED!\n")
}

cat("\n=== Now printing result ===\n")
print(result)
