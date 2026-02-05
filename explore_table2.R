# ============================================================================
# explore_table2.R - Interactive exploration script for table2()
# 
# This script generates various data structures to test table2() with:
# 1. Simple 2-way contingency tables
# 2. Multi-way tables (3+ variables)
# 3. Proportions (all, row, column)
# 4. Chi-square tests
# 5. Missing data patterns
# 6. Unbalanced tables
# 7. Different variable types
# ============================================================================
path = "c:/git/statuser"
devtools::load_all(path)
set.seed(2024)

# ============================================================================
# SECTION 1: BASIC 2-WAY TABLES
# ============================================================================

# Generate data
  n <- 200
  df2 <- data.frame(
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    response = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4)),
    education = sample(c("High School", "Bachelor", "Graduate"), n, replace = TRUE),
    satisfied = sample(c("Satisfied", "Neutral", "Unsatisfied"), n, replace = TRUE)
  )

# 1.1 - Basic frequency table
  table2(df2$gender, df2$response)

# 1.2 - Table with chi-square test
  table2(df2$gender, df2$response, chi = TRUE)

# 1.3 - Table with proportions (all cells)
  table2(df2$gender, df2$response, prop = "all")

# 1.4 - Row proportions
  table2(df2$gender, df2$response, prop = "row")

# 1.5 - Column proportions
  table2(df2$gender, df2$response, prop = "col")

# 1.6 - Proportions with chi-square
  table2(df2$gender, df2$response, prop = "all", chi = TRUE)


# ============================================================================
# SECTION 2: MULTI-WAY TABLES (3+ VARIABLES)
# ============================================================================

# 2.1 - Three-way table
  table2(df2$gender, df2$response, df2$education)

# 2.2 - Three-way with proportions
  table2(df2$gender, df2$response, df2$education, prop = "all")

# 2.3 - Four-way table
  table2(df2$gender, df2$response, df2$education, df2$satisfied)


# ============================================================================
# SECTION 3: UNBALANCED AND EDGE CASES
# ============================================================================

# Generate unbalanced data
  df_unbal <- data.frame(
    group = c(rep("A", 100), rep("B", 50), rep("C", 20)),
    outcome = sample(c("Success", "Failure"), 170, replace = TRUE)
  )

# 3.1 - Unbalanced groups
  table2(df_unbal$group, df_unbal$outcome)

# 3.2 - Unbalanced with row proportions (shows different group sizes)
  table2(df_unbal$group, df_unbal$outcome, prop = "row")

# 3.3 - Unbalanced with chi-square
  table2(df_unbal$group, df_unbal$outcome, chi = TRUE)


# ============================================================================
# SECTION 4: MISSING DATA
# ============================================================================

# Generate data with missing values
  df_miss <- data.frame(
    var1 = sample(c("A", "B", NA), 100, replace = TRUE),
    var2 = sample(c("X", "Y", NA), 100, replace = TRUE)
  )

# 4.1 - Default (excludes NA)
  table2(df_miss$var1, df_miss$var2)

# 4.2 - Include NA if any
  table2(df_miss$var1, df_miss$var2, useNA = "ifany")

# 4.3 - Always show NA category
  table2(df_miss$var1, df_miss$var2, useNA = "always")

# 4.4 - Missing data with proportions
  table2(df_miss$var1, df_miss$var2, useNA = "ifany", prop = "all")


# ============================================================================
# SECTION 5: ORDERED AND FACTOR VARIABLES
# ============================================================================

# Generate ordered factors
  df_ord <- data.frame(
    satisfaction = factor(sample(c("Low", "Medium", "High"), 150, replace = TRUE),
                         levels = c("Low", "Medium", "High"), ordered = TRUE),
    age_group = factor(sample(c("18-30", "31-50", "51+"), 150, replace = TRUE),
                      levels = c("18-30", "31-50", "51+"))
  )

# 5.1 - Ordered factors
  table2(df_ord$satisfaction, df_ord$age_group)

# 5.2 - Ordered with row proportions
  table2(df_ord$satisfaction, df_ord$age_group, prop = "row")

# 5.3 - Ordered with chi-square
  table2(df_ord$satisfaction, df_ord$age_group, chi = TRUE)


# ============================================================================
# SECTION 6: DIFFERENT CELL COUNTS (RARE EVENTS)
# ============================================================================

# Generate data with rare events
  df_rare <- data.frame(
    treatment = sample(c("Control", "Treatment"), 200, replace = TRUE),
    adverse = sample(c("None", "Mild", "Severe"), 200, replace = TRUE,
                    prob = c(0.85, 0.12, 0.03))  # Severe is rare
  )

# 6.1 - Table with rare events
  table2(df_rare$treatment, df_rare$adverse)

# 6.2 - Rare events with column proportions
  table2(df_rare$treatment, df_rare$adverse, prop = "col")

# 6.3 - Chi-square test with rare events
  table2(df_rare$treatment, df_rare$adverse, chi = TRUE)


# ============================================================================
# SECTION 7: TESTING YATES' CORRECTION (2x2 TABLES)
# ============================================================================

# Generate 2x2 table data
  df_2x2 <- data.frame(
    exposed = sample(c("Yes", "No"), 100, replace = TRUE),
    disease = sample(c("Cases", "Controls"), 100, replace = TRUE)
  )

# 7.1 - 2x2 without correction
  table2(df_2x2$exposed, df_2x2$disease, chi = TRUE, correct = FALSE)

# 7.2 - 2x2 with Yates' correction
  table2(df_2x2$exposed, df_2x2$disease, chi = TRUE, correct = TRUE)


# ============================================================================
# SECTION 8: PRECISION (DIGITS) TESTING
# ============================================================================

# 8.1 - Default digits (3)
  table2(df2$gender, df2$response, prop = "all")

# 8.2 - Two decimal places
  table2(df2$gender, df2$response, prop = "all", digits = 2)

# 8.3 - Four decimal places
  table2(df2$gender, df2$response, prop = "all", digits = 4)


# ============================================================================
# SECTION 9: DIFFERENT DATA SOURCES
# ============================================================================

# 9.1 - From vectors (not dataframe columns)
  vec1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
  vec2 <- sample(c("X", "Y"), 100, replace = TRUE)
  table2(vec1, vec2)

# 9.2 - From list elements
  data_list <- list(
    group = sample(c("Control", "Treatment"), 100, replace = TRUE),
    outcome = sample(c("Success", "Failure"), 100, replace = TRUE)
  )
  table2(data_list$group, data_list$outcome)

# 9.3 - Inline data
  table2(sample(c("A", "B"), 50, replace = TRUE),
         sample(c("X", "Y"), 50, replace = TRUE))


# ============================================================================
# SECTION 10: UNEXPECTED DATA TYPES
# ============================================================================

# 10.1 - Numeric variables (should work - converts to categorical)
  numeric1 <- sample(1:5, 100, replace = TRUE)
  numeric2 <- sample(1:3, 100, replace = TRUE)
  table2(numeric1, numeric2)

# 10.2 - Continuous numeric (gets binned/categorized automatically?)
  continuous <- rnorm(100, mean = 50, sd = 10)
  groups <- sample(c("A", "B"), 100, replace = TRUE)
  table2(continuous, groups)  # Will create many rows for each unique value

# 10.3 - Mixed character and factor
  char_var <- sample(c("Red", "Blue", "Green"), 100, replace = TRUE)
  factor_var <- factor(sample(c("Small", "Large"), 100, replace = TRUE))
  table2(char_var, factor_var)

# 10.4 - Logical variables (TRUE/FALSE)
  logical1 <- sample(c(TRUE, FALSE), 100, replace = TRUE)
  logical2 <- sample(c(TRUE, FALSE), 100, replace = TRUE)
  table2(logical1, logical2)

# 10.5 - Binary numeric (0/1) vs factor
  binary <- sample(0:1, 100, replace = TRUE)
  factor_bin <- factor(sample(c("Yes", "No"), 100, replace = TRUE))
  table2(binary, factor_bin)

# 10.6 - Ordered factor vs unordered factor
  ordered_var <- ordered(sample(c("Low", "Med", "High"), 100, replace = TRUE),
                        levels = c("Low", "Med", "High"))
  unordered_var <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))
  table2(ordered_var, unordered_var)

# 10.7 - Single variable (1-way table)
  single <- sample(c("Cat", "Dog", "Bird"), 100, replace = TRUE, 
                  prob = c(0.5, 0.3, 0.2))
  table2(single)

# 10.8 - Many categories (wide table)
  many_cats1 <- sample(LETTERS[1:10], 100, replace = TRUE)
  many_cats2 <- sample(letters[1:8], 100, replace = TRUE)
  table2(many_cats1, many_cats2)


# ============================================================================
# SECTION 11: COMPLEX REAL-WORLD SCENARIOS
# ============================================================================

# Simulate survey data
  survey <- data.frame(
    age_group = sample(c("<25", "25-40", "40-60", ">60"), 300, replace = TRUE),
    voted = sample(c("Yes", "No", "Prefer not to say"), 300, replace = TRUE,
                  prob = c(0.55, 0.35, 0.10)),
    party = sample(c("Democrat", "Republican", "Independent", "Other"), 300, 
                  replace = TRUE, prob = c(0.35, 0.35, 0.25, 0.05))
  )

# 11.1 - Survey response by age
  table2(survey$age_group, survey$voted, prop = "row", chi = TRUE)

# 11.2 - Three-way: age, voted, party
  table2(survey$age_group, survey$voted, survey$party)

# 11.3 - Party affiliation by voting behavior
  table2(survey$party, survey$voted, prop = "col")
