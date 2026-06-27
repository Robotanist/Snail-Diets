# Kahuli Feeding Trials
?pie()

View(X20251118_Amas_Utex2340_Concatenated_pos1_labels)


sum(X20251118_Amas_Utex2340_Concatenated_pos1_labels$background)
#[1] 7175
sum(X20251118_Amas_Utex2340_Concatenated_pos1_labels$eating)
#[1] 32142
sum(X20251118_Amas_Utex2340_Concatenated_pos1_labels$sleeping)
#[1] 1026329
sum(X20251118_Amas_Utex2340_Concatenated_pos1_labels$moving)
#[1] 103633


?sum()
sum(7175+32142+1026329+103633)
#[1] 1169279
# Numb obs. = 1166060
# 1169279 - 1166060 = 3219

# Checking number of dual annotations of behaviours to account for difference between numb obs. and numb. annotations
install.packages("matrixStats")
library(matrixStats)

# Checking number of dual annotations of behaviours to account for difference between numb obs. and numb. annotations
mat <- (X20251118_Amas_Utex2340_Concatenated_pos1_labels)

# Count rows where value 1 appears in both col 2,3,4,5
# rowAnys returns TRUE if the value exists in at least one cell of the row
# We need to check specific columns, so we subset first
cols_of_interest <- as.matrix(mat[, c(2,3,4,5)])

# Check if '1' is present in the selected columns for each row
has_1_in_cols <- rowAnys(cols_of_interest, value = 1)

# To count how many times '1' appears in *both* columns specifically:
# Create a boolean matrix for cols 1 and 2
check_mat <- cols_of_interest == 1

# Sum across the selected columns for each row
# If sum == 2, the value 5 is in both columns
instances_in_both <- rowSums(check_mat) == 2
sum(instances_in_both)   
#[1] 3219


#############################################################################

# Defaulting Dual Annotation to primary behaviour for single behaviour annotation

# Eating + X Behaviour --> Eating
# Moving + Sleeping --> Moving
# Sleeping + X Behaviour --> X Behaviour
# Background should have no dual instances by definition

# This means replacing the 1 in the secondary behaviour with a 0 in ever instance the obs. contains a dual instances

##############################################################################################################
library(tidyverse)

# 1. Read the data directly as a data frame (do not drop columns yet)
raw_data <- X20251118_Amas_Utex2340_Concatenated_pos1_labels

# 2. Extract just the behavior columns into a matrix (drops the first column automatically)
# This order matches your priority: eating > moving > sleeping > background
priority_order <- c("eating", "moving", "sleeping", "background")
pos1_matrix <- as.matrix(raw_data[, priority_order])

# 3. HIGH-SPEED VECTORIZED CLEANING
# Find rows that have more than one '1'
row_sums <- rowSums(pos1_matrix == 1)
multi_one_rows <- which(row_sums > 1)

# For problem rows, find the index of the FIRST '1' based on your priority order
# because matrix indexing matches priority_order, max.col finds the first highest value
first_one_index <- max.col(pos1_matrix[multi_one_rows, ] == 1, ties.method = "first")

# Wipe out all values in problem rows, then selectively restore only the priority '1'
pos1_matrix[multi_one_rows, ] <- 0
pos1_matrix[matrix(c(multi_one_rows, first_one_index), ncol = 2)] <- 1

# 4. Verification Check
# This should print 0 if the code worked perfectly
cat("Rows remaining with multiple 1s:", sum(rowSums(pos1_matrix == 1) > 1), "\n")

# View your final, clean matrix
View(pos1_matrix)

#  cat("Rows remaining with multiple 1s:", sum(rowSums(X20251118_Amas_Utex2340_Concatenated_pos1_labels == 1) > 1), "\n")

###################################################################################################

# Sum columns and combine into new dat frame with headers

# 1. Sum each column from your clean matrix
column_totals <- colSums(pos1_matrix)

# 2. Combine the totals and headers into a new data frame
summary_df <- tibble(
  behavior = names(column_totals),
  total_count = column_totals
)

# View the final summary
print(summary_df)

pie(summary_df$total_count)


############################################################################################################

# 1. Get the binary vector for eating (1 = eating, 0 = not eating)
eating_vector <- pos1_matrix[, "eating"]

# 2. Use Run-Length Encoding to find lengths of consecutive 1s and 0s
eating_rle <- rle(eating_vector)

# 3. Calculate the ending indices of each run
end_indices <- cumsum(eating_rle$lengths)

# 4. Calculate the starting indices of each run
start_indices <- end_indices - eating_rle$lengths + 1

# 5. Combine into a data frame and filter for ONLY the "eating" periods (where value == 1)
eating_ranges <- tibble(
  start_index = start_indices,
  end_index = end_indices,
  duration_frames = eating_rle$lengths,
  status = eating_rle$values
) %>%
  filter(status == 1) %>%
  select(-status) # Remove the status column since they are all 1s

# View your new range data frame
print(eating_ranges)

 