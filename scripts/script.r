library(rcarbon)

# R code itself is really slow
# Like python, the only things that are fast are functions that call compiled C code.
# We should try to use the stdlib as much as possible

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).csv", call. = FALSE)
} else if (length(args) == 1) {
  # Default output file
  args[2] <- "out.csv"
  print("No output file specified, 'out.csv' will be used.")
} else if (length(args) == 2) {
  print("No subset specified, using the full dataset.")
} else if (length(args) == 3) {
  stop("No column value for subset specified, quitting.", call. = FALSE)
} else if (length(args) == 4) {
  # All required arguments are present
  print("Subsetting data...")
}

c <- read.csv(args[[1]])

if (length(args) == 4) {
  column <- args[[3]]
  value <- args[[4]]
  print(column)
  print(paste("Filtering on column:", column, "with value:", value))
  c <- subset(c, c[[column]] == value)
}

if (nrow(c) == 0) {
  stop("CAREFUL: No values match the subsetting provided.")
}

original_col_len <- ncol(c)

confidence_interval <- 0.95
step <- 5

c.caldates <- calibrate(x = c$C14Age, errors = c$C14SD, calCurves = "intcal20", eps=1e-100, ncores=4, type="full")

# only consider values that are inside the confidence interval
considered <- function(lst) {
  len <- length(lst[[1]])
  low <- floor(len * (1 - confidence_interval))
  high <- ceiling(len * confidence_interval)

  return(lst$calBP[low:high])
}

reduce_lists <- function(lst) {
  mins <- sapply(lst, function(x) min(considered(x)))
  maxs <- sapply(lst, function(x) max(considered(x)))

  min <- min(mins)
  max <- max(maxs)

  return(list(min, max))
}

mm <- reduce_lists(c.caldates$grids)

len <- length(c.caldates$grids) # number of rows
num_steps <- ((mm[[2]] - mm[[1]]) / step) + 1 # number of new columns

step_values <- seq(from = mm[[1]], to = mm[[2]], by = step)

new_cols <- matrix(0, nrow = len, ncol = num_steps)
colnames(new_cols) <- paste0(step_values)

col <- 1
for (i in seq(from = mm[[1]], to = mm[[2]], by = step)) {
  for (j in 1:len) {
    elems <- c.caldates$grids[[j]]
    # unfortunately there isnt a List.find equivalent function in R, but the `which` function
    # is an equivalent to List.filter implemented in C or Fortran, so its pretty quick
    # we are interested in the first element of this list
    indices <- which(elems$calBP >= i - step / 2 & elems$calBP <= i + step / 2)
    if (length(indices) > 0) {
      new_cols[j, col] <- elems$PrDens[[indices[[1]]]]
    }
  }
  col <- col + 1
}

CalibratedDates <- c(paste("Probabilities of", c$C14ID))

c <- cbind(c, CalibratedDates)
c <- cbind(c, new_cols)

cumulative_values <- apply(new_cols, 2, sum)

new_row <- c(rep("", original_col_len + 1), cumulative_values)

new_row[original_col_len + 1] <- "Cumulative Probabilities"

new_row_df <- as.data.frame(t(new_row))
colnames(new_row_df) <- colnames(c)

# Bind the new row to the original data
c <- rbind(c, new_row_df)

write.csv(c, args[[2]], row.names = FALSE)
print(paste(args[[2]], "created."))
