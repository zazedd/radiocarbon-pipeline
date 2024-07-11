library(rcarbon)

# R code itself is really slow
# Like python, the only things that are fast are functions that call compiled C code.
# We should try to use the stdlib as much as possible

args <- commandArgs(trailingOnly = TRUE)

confidence_interval <- 0.95
step <- 5

get_value <- function(i) {
  v <- strsplit(config[[i, 1]], "=")[[1]][2]
  return(v)
}

if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).csv", call. = FALSE)
} else if (length(args) == 1) {
  # Default output file
  args[2] <- "out.csv"
  print("No output file specified, 'out.csv' will be used.")
} else if (length(args) == 2) {
  print("No config file specified. Using default values. (Confidence Interval -> 0.95, Step -> 5 years, No Filtering)")
} else if (length(args) == 3) {
  print("All arguments specified.")
  config <- read.delim2(args[[3]], header = FALSE, sep = "\n")

  step <- as.numeric(get_value(1))
  confidence_interval <- as.numeric(get_value(2))
  print(paste("Confidence Interval ->", confidence_interval, ", Step ->", step, "years"))

  print("Starting...")
}

c <- read.csv(args[[1]])

if (length(args) == 3) {
  column <- get_value(3)
  value <- get_value(4)

  if (is.na(column) || is.na(value)) {
    print("No subsetting applied")
  } else {
    print(column)
    print(value)
    print(paste("Filtering on column:", column, "with value:", value))

    c <- subset(c, c[[column]] == value)
  }
}

if (nrow(c) == 0) {
  stop("CAREFUL: No values match the subsetting provided.")
}

original_col_len <- ncol(c)

c.caldates <- calibrate(x = c$C14Age, errors = c$C14SD, calCurves = "intcal20", eps = 1e-5, ncores = 4, type = "full")

# DK.spd <- spd(c.caldates, timeRange = c(8000, 0))
# plot(DK.spd)
# plot(DK.spd, runm = 200, add = TRUE, type = "simple", col = "darkorange", lwd = 1.5, lty = 2) # using a rolling average of 200 years for smoothing

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

    indices <- (which(elems$calBP >= i - step / 2 & elems$calBP <= i + step / 2))
    if (length(indices) > 0) {
      tmp <- list()
      for (k in 1:length(indices)) {
        tmp <- c(elems$PrDens[[indices[[k]]]], tmp)
      }
      new_cols[j, col] <- sum(unlist(tmp))
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
