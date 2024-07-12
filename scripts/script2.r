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
  stop("CAREFUL: No output file specified.", call = FALSE)
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

c <- sapply(c, function(x) toupper(x))

write.csv(c, args[[2]], row.names = FALSE)
print(paste(args[[2]], "created."))
