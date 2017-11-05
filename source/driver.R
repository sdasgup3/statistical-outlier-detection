library(ggplot2)
library(grid)
library(gridExtra)
library(quantreg)
options(width=220)
options(scipen=50)

cls <- function() cat(rep("\n",100))
cls()

# Remove all the variables
rm(list=setdiff(ls(), lsf.str()))
source("utils.R")

# This flag determines whether images are saved as png files
saveAsPng <- TRUE

# Data Read
filepath <- "data/data.csv";

runtime_df<- read.csv(file=filepath, header=FALSE, sep="$")

cat("Reading done ...\n")

# Remove the column which are all NAs
na_matrix <-  is.na(runtime_df)
cs <- colSums(na_matrix)
col_all_na <- (cs == nrow(runtime_df))
df <- runtime_df[, !col_all_na]
cat("All NaN Columns: ", sum(col_all_na), " cols: ", length(df),  "\n")
ncols <- ncol(df)-1
nrows <- nrow(df)

# Check if any data is NA
na_matrix <-  is.na(df)
cs <- colSums(na_matrix)
col_all_na <- (cs > 0)
if(sum(col_all_na) != 0) {
	stop("Check sanity of df failed (NA data point even after all NaN colm removal )\n")
}

# If a row has 20% of its data as zero, we will ignore that row.
# This will cater 2 issues:
# 1. Those which are sampled very less number of times because of their short runtimes
# 2. With 80/20 rule for train and test, we need atleast 80% valid data for training any model
numeric_df <- df[,-1]
bool_matrix <- numeric_df == 0
bool_matrix <- (rowSums(bool_matrix) / ncols ) > .20
stat_df <- df[!bool_matrix,]
stat_ncols <- ncol(stat_df)-1
stat_nrows <- nrow(stat_df)
cat("Processing ", stat_nrows, " functions each for ", stat_ncols, " days\n")
ncols <- stat_ncols
nrows <- stat_nrows

run_all <- function() {
	sink("output.txt", append=FALSE, split=TRUE)
	havoc.report.1 <- havoc.report.2 <-  havoc.report.3 <-  havoc.report.4 <-  havoc.report.5 <-  havoc.report.6 <- havoc.report.7 <- havoc.report.8 <-  havoc.report.9 <- 0

	num_reported.1 <- num_reported.2 <- num_reported.3 <- num_reported.4 <- num_reported.5 <- num_reported.6 <-num_reported.7 <- num_reported.8 <- 0
	last_havoc.1 <- last_havoc.2 <- last_havoc.4 <- last_havoc.6 <- last_havoc.8 <- 0

	for (i in 1:nrows) {
		data <- stat_df[i,]
    cat(i, ":T2: ")
    h <- train_n_test_type_2(i, data, FALSE)
		len <- length(h$havoc)
    print(h$havoc)
		if(len != 0) {
		  num_reported.2 <- num_reported.2 + 1
		}
		havoc.report.2 <- havoc.report.2 + len
		if(31 %in% h$havoc) {
			last_havoc.2 = last_havoc.2 +1
		}

    cat(i, ":T4: ")
    h <- train_n_test_type_4(i, data, FALSE)
		len <- length(h$havoc)
    print(h$havoc)
		if(len != 0) {
		  num_reported.4 <- num_reported.4 + 1
		}
		havoc.report.4 <- havoc.report.4 + len
		if(31 %in% h$havoc) {
			last_havoc.4 = last_havoc.4 +1
		}

    cat(i, ":T6: ")
    h <- train_n_test_type_6(i, data, FALSE)
		len <- length(h$havoc)
    print(h$havoc)
		if(len != 0) {
		  num_reported.6 <- num_reported.6 + 1
		}
		havoc.report.6 <- havoc.report.6 + len
		if(31 %in% h$havoc) {
			last_havoc.6 = last_havoc.6 +1
		}

    cat(i, ":T8: ")
    h <- train_n_test_type_8(i, data, FALSE)
		len <- length(h$havoc)
    print(h$havoc)
		if(len != 0) {
		  num_reported.8 <- num_reported.8 + 1
		}
		havoc.report.8 <- havoc.report.8 + len
		if(31 %in% h$havoc) {
			last_havoc.8 = last_havoc.8 +1
		}
	}

	cat ("\n\nTest havoc reported per function\n")
	cat ("Type 2: ", havoc.report.2/nrows, "\n")
	cat ("Type 4: ", havoc.report.4/nrows, "\n")
	cat ("Type 6: ", havoc.report.6/nrows, "\n")
	cat ("Type 8: ", havoc.report.8/nrows, "\n")
	cat ("\n\nRate of functions reported (based on test)\n")
	cat ("Type 2: ", (num_reported.2/nrows)*100, " ", num_reported.2, "\n")
	cat ("Type 4: ", (num_reported.4/nrows)*100, " ", num_reported.4, "\n")
	cat ("Type 6: ", (num_reported.6/nrows)*100, " ", num_reported.6, "\n")
	cat ("Type 8: ", (num_reported.8/nrows)*100, " ", num_reported.8, "\n")
	cat ("\n\nLast Havoc\n")
	cat ("Type 2: ", (last_havoc.2/nrows)*100, " ", last_havoc.2, "\n")
	cat ("Type 4: ", (last_havoc.4/nrows)*100, " ", last_havoc.4, "\n")
	cat ("Type 6: ", (last_havoc.6/nrows)*100, " ", last_havoc.6, "\n")
	cat ("Type 8: ", (last_havoc.8/nrows)*100, " ", last_havoc.8, "\n")
	sink()
}

multiplot <- function(index, data) {
	func_name <- as.character(data[[1]])
	savefile <- paste(index, ".png", sep="")
	
	p1 <- train_n_test_type_1(index, data, TRUE)
  p2 <- train_n_test_type_2(index, data, TRUE)
	#p3 <- train_n_test_type_3(index, data, TRUE)
  p4 <- train_n_test_type_4(index, data, TRUE)
	#p5 <- train_n_test_type_5(index, data, TRUE)
  #p6 <- train_n_test_type_6(index, data, TRUE)
	#p7 <- train_n_test_type_7(index, data, TRUE)
  p8 <- train_n_test_type_8(index, data,  TRUE)
  #p9 <- train_n_test_type_9(index, data, TRUE)
  #grid.arrange(p1, p2,  p4, p8, nrow=4)
	g <- arrangeGrob(p1, p2,p4,  p8, nrow=4) #generates g
	#ggsave(file=savefile, g)
}
