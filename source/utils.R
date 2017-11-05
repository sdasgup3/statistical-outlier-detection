options(scipen=10)
options(width=222)

###########################################################################
#######################	BASIC FUNCTION ###################################
###########################################################################

# This is equiv to R's quantine(data, type=6) accurate upto 2 places of decimal
# works for data size > 2
quantile_ <- function(data, probs = c(0,0.25,0.50, 0.75,1)) {
  data <- sort(data)
  n <- length(data)

  result <- vector(mode="numeric", length=length(probs))
 # cat(probs, "\n", length(probs), "\n")

  for (i in 1:length(probs)) {

    if(0 == probs[i]) {
      result[i] <- data[1]
    } else if(1 == probs[i]) {
      result[i] <- data[n]
    } else {
      index_p <- probs[i] * (n+1)

      if(floor(index_p) == index_p) {
        result[i] <- data[index_p]
      } else {
        index <- as.integer(index_p)
      	p <- (index_p - index)
      	if(index == n) {
		index <- n-1
	}
	if(0 == index) {
		index <- 1
	}
       	result[i] <- data[index] + p * (data[index+1] - data[index])   
      }
    }
  }

  return (result)
}

rank_ <- function(input) {
	num_points <- length(input)
	indices	<- c(1:length(input))
  	
	sort_order <- order(input)
	sorted_input <- input[sort_order]
	indices <- indices[sort_order]
	ranks <- numeric(length=num_points)

	i <- 1
	while(i <= num_points) {
    		j <- i + 1;
    		while ((j <= num_points) & (sorted_input[j] == sorted_input[i])) {
			#cat(" Dup: ", j, " ", sorted_input[i] == sorted_input[j]," " , sorted_input[i] , " ",  sorted_input[j] , " \n")

      			j <- j  + 1
    		}
    		num_dups = j - i
    		sum = num_dups * (i + 1) + (num_dups - 1) * (num_dups) / 2
    		rank = sum / num_dups
	    	k <- i
	    	while(k < j) {
      			ranks[indices[k]] = rank;
			k <- k + 1
    		}
    		i <- j
  	}
	ranks <- ranks - 1
  	return (ranks)
}

median_ <- function(data) {
  data <- sort(data)
  n <- length(data)
  m <- (n+1)/2

  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
    return ( ( data[l] + data[u] )/2 )  
  } else {
    return (data[m])
  }
}

quantile_ver2 <- function(data) {
  data <- sort(data)
  n <- length(data)
  result <- vector(mode="numeric", length=3)

	if ( (n %% 2) == 0) {
		c1 <- n / 2
		c2 <- n / 2
	} else {
		c1 <- (n - 1) / 2
		c2 <- c1 + 1
	}

	# Find the Medians with the cutoff points
	result[1] <-  median(data[1:c1])
	result[2] <- median(data)
	result[3] <- median(data[c2:n])

	return (result)
}


# Median Absolute Deviation 
mad_ <- function(data) {
	m <- median_(data)
	mad <- median_(abs(data - m))
	return (mad)
}

# Two tail Median Absolute Deviation 
# If the underlying distribution is unsymmetric, standard-deviations-from-mean and MADs-from-median strategies both fall flat. 
# The problem is that these strategies apply the same cutoff—e.g. 2 sample standard deviations or 2 MADs—to both tails of a sample, 
# even if one tail is far longer than the other. 
double_mad_ <- function(x){
   x         <- x[!is.na(x)]
   m         <- median(x)
   abs.dev   <- abs(x - m)
   left.mad  <- median(abs.dev[x<=m])
   right.mad <- median(abs.dev[x>=m])
   # If more than 50% of  data have identical values, MAD will equal zero. All points in dataset except those that equal the median 
   # will then be flagged as outliers, regardless of the level at which we set your outlier cutoff. 
   # So at the very least check that we don't have too many identical data points before using the MAD to flag outliers.
   if (left.mad == 0 || right.mad == 0){
      stop("MAD is 0")
   }
   return(c(left.mad, right.mad))
}


# The function finds the outliers based on double MAD
find_outliers_double_mad_based_ <- function(data_y, data_x) {
   	two.sided.mad <- double_mad_(data_y)
   	m <- median(data_y, na.rm=TRUE)
   	mad <- rep(two.sided.mad[1], length(data_y))
   	mad[data_y > m] <- two.sided.mad[2]
	cutoff <- 3

   	metric <- abs(data_y - m) / mad
   	metric[data_y==m] <- 0
  	cat (metric, "\n")

#df <- data.frame(X = data_x[metric > cutoff ], Y = data_y[metric > cutoff ])
        outliers <- which(metric > cutoff)


  	return (outliers)
}

###########################################################################
#######################	FIND HAVOC ###################################
###########################################################################

#####################################################
###
### Find outlier based on mad
###
#####################################################

# The function finds mad statistics
mad_statistics_ <- function (data_y, data_x) {
	mad <- mad_(data_y)
	m <- median_(data_y)
	consistency_constant <- 1.4826
	# This modification will ensure that for large samples the MAD provides a good estimate of the standard deviation
	# (more formally, the MAD becomes a consistent estimator of the population standard deviation).
	mad <- consistency_constant * mad
	cutoff <- 3
  	return (list(median=m, mad=mad, cutoff=cutoff))
}

train_n_test_type_1 <- function(index, data, plot=TRUE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("MAD", func_name, sep=" ("), ")", sep="")
		g <- g + ggplot2::geom_point() + ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1)) +
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train
	analysis <- mad_statistics_(train_data_y_no_zero, train_data_x_no_zero)
	m <- analysis$median
	mad <- analysis$mad
	cutoff <- analysis$cutoff

	metric <- abs(train_data_y_no_zero - m) / mad
	outlier_indices <- which(metric > cutoff)
	severity <- (train_data_y_no_zero[outlier_indices] > m)

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(m - cutoff*mad , m, m + cutoff*mad))
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_line(aes(y=m + cutoff*mad),colour="red", size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(y=m),colour="black", size=1, linetype="dotted") +
			ggplot2::geom_line(aes(y=m - cutoff*mad),colour="black", size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted") 

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
	metric <- abs(test_data_y - m) / mad
	outlier_indices <- which(metric > cutoff)
	severity <- (test_data_y[outlier_indices] > m)
	if(!plot) {
		if(debug) {
			cat("Test\n")
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	} else {
	        return (list(havoc=test_data_x[outlier_indices]))
        }

}

#####################################################
###
### Find havoc based on inter quantile range (iqr)
###
#####################################################

# The function finds iqr statistics
quantile_iqr_statistics_ <- function(data_y, data_x) {
	data <- data_y
  n <- length(data)
  qnt <- quantile_(data)
  q3 <- qnt[4]
  q2 <- qnt[3]
  q1 <- qnt[2]
  iqr <- (q3-q1)
  lower_bound <- q1 -  1.5*iqr 
  upper_bound <- q3 + 1.5*iqr 

 	return (list(median=q2, lower_bound=lower_bound, upper_bound=upper_bound))
}

find_havoc_using_iqr <- function(train_data_y, train_data_x, test_data_y, test_data_x, g, plot=TRUE, debug=FALSE) {
	# Train
	num_train <- length(train_data_x)
	num_test <- length(test_data_x)

	analysis <- quantile_iqr_statistics_(train_data_y, train_data_x)
	m <- analysis$median
	lower_bound <- analysis$lower_bound
	upper_bound <- analysis$upper_bound

	metric <- (train_data_y > upper_bound) | (train_data_y < lower_bound)
	outlier_indices <- which(metric)
	severity <- (train_data_y[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("Bounds\n")
			print(c(lower_bound , m, upper_bound))
		}

		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x[outlier_indices], 
						 TS=train_data_y[outlier_indices], S=severity )
			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_segment(aes_string(x=train_data_x[1], y=upper_bound, xend=test_data_x[num_test], yend=upper_bound ),colour="red", size=0.8, linetype="dotted") +
			ggplot2::geom_segment(aes_string(x=train_data_x[1], y=m, xend=test_data_x[num_test], yend=m),colour="black", size=1, linetype="dotted") +
		ggplot2::geom_segment(aes_string(x=train_data_x[1], y=lower_bound, xend=test_data_x[num_test], yend=lower_bound),colour="black", size=0.8, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x[high_severity_indices], 
					      TS=train_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x[low_severity_indices], 
					     TS=train_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
	metric <- (test_data_y > upper_bound) | (test_data_y < lower_bound)
	outlier_indices <- which(metric)
	severity <- (test_data_y[outlier_indices] > upper_bound)
		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

	if(!plot) {
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	#return (list(havoc=test_data_x[outlier_indices], severity=severity, g=g))
	return (list(havoc=test_data_x[high_severity_indices], severity=severity, g=g, median=m))
}


#####################################################
###
### Find outlier based on linear reggression without removing influentials
###
#####################################################

lm_ <- function(data_y, data_x) {
  Y <- data_y
  X <- data_x
  n <- length(Y)

  slope <- sum( (X - mean(X)) * (Y - mean(Y)) ) / sum( (X- mean(X))^2   )
  intercept <- mean(Y) - slope*mean(X)
  estimated <- slope*X + intercept
  # COefficient of determination or Multiple R sqaured in R terminology
  # "r2 ×100 percent of the variation in y is 'explained by' the variation in predictor x."
  # 0 <= r2 <= 1, More is better
  r2 <- sum ( (estimated - mean(Y))^2 ) / sum ( (Y - mean(Y))^2 )  
  # (Pearson) Correlation Coefficient r
  #If r = -1, then there is a perfect negative linear relationship between x and y.
  #If r = 1, then there is a perfect positive linear relationship between x and y.
  #If r = 0, then there is no linear relationship between x and y.
  r <- sqrt(r2)
  if(slope < 0) {
    r <- -1*r
  }
  # Mean squared error
  mse <- (sum( (estimated - Y )^2  ))  / (n-2)
  # Residual standard error in R terminology
  standard_error_estimate <- sqrt( mse )
  #residuals <- (Y - estimated)

  return (list(slope=slope, intercept=intercept, r2=r2, std_err=standard_error_estimate))
}

train_n_test_type_3 <- function(index, data, plot=TRUE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("SLR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train
	#fit <- lm_(train_data_y_no_zero, c(1:length(train_data_x_no_zero)))
	fit <- lm_(train_data_y_no_zero, train_data_x_no_zero)
	slope <- fit$slope
	intercept <- fit$intercept

    	residuals <- train_data_y_no_zero - (train_data_x_no_zero*slope + intercept)
	analysis <- quantile_iqr_statistics_(residuals, c(1:length(residuals)))
	m <- analysis$median
	lower_bound <- analysis$lower_bound
	upper_bound <- analysis$upper_bound

	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(lower_bound , m, upper_bound))
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_abline(slope=slope, intercept=intercept, size=1, linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + upper_bound, size=0.8, color="red", linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + lower_bound, size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
    	residuals <- test_data_y - (test_data_x*slope + intercept)
	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("Test\n")
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}

		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	}
}

#####################################################
###
### Find outlier based on linear reggression removing influentials
###
#####################################################


# The leverage, hii, quantifies the influence that the observed response yi has on its predicted value y^i. That is, 
# if hii is small, then the observed response yi plays only a small role in the value of the predicted response y^i. 
# On the other hand, if hii is large, then the observed response yi plays a large role in the value of the predicted 
# response y^i. It's for this reason that the hii are called the "leverages
# 1. The leverage hii is a measure of the distance between the x value for the ith data point and the mean of the x 
#    values for all n data points.
# 2. The leverage hii is a number between 0 and 1, inclusive.
# 3. The sum of the hii equals p, the number of parameters (regression coefficients including the intercept).
hatvalues_ <- function(data_x) {
  # Finding leverages: Extreme x values 
  X <- matrix(c(rep(1, length(data_x)),data_x), nrow=length(data_x), ncol=2, byrow=FALSE)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  HII <- diag(H)

  return (HII)
}

# An observation is said to have high leverage, if that observation has a particularly unusual combination of predictor 
# values (e.g., one predictor has a very different value for that observation compared with all the other data observations).
find_leverage_ <- function (data_y, data_x) {
  HII <- hatvalues_(data_x)
  mean_leverage_val <- mean(HII) # NOTE: sum(HII) == 2
  cat(HII, "\nmean leverage value: ", mean_leverage_val, "\n")
  df <- data.frame(X = data_x[HII > 3 * mean_leverage_val], Y = data_y[HII > 3 *mean_leverage_val])
  return (df)
}

# An observations is called an outlier, if that observation has a response value that is very different from the predicted 
# value based on a model
find_outliers_simple_linear_regression_based_ <- function(data_y,data_x) {
  
  fit <- lm_(data_y, data_x)
  mse <- fit$std_err ^ 2  
  slope <- fit$slope
  intercept <- fit$intercept
  residuals <- data_y - (slope*data_x + intercept)

  hii <- hatvalues_(data_x)
  # Studentized residuals (or internally studentized residuals)
  sres <- residuals / sqrt(mse * (1-hii) )

  cat (sres, "\n")
  df <- data.frame(X = data_x[abs(sres) > 3 ], Y = data_y[abs(sres) > 3 ])

  return (df)
}

# A data point is influential if it unduly influences any part of a regression analysis, such as the predicted responses, 
# the estimated slope coefficients, or the hypothesis test results. Outliers and high leverage data points have the potential 
# to be influential, but we generally have to investigate further to determine whether or not they are actually influential.
# The function finds the influence based on cook's distance di
# If di is greater than 0.5, then the ith data point is worthy of further investigation as it may be influential.
# If di is greater than 1, then the ith data point is quite likely to be influential.
# Or, if di sticks out like a sore thumb from the other Di values, it is almost certainly influential.
find_influence_cooks_ <- function (data_y, data_x) {

  n <- length(data_x)
  hii <- hatvalues_(data_x)
  di <- vector(mode="numeric", length=n)
  cutoff <- 2*sqrt( (2 + 1) / (n-2-1)  ) 

  # Find predicted value of y with all included
  fit <- lm_(data_y, data_x)
  mse <- fit$std_err ^ 2  
  slope <- fit$slope
  intercept <- fit$intercept
  predicted_y <- slope*data_x + intercept

  di <-  ( ((data_y - predicted_y)^2)  / (2*mse) ) * ( hii / (1-hii)^2)
#cat(di, "\n")

  return (list(IF_IDX=which(abs(di) > 0.5)))
}

# A data point is influential if it unduly influences any part of a regression analysis, such as the predicted responses, 
# the estimated slope coefficients, or the hypothesis test results. Outliers and high leverage data points have the potential 
# to be influential, but we generally have to investigate further to determine whether or not they are actually influential.
# The function finds the influence based on di, difference in fits for observation i. 
find_influence_dffits_ <- function (data_y, data_x) {

  n <- length(data_x)
  hii <- hatvalues_(data_x)
  di <- vector(mode="numeric", length=n)
  cutoff <- 2*sqrt( (2 + 1) / (n-2-1)  ) 

  # Find predicted value of y with all included
  fit <- lm_(data_y, data_x)
  slope <- fit$slope
  intercept <- fit$intercept
  predicted_y <- slope*data_x + intercept

  for (i in 1:n) {

    # Remove ith obseration
    X <- data_x[-c(i)]
    Y <- data_y[-c(i)]

    fit <- lm_(Y, X)
    slope <- fit$slope
    intercept <- fit$intercept
    mse <- fit$std_err ^ 2  

    predicted_yi <- data_x[i]*slope + intercept

    di[i] <- (predicted_y[i] - predicted_yi) / sqrt(mse*hii[i])

  }

  cat (di, "\n cutoff: ", cutoff, "\n")
  df <- data.frame(X = data_x[abs(di) > cutoff ], Y = data_y[abs(di) > cutoff ])

  return (df)

}

train_n_test_type_4 <- function(index, data, plot=TRUE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("SLR-INF+IQR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train

	## Remove influential points
	influentials_analysis <- find_influence_cooks_( train_data_y_no_zero, train_data_x_no_zero)
	IY <- train_data_y_no_zero[influentials_analysis$IF_IDX]
	IX <- train_data_x_no_zero[influentials_analysis$IF_IDX]
	if(!plot) {
		if(debug) {
			cat("Influentials: ", length(influentials_analysis$IF_IDX), "\n")
			print(IY)
			print(IX)
		}
	} else {
		influential_df <- data.frame(D=IX, TS=IY)
		g <- g +
		ggplot2::geom_point(data=influential_df,colour="pink", size=2) 
	}
        influence_removed <- FALSE
	if(length(influentials_analysis$IF_IDX) != 0 ) {
                influence_removed <- TRUE
#cat(influentials_analysis$IF_IDX, "\n")
		train_data_y_no_zero <- train_data_y_no_zero[-c(influentials_analysis$IF_IDX)]
		train_data_x_no_zero <- train_data_x_no_zero[-c(influentials_analysis$IF_IDX)]
	}


	## Train with influential points removed
	fit <- lm_(train_data_y_no_zero, train_data_x_no_zero)
	slope <- fit$slope
	intercept <- fit$intercept

    	residuals <- train_data_y_no_zero - (train_data_x_no_zero*slope + intercept)

	analysis <- quantile_iqr_statistics_(residuals, c(1:length(residuals)))
	m <- analysis$median
	lower_bound <- analysis$lower_bound
	upper_bound <- analysis$upper_bound

	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(lower_bound , m, upper_bound))
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_abline(slope=slope, intercept=intercept, size=1, linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + upper_bound, size=0.8, color="red", linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + lower_bound, size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
    	residuals <- test_data_y - (test_data_x*slope + intercept)
	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("Test\n")
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}

		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	} else {
                #return (influentials_analysis$IF_IDX)
		return (list(havoc=test_data_x[outlier_indices]))
        }
}

#####################################################
###
### Find outlier based on control charts
###
#####################################################

# The violation ratio represents the degree to which the current operation is out-of-control. 
# A threshold is chosen by the operator to indicate when an alert should be raised. A suitable 
# threshold must be greater than the normally expected violation ratio. For example, 
# if we choose the 10th and the 90th percentile as control limits, the expected violation ratio is 20%, 
# because that is the violation ratio when scoring the baseline dataset against the control chart built using 
# itself. So, the operator probably wants to set a threshold of 25% or 30%.
control_charts_statistics_ <- function (data_y, data_x) {
	limits <- quantile_(data_y, c(.10, .50, .90))
	return (list(lower_bound=limits[1], median=limits[2], upper_bound=limits[3]))
}

train_n_test_type_5 <- function(index, data, plot=FALSE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("CC", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train
	analysis <- control_charts_statistics_(train_data_y_no_zero, train_data_x_no_zero)
	m <- analysis$median
	lower_bound <- analysis$lower_bound
	upper_bound <- analysis$upper_bound

	metric <- (train_data_y_no_zero > upper_bound) | (train_data_y_no_zero < lower_bound)
	outlier_indices <- which(metric)
	severity <- (train_data_y_no_zero[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(lower_bound , m, upper_bound))
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_line(aes(y=upper_bound),colour="red", size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(y=m),colour="black", size=1, linetype="dotted") +
			ggplot2::geom_line(aes(y=lower_bound),colour="black", size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
	metric <- (test_data_y > upper_bound) | (test_data_y < lower_bound)
	outlier_indices <- which(metric)
	severity <- (test_data_y[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("Test\n")
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}

		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	}
}

#####################################################
###
### Find outlier based on quantile reggression
###
#####################################################

quantile_regression_statistics_ <- function (data_y, data_x) {
	q <- rq(data_y~data_x, tau=c(.10, .50, .90))
	return (list(intercept_10=q$coefficients[1,1],intercept_50=q$coefficients[1,2], intercept_90=q$coefficients[1,3], 
		     slope_10 =q$coefficients[2,1],slope_50=q$coefficients[2,2], slope_90=q$coefficients[2,3]))
}

train_n_test_type_6 <- function(index, data, plot=TRUE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("QR+IQR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train
	fit <- quantile_regression_statistics_(train_data_y_no_zero, train_data_x_no_zero)
	slope <- fit$slope_50
	intercept <- fit$intercept_50

    	residuals <- train_data_y_no_zero - (train_data_x_no_zero*slope + intercept)

	analysis <- quantile_iqr_statistics_(residuals, c(1:length(residuals)))
	m <- analysis$median
	lower_bound <- analysis$lower_bound
	upper_bound <- analysis$upper_bound

	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(lower_bound , m, upper_bound))
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_abline(slope=slope, intercept=intercept, size=1, linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + upper_bound, size=0.8, color="red", linetype="dotted") +
			ggplot2::geom_abline(slope=slope, intercept=intercept + lower_bound, size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
    	residuals <- test_data_y - (test_data_x*slope + intercept)
	metric <- (residuals > upper_bound) | (residuals < lower_bound)
	outlier_indices <- which(metric)
	severity <- (residuals[outlier_indices] > upper_bound)

	if(!plot) {
		if(debug) {
			cat("Test\n")
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}

		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	}  else {
		return (list(havoc=test_data_x[outlier_indices]))
	}
}

# Quantile regression with cutoff decided by .9 and .1 quantile regression line
train_n_test_type_6_1 <- function(index, data, plot=TRUE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("QR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	# Train
	fit <- quantile_regression_statistics_(train_data_y_no_zero, train_data_x_no_zero)
	slope_10 <- fit$slope_10
	intercept_10 <- fit$intercept_10
	slope_50 <- fit$slope_50
	intercept_50 <- fit$intercept_50
	slope_90 <- fit$slope_90
	intercept_90 <- fit$intercept_90

    	upper_bound <- (train_data_x_no_zero*slope_90 + intercept_90)
    	lower_bound <- (train_data_x_no_zero*slope_10 + intercept_10)

	metric <- (train_data_y_no_zero > upper_bound) | (train_data_y_no_zero < lower_bound)
	outlier_indices <- which(metric)
	severity <- (train_data_y_no_zero[outlier_indices] > upper_bound[outlier_indices])

	if(!plot) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(c(lower_bound , m, upper_bound))
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=train_data_x_no_zero[outlier_indices], TS=train_data_y_no_zero[outlier_indices], S=severity )

			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + 
			ggplot2::geom_abline(slope=slope_50, intercept=intercept_50, size=1, linetype="dotted") +
			ggplot2::geom_abline(slope=slope_90, intercept=intercept_90, size=0.8, color="red", linetype="dotted") +
			ggplot2::geom_abline(slope=slope_10, intercept=intercept_10, size=0.8, linetype="dotted") +
			ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=train_data_x_no_zero[high_severity_indices], TS=train_data_y_no_zero[high_severity_indices] )
		outlier_df_low <- data.frame(D=train_data_x_no_zero[low_severity_indices], TS=train_data_y_no_zero[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
	}

	# Test
    	upper_bound <- (test_data_x*slope_90 + intercept_90)
    	lower_bound <- (test_data_x*slope_10 + intercept_10)
	metric <- (test_data_y > upper_bound) | (test_data_y < lower_bound)
	outlier_indices <- which(metric)
	severity <- (test_data_y[outlier_indices] > upper_bound[outlier_indices])

	if(!plot) {
		if(debug) {
			cat("Test\n")
			print(residuals)
		}
		if(length(outlier_indices) != 0) {
			outlier_df <- data.frame(D=test_data_x[outlier_indices], TS=test_data_y[outlier_indices], S=severity )

			havoc.test <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}

		}
	} else {

		high_severity_indices <- outlier_indices[severity]
		low_severity_indices <- outlier_indices[!severity]

		outlier_df_high <- data.frame(D=test_data_x[high_severity_indices], TS=test_data_y[high_severity_indices] )
		outlier_df_low <- data.frame(D=test_data_x[low_severity_indices], TS=test_data_y[low_severity_indices] )
		g <- g + 
			ggplot2::geom_point(data=outlier_df_high,colour="red", size=5) + 
			ggplot2::geom_point(data=outlier_df_low,colour="green", size=5) 
	}

	if(plot) {
		return (g)
	} else {
		return (havoc.test)
	}
}

#####################################################
###
### Find outlier based on General ESD Test (Extreme Studentized Deviate Test)
###
#####################################################

gesd_statistics_ <- function (data_y, data_x, alpha, value.zscore="NO", r=NA) {
	n <- length(data_y)

        # by default, set upper bound on number of outliers 'r' to  sample size
	if(is.na(r)){
          r <- floor(n*.49)
        } 

	R <- numeric(length=r)                                                # test statistics for 'r' outliers

	lambda <- numeric(length=r)                                           # critical values for 'r' outliers
	outlier_ind <- numeric(length=r)                                      # removed outlier observation indices
	outlier_val <- numeric(length=r)                                      # removed outlier observation values

	m <- 0                                                                # number of outliers
	data <- data_y                                                        # temporary observation values
	
	# Find outliers
	for(i in 1:r) {
	
		#### Compute test statistic ####
		if((value.zscore == "YES") | (value.zscore == "Y")){
			z <- abs(data)                                        # If Z-score is alrealy computed
		}else if((value.zscore == "NO") | (value.zscore == "N")){
			z <- abs(data - median_(data))/mad_(data)             # Based on robust statistics
		} else{
			stop("ERROR! Inappropriate value for value.score=[YES|NO]")
		}
		
		max_ind <- which(z==max(z),arr.ind=T)[1]                        # in case of ties, return first one
		R[i] <- z[max_ind]                                              # max Z-score
		outlier_val[i] <- data[max_ind]                                 # removed outlier observation values
		outlier_ind[i] <- which(data[max_ind] == data_y, arr.ind=T)[1]  # index of removed outlier observation values
		data <- data[-max_ind]                                          # remove observation that maximizes |x_i - x_mean|
		
		#### Compute critical values 
		p <- 1 - alpha/(2*(n-i+1))                                      # probability
		t_pv <- qt(p,df=(n-i-1))                                        # Critical value from Student's t distribution
		lambda[i] <- ((n-i)*t_pv) / (sqrt((n-i-1+t_pv^2)*(n-i+1)))
		
		# Find exact number of outliers: largest 'i' such that R_i > lambda_i 
		if(!is.na(R[i]) & !is.na(lambda[i])) {                          # qt can produce NaNs
			if (R[i] > lambda[i]) {
				m <- i
			}
		}
	}

	vals <- data.frame(NumOutliers=1:r, TestStatistic=R,
            CriticalValue=lambda, Val=outlier_val, Idx=outlier_ind)
        #print(vals)
	
	# Rank outlier observations
#       outlier_rank <- numeric(length=n)
        outlier_res <- numeric(length=0)
	if (m > 0) {
                outlier_res <- outlier_ind[1:m]
#               for (i in 1:m) {
#			outlier_rank[which(data_y==outlier_val[i])] <- i
#		}
	}
#	num_outliers <- sum(outlier_rank != 0) 
#	res <- c(num_outliers, outlier_rank)
#	names(res) <- c("Total", names(data_y))
	
#	return(res)	
	return(list(outlier_indices=outlier_res))	
}

train_n_test_type_7 <- function(index, data, print=FALSE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(!print) {
		label <- paste( paste("ESD", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	}
	
	analysis <- gesd_statistics_(Y, X, alpha=0.05)
	outlier_indices <- analysis$outlier_indices
        num_outliers <- length(outlier_indices)

	if(print) {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
			cat("Train\n")
			print(length(outlier_indices))
		}
		if(num_outliers != 0) {
			outlier_df <- data.frame(D=X[outlier_indices], TS=Y[outlier_indices])
			havoc.train <<- as.numeric(nrow(outlier_df))
			if(debug) {
				print(outlier_df)
			}
		}
	} else {
		g <- g + ggplot2::geom_line(aes(x=25),colour="black", size=1, linetype="dotted")
		if(num_outliers != 0) {
                      if(num_outliers <= 5) {
		        outlier_df_high <- data.frame(D=X[outlier_indices], TS=Y[outlier_indices] )
		        g <- g +
                        ggplot2::geom_point(data=outlier_df_high,colour="grey", size=2) 
                      } else {
		        outlier_df_high <- data.frame(D=X[outlier_indices[1:5]], TS=Y[outlier_indices[1:5]] )
		        g <- g + ggplot2::geom_point(data=outlier_df_high,colour="red", size=2) 

		        outlier_df_low <- data.frame(D=X[outlier_indices[6:num_outliers]],
                            TS=Y[outlier_indices[6:num_outliers]] )
		        g <- g +
                        ggplot2::geom_point(data=outlier_df_low,colour="green", size=2) 
                      }
                }
	}

	if(!print) {
		return (g)
	}
}

#####################################################
###
### Find  CUSUM changepoints
###
#####################################################

cusum_statistics_ <- function (data_y, debug=FALSE) {

    data <- rank_(data_y)
    #data <- (data_y)
    m <- mean(data)
    n <- length(data)

    # Find the cumulative sums
    S <- numeric(length=n)
    S[1] <- 0 + (data[1] - m) 
    for (i in 2:n) {
      S[i] <- S[i-1] + (data[i] - m)
    }

    s_max <- s_min <- 0
    s_max_index <- s_min_index <- 0
    for(i in 1:n) {

      if(S[i] > s_max) {
        s_max <- S[i]
        s_max_index <- i
      }
      
      if(S[i] < s_min) {
        s_min <- S[i]
        s_min_index <- i
      }
    }

    s_diff <- s_max - s_min
    if(debug) {
	print(paste(data_y, sep="", collapse=", "))
	print(paste(data, sep="", collapse=", "))
      cat("\n", "m= ", m, " s_max= ",  s_max, " s_min= ", s_min , " s_min_index= " , s_min_index, " s_max_index= " , 
	  s_max_index, " diff= ", s_diff, "\n")
    }

    if(abs(s_min) > abs(s_max) ) {
      s_max_index <- s_min_index
    }

    return (list(s_diff=s_diff, index=s_max_index))
}

changepoint_analysis_ <- function(data_y, iteration = 10000, conf.threshold = .90, debug=FALSE) {
    count <- 0

    analysis <- cusum_statistics_(data_y,  debug)
    
    if(debug) {
    	cat("\nuadjusted potential index= ", analysis$index, "\n")
    }

    test <- TRUE

    set.seed(123)
    if(test) {
    	data_r <- data_y
    }
    for(i in 1:iteration) {
    	if(test) {
      		data_r <- sample(data_r)
	} else {
      	data_r <- sample(data_y)
	}
      analysis_i <- cusum_statistics_(data_r, FALSE)

      #print(data_r)
      #print(analysis_i$s_diff)

      if(analysis_i$s_diff < analysis$s_diff) {
        count <- count + 1
      }
    }

    # Typically 90%, or 95% confidence is required before one states that a significant 
    # change has been detected.
    conf <- (count/iteration)

    if(debug) {
    	cat("\nconf= ", conf, "count = ", count , " conf= ", conf, " TH= ", conf.threshold, "\n")
    }

    if(conf > conf.threshold) {
      return (list(index=analysis$index, conf=conf))
    }

    return (list(index=0, conf=0))
}

find_segments_ <- function(data_y, first, last, debug=FALSE) {

  num_points <- last-first+1
  #if(num_points <= 3) {
  	if(num_points <= 1) {
    	stop("Error in segmentation !!")
    return (list())
  }

  if(num_points == 2) {
    return (list())
  }
  if(debug) {
  	cat("\n\t\t\t", "first: ", first, " last: ", last, "\n")
  }

  analysis <- changepoint_analysis_(data_y[first:last], 1000, .90, debug)
  if(0 == analysis$index) {
    return (list())
  }

  #analysis$index <- first + analysis$index -1
  analysis$index <- first + analysis$index 
  if(debug) {
  	cat("\n\t\t\t", " adjusted index= ", analysis$index, "\n")
  }

  if(analysis$index == first | analysis$index == last) {
    return (list())
  }

  return ( append( append( analysis , find_segments_((data_y), first, analysis$index -1, debug) ),
  		  find_segments_((data_y), analysis$index, last, debug)) )
  #return ( append( append( analysis , find_segments_(data_y, first, analysis$index - 1, debug) ),
  #		  find_segments_(data_y, analysis$index + 1, last, debug)) )
}

isSegmented <- function(index, data, plot=TRUE, debug=FALSE) {
	len <- as.numeric(length(data))
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: len])
	len_numeric <- length(Y)
	X <- c(1:len_numeric)


        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("Full Seg", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
	} else {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
		}
	}

	data_y <- Y[Y != 0]
	data_x <- X[Y != 0]

	segment_analysis <- find_segments_((data_y), 1, length(data_x), debug)
	num_analysis <- length(segment_analysis)

	segment_indices <- c()
	if(num_analysis != 0 ) {
		for(i in seq(from=1, to=num_analysis, by=2) ) {
			segment_indices <- append(segment_indices, segment_analysis[[i]])
		}
	}

	if(0 != length(segment_indices)) {
		sort_order <- order(segment_indices)
        	segment_indices <- segment_indices[sort_order]
	}
	cat("Segments:", segment_indices, ":\t")

	if(0 == length(segment_indices)) {
		if(plot) {
			return (g)
		} else {
			return (FALSE)
		}
	} else {
		if(plot) {
	       		g <- g + ggplot2::geom_vline(xintercept=data_x[segment_indices],colour="grey", size=2, linetype="dotted")
			return (g)
		} else {
			return (TRUE)
		}
	}
}

find_recent_segment_ <- function(data_y, data_x, g, plot=TRUE,  debug=FALSE) {

	max_recent_points <- 8
	n <- length(data_x)

	# Train: Find segments
        #segment_analysis <- find_segments_(rank_(data_y), 1, length(data_x), debug)
        segment_analysis <- find_segments_((data_y), 1, length(data_x), debug)
	num_analysis <- length(segment_analysis)

	segment_indices <- 1
	segment_conf <- 1
	if(num_analysis != 0 ) {
		for(i in seq(from=1, to=num_analysis, by=2) ) {
			segment_indices <- append(segment_indices, segment_analysis[[i]])
			segment_conf <- append(segment_conf, segment_analysis[[i+1]])
		}
	}
	segment_indices <- append(segment_indices, length(data_x))
	segment_conf <- append(segment_conf, 1)

	sort_order <- order(segment_indices)
        segment_indices <- segment_indices[sort_order]
        segment_conf <-	segment_conf[sort_order]

        segments.num <-  length(segment_indices) - 1
       	#segments.start.indices <-  data_x[segment_indices[1: (length(segment_indices) -1) ]]
       	#segments.end.indices <-  data_x[segment_indices[2:length(segment_indices)]]
       	segments.start.indices <-  segment_indices[1: (length(segment_indices) -1)]
       	segments.end.indices <-  segment_indices[2:length(segment_indices)]

	cat("Segments:", segment_indices, ":\t")
	if(debug) {
		print(data_x)
		print(segment_indices)
		print(segments.end.indices)
	}
	# Case I
	# For 3 segments with x indices : 2..............16..19.....25
	# segment sizes will be: (16-2) , (19 - 16) , (25 - 19 +1)
	# Case II
	# test_data_x have indices of non zero entries
	# While computing sizes we will not consider the zero entries
	segments.size <-  segments.end.indices - segments.start.indices 
	segments.size[length(segments.size)] = segments.size[length(segments.size)] + 1

	if(sum(segments.size) != length(data_x)) {
		print(segments.size)
		stop("Error in segment size calculation\n")
	}


	if(!plot) {
		if(debug) {
			cat("Train\n")
			print(data_x[segment_indices])
			print(segment_conf)
		}
	} else {
	       g <- g + ggplot2::geom_vline(xintercept=data_x[segment_indices],colour="grey", size=2, linetype="dotted")
	}

	# Find recent segment(s)
	end <- segment_indices[segments.num + 1]
	if(segments.num == 1) {
		return (list(start=segment_indices[1], end=end, indices=data_x[segment_indices], conf=segment_conf, g=g))
	} else {
		points_included <- n
		for (i in 1:segments.num) {
			points_included <- points_included  - segments.size[i]
			if(points_included < max_recent_points) {
				return (list(start=segment_indices[i], end=end, indices=data_x[segment_indices], 
					     conf=segment_conf, g=g))
			}
		}
	}

	return (list())
}






###########################################################################
#######################	TEST VARIATIONS ###################################
###########################################################################

# Type 2
train_n_test_type_2 <- function(index, data, plot=FALSE, debug=FALSE) {
	
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: 31])
	X <- c(2:31)

        plot_frame <- data.frame(D=X, TS=Y)
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
	if(plot) {
		label <- paste( paste("IQR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1)) +
			labs(title=label)
	} else {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
		}
	}
	
	# Prepare train data
	train_data_y <- Y[1:24]
	train_data_x <- X[1:24]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[25:30]
	test_data_x <- X[25:30]

	H <- find_havoc_using_iqr(train_data_y_no_zero, train_data_x_no_zero, test_data_y, test_data_x, g, plot, debug)

	if(plot) {
		return (H$g)
	} else {
		return (H)
	}
}

find_recent_2_segment_ <- function(data_y, data_x, g, plot=TRUE,  debug=FALSE) {

	max_recent_points <- 8
	n <- length(data_x)

	# Train: Find segments
        #segment_analysis <- find_segments_(rank_(data_y), 1, length(data_x), debug)
        segment_analysis <- find_segments_((data_y), 1, length(data_x), debug)
	num_analysis <- length(segment_analysis)

	segment_indices <- 1
	segment_conf <- 1
	if(num_analysis != 0 ) {
		for(i in seq(from=1, to=num_analysis, by=2) ) {
			segment_indices <- append(segment_indices, segment_analysis[[i]])
			segment_conf <- append(segment_conf, segment_analysis[[i+1]])
		}
	}
	segment_indices <- append(segment_indices, length(data_x))
	segment_conf <- append(segment_conf, 1)

	sort_order <- order(segment_indices)
        segment_indices <- segment_indices[sort_order]
        segment_conf <-	segment_conf[sort_order]

        segments.num <-  length(segment_indices) - 1
       	#segments.start.indices <-  data_x[segment_indices[1: (length(segment_indices) -1) ]]
       	#segments.end.indices <-  data_x[segment_indices[2:length(segment_indices)]]
       	segments.start.indices <-  segment_indices[1: (length(segment_indices) -1)]
       	segments.end.indices <-  segment_indices[2:length(segment_indices)]

	cat("Segments:", segment_indices, ":\t")
	if(debug) {
		print(data_x)
		print(segment_indices)
		print(segments.end.indices)
	}
	# Case I
	# For 3 segments with x indices : 2..............16..19.....25
	# segment sizes will be: (16-2) , (19 - 16) , (25 - 19 +1)
	# Case II
	# test_data_x have indices of non zero entries
	# While computing sizes we will not consider the zero entries
	segments.size <-  segments.end.indices - segments.start.indices 
	segments.size[length(segments.size)] = segments.size[length(segments.size)] + 1

	if(sum(segments.size) != length(data_x)) {
		print(segments.size)
		stop("Error in segment size calculation\n")
	}


	if(!plot) {
		if(debug) {
			cat("Train\n")
			print(data_x[segment_indices])
			print(segment_conf)
		}
	} else {
	       g <- g + ggplot2::geom_vline(xintercept=data_x[segment_indices],colour="grey", size=2, linetype="dotted")
	}

	# Find recent segment(s)
	end <- segment_indices[segments.num + 1]
	if(segments.num == 1) {
		return (list(start=segment_indices[1], end=end, indices=data_x[segment_indices], conf=segment_conf, g=g))
	} 

	return (list(start=segments.start.indices[segments.num-1], end=end, indices=data_x[segment_indices], conf=segment_conf, g=g))
}
# Type 9
train_n_test_type_9 <- function(index, data, relative, plot=TRUE, debug=FALSE) {
	len <- as.numeric(length(data))
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: len])
	len_numeric <- length(Y)
	#X <- c(2:len)
	X <- c(1:len_numeric)

        plot_frame <- data.frame(D=X, TS=Y, R=as.numeric(relative))
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
        gr <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=R)) 
	if(plot) {
		label <- paste( paste("CP+IQR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
		gr <- gr + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))
			labs(title="Relative Plot")
	} else {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
		}
	}

	
	train_num <- as.integer(len_numeric)
	# Prepare train data
	train_data_y <- Y
	train_data_x <- X

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[len_numeric]
	test_data_x <- X[len_numeric]

	if(debug) {
		cat("Train data: " , train_data_x_no_zero[1], ",", train_data_x_no_zero[length(train_data_x_no_zero)], "\n")
		cat("Test data: " , X[(train_num + 1)], ",", X[len_numeric], "\n")
	}

	# Find recent Train Data
	segment_analysis <- find_recent_2_segment_(train_data_y_no_zero, train_data_x_no_zero, g, plot, debug)
	if(length(segment_analysis) == 0) {
		stop("Error Segment Analysis")
	}

	start <- segment_analysis$start
	end <- length(train_data_x_no_zero)
	g <- segment_analysis$g

	if(plot) {
		g <- g + ggplot2::geom_vline(xintercept=c(train_data_x_no_zero[start], train_data_x_no_zero[end]),colour="black", size=0.5)
	}
	if (debug) {
		cat("Recent Segment: " , start, ",", end, "\n")
	}


	train_data_y_no_zero_recent <- train_data_y_no_zero[start:(end-1)]
	train_data_x_no_zero_recent <- train_data_x_no_zero[start:(end-1)]

	H <- find_havoc_using_iqr(train_data_y_no_zero_recent, train_data_x_no_zero_recent, test_data_y, test_data_x, g, plot, debug)
	g <- H$g

	if(debug) {
		print(H$havoc)
	}

	if(plot) {
      		gt <- grid.arrange(g, gr, nrow=2) 
		#gt <- arrangeGrob(g, gr, nrow=2)
		ggsave(file="x.png", gt)
		return (gt)
	} else {
		return (list(havoc=H$havoc, severity=H$severity, median=H$median))
	}
}

# Type 8
train_n_test_type_8 <- function(index, data, relative, plot=FALSE, debug=FALSE) {
	len <- as.numeric(length(data))
	func_name <- as.character(data[[1]])
	Y <- as.numeric(data[2: len])
	len_numeric <- length(Y)
	#X <- c(2:len)
	X <- c(1:len_numeric)

        plot_frame <- data.frame(D=X, TS=Y, R=as.numeric(relative))
        g <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=TS)) 
        gr <- ggplot2::ggplot(plot_frame, ggplot2::aes(x=D, y=R)) 
	if(plot) {
		label <- paste( paste("CP+IQR", func_name, sep=" ("), ")", sep="")
		g <- g + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))+
			labs(title=label)
		gr <- gr + 
			ggplot2::geom_point() + 
			ggplot2::geom_line(colour="blue") + 
			scale_x_continuous(breaks = seq(min(plot_frame$D), max(plot_frame$D), by = 1))
			labs(title="Relative Plot")
	} else {
		if(debug) {
			cat("\nProcessing function: ", func_name, "(", index, ")\n")
		}
	}

	
	train_num <- as.integer(0.8 * len_numeric)
	# Prepare train data
	train_data_y <- Y[1:train_num]
	train_data_x <- X[1:train_num]

	train_data_y_no_zero <- train_data_y[train_data_y != 0]
	train_data_x_no_zero <- train_data_x[train_data_y != 0]

	# Prepare test data
	test_data_y <- Y[(train_num + 1) : len_numeric]
	test_data_x <- X[(train_num + 1) : len_numeric]

	if(debug) {
		cat("Train data: " , train_data_x_no_zero[1], ",", train_data_x_no_zero[length(train_data_x_no_zero)], "\n")
		cat("Test data: " , X[(train_num + 1)], ",", X[len_numeric], "\n")
	}

	# Find recent Train Data
	segment_analysis <- find_recent_segment_(train_data_y_no_zero, train_data_x_no_zero, g, plot, debug)
	if(length(segment_analysis) == 0) {
		stop("Error Segment Analysis")
	}

	start <- segment_analysis$start
	end <- segment_analysis$end
	g <- segment_analysis$g

	if(plot) {
		g <- g + ggplot2::geom_vline(xintercept=c(train_data_x_no_zero[start], train_data_x_no_zero[end]),colour="black", size=0.5)
	}
	if (debug) {
		cat("Recent Segment: " , start, ",", end, "\n")
	}


	train_data_y_no_zero_recent <- train_data_y_no_zero[start:end]
	train_data_x_no_zero_recent <- train_data_x_no_zero[start:end]

	H <- find_havoc_using_iqr(train_data_y_no_zero_recent, train_data_x_no_zero_recent, test_data_y, test_data_x, g, plot, debug)
	g <- H$g

	if(debug) {
		print(H$havoc)
	}

	# If the actual last test point is an havoc
        last_havoc_index <- test_data_x[length(test_data_x)]
	class <- 0
	#if(last_havoc_index %in% H$havoc) {

	if(plot) {
      		gt <- grid.arrange(g, gr, nrow=2) 
		#gt <- arrangeGrob(g, gr, nrow=2)
		ggsave(file="x.png", gt)
		return (gt)
	} else {
		return (list(havoc=H$havoc, severity=H$severity, median=H$median))
	}
}



classify_havoc_ <- function(test_data_y, test_data_x, train_data_y_no_zero_recent, debug=FALSE) {
        last_havoc_index <- test_data_x[length(test_data_x)]
	
	segment_analysis <- find_segments_(rank_(test_data_y), 1, length(test_data_x), debug)
	num_analysis <- length(segment_analysis)

	segment_indices <- 1
	segment_conf <- 1
	if(num_analysis != 0 ) {
		for(i in seq(from=1, to=num_analysis, by=2) ) {
			segment_indices <- append(segment_indices, segment_analysis[[i]])
			segment_conf <- append(segment_conf, segment_analysis[[i+1]])
		}
	}
	segment_indices <- append(segment_indices, length(test_data_x))
	segment_conf <- append(segment_conf, 1)

	sort_order <- order(segment_indices)
        segment_indices <- segment_indices[sort_order]

	if(debug) {
		print(segment_indices)
	}
	
	havoc_containing_segment_start <- segment_indices[length(segment_indices) - 1 ]
	havoc_containing_segment_end <- segment_indices[length(segment_indices)]
	size_of_last_segment <- havoc_containing_segment_end - havoc_containing_segment_start + 1

	if(debug) {
		cat("Size: ", size_of_last_segment, "\n")
	}

	# Check if size of the last segemnt is 1, then report  "point anomaly"
	if(1 == size_of_last_segment) {
		return (1)
	}


	#Check the slope of the last segment
	havoc_containing_segment_data_y <- test_data_y[havoc_containing_segment_start:havoc_containing_segment_end]
	havoc_containing_segment_data_x <- test_data_x[havoc_containing_segment_start:havoc_containing_segment_end]

	fit <- lm_(havoc_containing_segment_data_y, havoc_containing_segment_data_x)

	# If the line fitting the  last segment containing havoc has a positive slope, report "creping up", else "creeping down" 
	# for negative slope
	if(fit$slope > 0) {
		return (2)
	} else if(fit$slope < 0 ) {
		return (3)
	} else {
		return (1)
	}
}
