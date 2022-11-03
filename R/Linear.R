gender <- NULL
height <- NULL
#Importing data set


# Selecting the x and y variable for testing the regression model


#Function that outputs the null and alternate hypothesis

#' Hypothesis_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return the null and alternate hypothesis
#' @export
#' @importFrom janitor tabyl
#' @importFrom broom tidy
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by select summarise slice
#' @importFrom stats filter qqline qqnorm sd t.test chisq.test fitted lm resid
#' @importFrom ggplot2 aes geom_histogram geom_hline geom_point geom_smooth ggplot ggtitle xlab ylab
#' @examples  hypothesis_linear(project2022)
hypothesis_linear <- function(filename){

  cat("NULL HYPOTHESIS","Slope = 0",sep = "\n") #Null hypothesis
  cat(sep = "\n") #Break between null and alternate
  cat("ALTERNATE HYPOTHESIS","Slope not = 0",sep = "\n") #Alternate hypothesis

}

#Function that outputs the three required graphs
#' Assumptions_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return graphical and numerical summary required to complete the test
#' @export
#' @examples assumptions_linear(project2022)

assumptions_linear <- function(filename){
  Y <- filename$weight
  X <- filename$height
  #scatter plot of y vs x
  y_vs_x <- ggplot(filename, aes(Y,X)) +
    geom_point() +
    geom_smooth(method = lm, se = TRUE) + #line of best fit
    ggtitle ("Y vs X") #title of graph

  model <- lm(Y ~ X) #defining linear regression model

  #scatter plot of residual distance against fitted values
  scatter_plot <- ggplot(data = model, mapping = aes(fitted(model), resid(model))) +
    geom_point() +
    ggtitle("Scatter Plot of Residual Distance against Fitted Values") + #title of graph
    geom_hline(yintercept = 0, color = "red") + #horizontal line through y = 0
    xlab("fitted values") + #label of x-axis
    ylab("residual distance") #label of y-axis

  #histogram showing frequency of residual distance
  histogram <- ggplot(data = model) +
    geom_histogram(mapping = aes(resid(model)), binwidth = 7, col = "white", fill = "black") +
    ggtitle("Histogram of Residual Distance") + #title of graph
    xlab("residual distance") #label of x-axis

  y_vs_x
  scatter_plot
  histogram

  cat("1. Relationship between x and y is linear, as shown in the y vs x graph.", sep = "\n",
      "2. Y-values are normally distributed about the regression line")
  cat("3. Residuals are independently and normally distributed", "\n",
      "4. Residuals have constant variance")

}

#Function that outputs the required statistics for the regression model
#' Fit_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return summary of your important values to carry out the test
#' @export
#' @examples fit_linear(project2022)
fit_linear <- function(filename){
  Y <- filename$weight
  X <- filename$height
  regression <- lm(Y~X, data = filename) #fits linear regression model
  tibble <- tidy(regression) %>% #tibble that displays key statistics from model
    slice(-1) #removes second row of table

  #Extracts particular values from table
  slope <- tibble$estimate
  lower_interval <- sum(slope,-tibble$std.error)
  upper_interval <- sum(slope,tibble$std.error)
  p.value <- tibble$p.value #'<<-' makes p.value a global element to use for decision and conclusion
  df <- regression$df.residual
  test_stat <- tibble$statistic

  #List that prints the extracted values
  values <- list("SLOPE" = slope, "CONFIDENCE INTERVAL" = glue("({lower_interval},{upper_interval})"), "TEST-STATISTIC" = test_stat, "DEGREES OF FREEDOM" = df, "P-VALUE" = p.value)
  print(values)
}

#Decision
#' Decision_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return the decision of your test based on your p.value and a signifance level of 5%
#' @export
#' @examples decision_linear(project2022)
decision_linear <- function(filename) { #writes new method for class mylm
  Y <- filename$weight
  X <- filename$height
  regression <- lm(Y~X, data = filename) #fits linear regression model
  tibble <- tidy(regression) %>% #tibble that displays key statistics from model
    slice(-1) #removes second row of table
  p.value <- tibble$p.value
  cat("DECISION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("Since the p.value of {p.value} is greater than the significance level of 5%, we retain the null hypothesis") #retaining null if p.value greater than 0.05
  } else {
    glue("Since the p.value of {p.value} is less than the significance level of 5%, we reject the null hypothesis") #rejecting null if p.value less than 0.05
  }
}


#Conclusion
#' Conclusion_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return the answer to your research question based on your decision
#' @export
#' @examples conclusion_linear(project2022)
conclusion_linear <- function(filename) { #writes new method for class mylm
  Y <- filename$weight
  X <- filename$height
  regression <- lm(Y~X, data = filename) #fits linear regression model
  tibble <- tidy(regression) %>% #tibble that displays key statistics from model
    slice(-1) #removes second row of table
  p.value <- tibble$p.value
  cat("CONCLUSION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("As we have retained the null hypothesis, there is sufficient evidence to suggest that there is a lack of relationship between the X and Y variable.")
  } else {
    glue("As we have rejected the null hypothesis, there is sufficient evidence to suggest that the alternate hypothesis is true. This means that there exists a relationship between the X and Y variable. ")
  }

}


#Function that outputs eveyrthing in a report
#' Mytest_linear
#'
#' @param filename dataset that you want to be tested
#'
#' @return all functions outputted into a nicely formatted report
#' @export

mytest_linear <- function(filename){
  cat("HYPOTHESIS TESTING FOR LINEAR REGRESSION DATA", sep = "\n") #title of report

  cat(sep = "\n") #line break

  cat(hypothesis_linear(filename)) #hypothesis

  cat(sep = "\n") #line break

  print(assumptions_linear(filename)) #graphs

  fit1 <- fit_linear(filename) #assigning fit1 to output of fit(filename)

  print(decision_linear(fit1)) #decision

  cat(sep = "\n") #line break

  print(conclusion_linear(fit1)) #conclusion

}
