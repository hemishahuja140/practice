gender <- NULL
height <- NULL
#Importing data set



#Function that outputs the null and alternate hypothesis
#' Hypothesis_t
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
#' @examples hypothesis_t(project2022)
hypothesis_t <- function(filename){

  cat("NULL HYPOTHESIS","Mean height of males = Mean height of females",sep = "\n") #Null hypothesis
  cat(sep = "\n") #Break between null and alternate
  cat("ALTERNATE HYPOTHESIS","Mean height of males not = Mean height of females",sep = "\n") #Alternate hypothesis

}



#' Assumptions_t
#'
#' @param filename dataset that you want to be tested
#'
#' @return graphical and numerical summary required to complete the test
#' @export
#' @examples assumptions_t(project2022)
assumptions_t <- function(filename) {
  data <- filename %>%
    group_by(gender) %>%
    summarise(avg_height = mean(height), sd_height = sd(height), no. = length(height))
  cat("1. Two samples are independent from each other", "2. Observations are independent from each other", sep = "\n")
  s1 <- max(data$sd_height)
  s2 <- min(data$sd_height)
  s1_div_s2 <- s1/s2
  equalvar_assum <- if(s1_div_s2 <= 2) {
    cat("3. Since the ratio of standard deviations is less than 2, we can assume equal variance between the two populations")
  } else {
    cat("3. Since the ratio of standard deviations is greater than 2, we cannot assume equal variance between the two populations")
  }
  print(equalvar_assum)
  qqnorm(filename$height)
  qqline(filename$height)
  cat("4. Data is approximately normally distributed, which can be proven through the upward sloping line in the qq plot.")
}

#' Fit_t
#'
#' @param filename dataset that you want to be tested
#'
#' @return summary of your important values to carry out the test
#' @export
fit_t <- function(filename) {
  Male <- filename %>%
    filter(gender == "Male") %>%
    as.data.frame() %>%
    select(height)
  Female <- filename %>%
    filter(gender == "Female") %>%
    as.data.frame() %>%
    select(height)
  model <- t.test(Male,Female, var.equal = TRUE)
  data_stats<- tidy(model)
  sample_diff <- data_stats$estimate
  p.value <- data_stats$p.value
  test_stat <- data_stats$statistic
  df <- data_stats$parameter
  lower_interval <- data_stats$conf.low
  upper_interval <- data_stats$conf.high
  values <- list("ESTIMATED DIFFERENCE" = sample_diff, "CONFIDENCE INTERVAL" = glue("({lower_interval},{upper_interval})"), "TEST-STATISTIC" = test_stat, "DEGREES OF FREEDOM" = df, "P-VALUE" = p.value)
  print(values)
}



#Decision
#' Decision_t
#'
#' @param filename dataset that you want to be tested
#'
#' @return the decision of your test based on your p.value and a signifance level of 5%
#' @export
decision_t <- function(filename) { #writes new method for class mylm
  Male <- filename %>%
    filter(gender == "Male") %>%
    as.data.frame() %>%
    select(height)
  Female <- filename %>%
    filter(gender == "Female") %>%
    as.data.frame() %>%
    select(height)
  model <- t.test(Male,Female, var.equal = TRUE)
  data_stats<- tidy(model)
  p.value <- data_stats$p.value
  cat("DECISION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("Since the p.value of {p.value} is greater than the significance level of 5%, we retain the null hypothesis") #retaining null if p.value greater than 0.05
  } else {
    glue("Since the p.value of {p.value} is less than the significance level of 5%, we reject the null hypothesis") #rejecting null if p.value less than 0.05
  }
}


#Conclusion
#' Conclusion_t
#'
#' @param filename dataset that you want to be tested
#'
#' @return the answer to your research question based on your decision
#' @export
conclusion_t <- function(filename) { #writes new method for class mylm
  Male <- filename %>%
    filter(gender == "Male") %>%
    as.data.frame() %>%
    select(height)
  Female <- filename %>%
    filter(gender == "Female") %>%
    as.data.frame() %>%
    select(height)
  model <- t.test(Male,Female, var.equal = TRUE)
  data_stats<- tidy(model)
  p.value <- data_stats$p.value
  cat("CONCLUSION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("As we have retained the null hypothesis, there is sufficient evidence to suggest that the mean height of males and females are the same.")
  } else {
    glue("As we have rejected the null hypothesis, there is sufficient evidence to suggest that the alternate hypothesis is true. This means the mean height of males and females are different. ")
  }

}


#Function that outputs eveyrthing in a report
#' Mytest_t
#'
#' @param filename dataset that you want to be tested
#'
#' @return all functions outputted into a nicely formatted report
#' @export
mytest_t <- function(filename){
  cat("TWO SAMPLE T-TEST FOR MALES AND FEMALES HEIGHT", sep = "\n") #title of report

  cat(sep = "\n") #line break

  cat(hypothesis_t(filename)) #hypothesis

  cat(sep = "\n") #line break

  print(assumptions_t(filename)) #graphs

  fit1 <- fit_t(filename) #assigning fit1 to output of fit(filename)

  print(decision_t(fit1)) #decision

  cat(sep = "\n") #line break

  print(conclusion_t(fit1)) #conclusion

}







