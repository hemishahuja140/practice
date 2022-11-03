gender <- NULL
height <- NULL
phys <- NULL

#Importing data set



#' Hypothesis_chi
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
#' @examples hypothesis_chi(project2022)
hypothesis_chi <- function(filename){
  cat("NULL HYPOTHESIS","Gender and amount of physical activity are independent from each other.",sep = "\n") #Null hypothesis
  cat(sep = "\n") #Break between null and alternate
  cat("ALTERNATE HYPOTHESIS","Geder and amount of physical relationship are not independent from each other.",sep = "\n") #Alternate hypothesis

}


#' Assumptions_chi
#'
#' @param filename dataset that you want to be tested
#'
#' @return graphical and numerical summary required to complete the test
#' @export
#' @examples assumptions_chi(project2022)
assumptions_chi <- function(filename){
  data2 <- tabyl(filename,gender,phys) %>%
    select(-gender)
  names(data2) <- NULL
  chi <- chisq.test(data2)
  cat("1. Gender and amount of physical activity are categorical", "2. All observations are independent from each other", sep = "\n")
  expect <- data2$expected
  if(any(expect$Intense & expect$Moderate & expect$None < 5) == FALSE ){
    cat("3. All expected frequencies are large enough (greater than 5)")
  } else{
    stop("Expected frequencies are not large enough for a chi-squared test to be completed")
  }
}

#' Fit_chi
#'
#' @param filename dataset that you want to be tested
#'
#' @return summary of your important values to carry out the test
#' @export
#' @examples fit_chi(project2022)
fit_chi <- function(filename) {
  cat("VALUES", sep = "\n")
  data2 <- tabyl(filename,gender,phys) %>%
    select(-gender)
  names(data2) <- NULL
  chi <- chisq.test(data2)
  test_stat <- chi$statistic
  p.value <- chi$p.value
  test_stat <- chi$statistic
  df <- chi$parameter
  values <- list("TEST STATISTIC" = test_stat, "DEGREES OF FREEDOM" = df, "P-VALUE" = p.value)
  print(values)
}


#Decision
#' Decision_chi
#'
#' @param filename dataset that you want to be tested
#'
#' @return the decision of your test based on your p.value and a significance level of 5%
#' @export
#' @examples decision_chi(project2022)
decision_chi <- function(filename) {
  data2 <- tabyl(filename,gender,phys) %>%
    select(-gender)
  names(data2) <- NULL
  chi <- chisq.test(data2)
  p.value <- chi$p.value
  cat("DECISION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("Since the p.value of {p.value} is greater than the significance level of 5%, we retain the null hypothesis") #retaining null if p.value greater than 0.05
  } else {
    glue("Since the p.value of {p.value} is less than the significance level of 5%, we reject the null hypothesis") #rejecting null if p.value less than 0.05
  }
}


#Conclusion
#' Conclusion_chi
#'
#' @param filename dataset that you want to be tested
#'
#' @return the answer to your research question based on your decision
#' @export
#' @examples conclusion_chi(project2022)
conclusion_chi <- function(filename) { #writes new method for class mylm
  data2 <- tabyl(filename,gender,phys) %>%
    select(-gender)
  names(data2) <- NULL
  chi <- chisq.test(data2)
  p.value <- chi$p.value
  cat("CONCLUSION", sep = "\n") #sub-heading
  if(p.value > 0.05) {
    glue("As we have retained the null hypothesis, there is sufficient evidence to suggest that there is no relationship between gender and amount of physical activity.")
  } else {
    glue("As we have rejected the null hypothesis, there is sufficient evidence to suggest that the alternate hypothesis is true. This means that there exists a relationship between gender and the amount of physical activity.")
  }

}


#Function that outputs eveyrthing in a report
#' Mytest_chi
#'
#' @param filename dataset that you want to be tested
#'
#' @return all functions outputted into a nicely formatted report
#' @export
#' @examples mytest_chi(project2022)
mytest_chi <- function(filename){
  cat("TESTING FOR INDEPENDENCE BETWEEN GENDER AND AMOUNT OF PHYSICAL ACTIVITY", sep = "\n") #title of report

  cat(sep = "\n") #line break

  cat(hypothesis_chi(filename)) #hypothesis

  cat(sep = "\n") #line break

  print(assumptions_chi(filename)) #graphs

  cat(sep = "\n")

  print(fit_chi(filename))

  print(decision_chi(filename)) #decision

  cat(sep = "\n") #line break

  print(conclusion_chi(filename)) #conclusion

}
