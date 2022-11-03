## code to prepare `project2022` data set goes here

project2022 <- read.csv("C:/Users/hemis/OneDrive/Desktop/UNI 2022/Sem 2 Working Directory/STAT1378/Project/project2022.csv")
View(project2022)




usethis::use_data(project2022, overwrite = TRUE)
