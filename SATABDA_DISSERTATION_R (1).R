install.packages("MASS", repos = "https://cloud.r-project.org/")
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization
library(tidyr)   # For data tidying
library(lmtest)  # For model diagnostics
library(MASS)    # For logistic regression
library(readxl)
#####
#####
# TOTAL NUMBER OF  FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

file_path = ("C:/Users/pc1/Desktop/ASSIGNMENTS/SATABDA_DISSERTATION_1 (Autosaved)2.xlsx")
data = read_excel(file_path, sheet = "Sheet5",range = "R7C2:R22C7" )
print(data)
model <- glm(TOTAL_NUMBER_OF_MIGRANTS ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data, family = poisson())
summary(model)
fitted(model)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF  FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

residuals1 <- residuals(model)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals1)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")

# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS

library(lmtest)
fitted_values <- fitted(model)
residuals1 <- residuals(model)
lm_squared_resid <- lm(residuals1^2 ~ fitted_values)
bptest(lm_squared_resid)


######
######
# FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

data1 = read_excel(file_path, sheet = "Sheet5",range = "R7C9:R21C14" )
print(data1)
model1 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data1, family = poisson())
summary(model1)
fitted(model1)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

residuals2 <- residuals(model1)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals2)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")


# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS

library(lmtest)
fitted_values <- fitted(model1)
residuals2 <- residuals(model1)
lm_squared_resid <- lm(residuals2^2 ~ fitted_values)
bptest(lm_squared_resid)



#####
#####
# # FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

data2 = read_excel(file_path, sheet = "Sheet5",range = "R7C16:R21C21" )
print(data2)
library(MASS)
model2 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data2, family = poisson())
summary(model2)
fitted(model2)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

residuals3 <- residuals(model2)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals3)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")

# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS

library(lmtest)
fitted_values <- fitted(model2)
residuals3 <- residuals(model2)
lm_squared_resid <- lm(residuals3^2 ~ fitted_values)
bptest(lm_squared_resid)


#####
#####
# FOR TOTAL NUMBER OF TOTAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

data3 = read_excel(file_path, sheet = "Sheet5",range = "R30C2:R44C7" )
print(data3)
model3 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data3, family = poisson())
summary(model3)
fitted(model3)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF  FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

residuals4 <- residuals(model3)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals4)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")


# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF  FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS

library(lmtest)
fitted_values <- fitted(model3)
residuals4 <- residuals(model3)
lm_squared_resid <- lm(residuals4^2 ~ fitted_values)
bptest(lm_squared_resid)


#####
#####
# FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

data4 = read_excel(file_path, sheet = "Sheet5",range = "R30C9:R44C14" )
print(data4)
model4 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data4, family = poisson())
summary(model4)
fitted(model4)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

residuals5 <- residuals(model4)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals5)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")


# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS

library(lmtest)
fitted_values <- fitted(model4)
residuals5 <- residuals(model4)
lm_squared_resid <- lm(residuals5^2 ~ fitted_values)
bptest(lm_squared_resid)



#####
#####
# FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

data5 = read_excel(file_path, sheet = "Sheet5",range = "R30C16:R44C21" )
print(data5)
model5 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data5, family = poisson())
summary(model5)
fitted(model5)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

residuals6 <- residuals(model5)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals6)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")



# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS

library(lmtest)
fitted_values <- fitted(model5)
residuals6 <- residuals6(model5)
lm_squared_resid <- lm(residuals^2 ~ fitted_values)
bptest(lm_squared_resid)



#####
#####
# FOR TOTAL NUMBER OF TOTAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

data6 = read_excel(file_path, sheet = "Sheet5",range = "R51C2:R64C7" )
print(data6)
model6 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data6, family = poisson())
summary(model6)
fitted(model6)


#GENERATING QQ PLOT FOR TOTAL NUMBER OF FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

residuals7 <- residuals(model6)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals7)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF  FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")


# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS

library(lmtest)
fitted_values <- fitted(model6)
residuals7 <- residuals(model6)
lm_squared_resid <- lm(residuals7^2 ~ fitted_values)
bptest(lm_squared_resid)


#####
#####
# FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

data7 = read_excel(file_path, sheet = "Sheet5",range = "R51C9:R64C14" )
print(data7)
model7 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data7, family = poisson())
summary(model7)
fitted(model7)

#GENERATING QQ PLOT FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

residuals8 <- residuals(model7)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals8)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")


# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS

library(lmtest)
fitted_values <- fitted(model7)
residuals8 <- residuals(model7)
lm_squared_resid <- lm(residuals8^2 ~ fitted_values)
bptest(lm_squared_resid)




#####
#####
# FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

data8 = read_excel(file_path, sheet = "Sheet5",range = "R51C16:R64C21" )
print(data8)
model8 <- glm(`TOTAL NUMBER OF MIGRANTS` ~ `NUMBER OF MIGRANTS DUE TO WORK EMPLOYMENT` + `NUMBER OF MIGRANTS DUE TO BUSINESS` + `NUMBER OF MIGRANTS DUE TO EDUCATION` + `NUMBER OF MIGRANTS DUE TO MARRIAGE`, data = data8, family = poisson())
summary(model8)
fitted(model8)


#GENERATING QQ PLOT FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

residuals9 <- residuals(model8)
library(ggplot2)
ggplot(data = NULL, aes(sample = residuals9)) +
  stat_qq(distribution = qnorm) +  # Q-Q plot against normal distribution
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add line of equality
  labs(title = "QQ PLOT OF POISSON REGRESSION RESIDUALS FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS", 
       x = "Theoretical Quantiles", y = "Sample Quantiles")



# Perform Breusch-Pagan test for heteroscedasticity FOR TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS

library(lmtest)
fitted_values <- fitted(model8)
residuals9 <- residuals(model8)
lm_squared_resid <- lm(residuals9^2 ~ fitted_values)
bptest(lm_squared_resid)





# TOTAL NUMBER OF  FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

# Create a data frame with the provided data
data <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(163275, 1854139, 443445, 2246796, 6067612, 19871,
                     669621, 1451881, 1981543, 886352, 1516952, 1195536,
                     1061732, 3602243),
  Work_Employment = c(5422, 67966, 39581, 64358, 244890, 627, 40895,
                      65103, 81690, 26541, 77216, 68378, 76790, 148008),
  Business = c(729, 11277, 1693, 15854, 26403, 69, 2711, 7172, 7748,
               7152, 7653, 6804, 8202, 17403),
  Education = c(2857, 9380, 3151, 8875, 35140, 1336, 4193, 8665, 9840,
                7403, 8014, 11485, 9591, 22696),
  Marriage = c(58578, 980892, 255876, 1351148, 3003996, 8025, 386746,
               914724, 1296694, 451521, 809682, 610420, 471628, 1775188)
)

# Selecting numeric columns for clustering
clustering_data <- data[, c("State","Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF  FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS")

#TOTAL NUMBER OF RURAL FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

# Create a data frame with the provided data
data1 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(62591,717125,243928,969240,1593933,11794,324730,694380,1060812,33926,690278,428635,305232,1168141),
  Work_Employment = c(1457,21770,22040,20598,60897,228,15444,15021,36535,5555,25828,16896,19264,49614),
  Business = c(141,2057,566,3168,3654,30,617,1846,2228,777,2163,1503,1540,3895),
  Education = c(578,1752,594,1408,4602,372,879,1155,2081,1039,1968,2430,1620,2527),
  Marriage = c(28561,499781,165815,790302,1167453,5948,249881,572596,833763,165061,459538,305326,174882,843359)
)

# Selecting numeric columns for clustering
clustering_data <- data1[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF RURAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS")
print(data1)


#TOTAL NUMBER OF URBAN FEMALE  MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS					

# Create a data frame with the provided data
data2 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(100684,1137014,199517,1277556,4473679,8077,344891,757501,920731,652426,826674,766901,756500,2434102),
  Work_Employment = c(3965,46196,17541,43760,183993,399,25451,50082,45155,20986,51388,51482,57526,98394),
  Business = c(588,9220,1127,12686,22749,39,2094,5326,5520,6375,5490,5301,6662,13508),
  Education = c(2279,7628,2557,7467,30538,964,3314,7510,7759,6364,6046,9055,7971,20169),
  Marriage = c(30017,481111,90061,560846,1836543,2077,136865,342128,462931,286460,350144,305094,296746,931829)
)

# Selecting numeric columns for clustering
clustering_data <- data2[, c("State","Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF URBAN FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2011 CENSUS")
print(data2)


# TOTAL NUMBER OF TOTAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

# Create a data frame with the provided data
data3 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(142936,1281334	,521464,1515575,4324799,11822,465593,936167,1322193,753890,1073354,897875,848841,2211222),
  Work_Employment = c(3912,41582,58999,37823,136114,319,23426,36156,50456,17030,62378,47811,67654,80534),
  Business = c(514,8169,1061,8630,10524,48,1063,3252,3754,3125,2482,3961,5098,7612),
  Education = c(2501,4992,3232,4332,17617,598,2012,5455,4658,4913,4826,6525,7315,9954),
  Marriage = c(74556,690012,256378,925526,2222702,4707,273715,556175,850380,414533,569086,472357,382995,1074114)
)

# Selecting numeric columns for clustering
clustering_data <- data3[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS")


# TOTAL NUMBER OF  RURAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

# Create a data frame with the provided data
data4 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(72984,546815,332445,717956,1399887,6714,254770,441349,753560,222753,486832,373939,288783,809825),
  Work_Employment = c(1448,17044,41019,16568,43619,125,11695,10310,26757,4484,27263,18776,27826,31365),
  Business = c(213,3589,535,2736,2607,26,349,959,1809,638,765,1298,1644,2449),
  Education = c(502,1054,395,784,2805,161,430,764,1041,630,887,968,1178,1611),
  Marriage = c(53901,361209,184764,556865,1002454,3586,185962,339289,561363,160062,312139,249302,150917,537158)
)

# Selecting numeric columns for clustering
clustering_data <- data4[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF RURAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS")



# TOTAL NUMBER OF  URBAN FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS					

# Create a data frame with the provided data
data5 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Chattishgarh", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(69952,734519,189019,797619,2924912,5108,210823,494818,568633,531137,586522,523936,560058,1401397),
  Work_Employment = c(2464,24538,17980,21255,92495,194,11731,25846,23699,12546,35115,29035,39828,49169),
  Business = c(301,4580,526,5894,7917,22,714,2293,1945,2487,1717,2663,3454,5163),
  Education = c(1999,3938,2837,3548,14812,437,1582,4691,3617,4283,3939,5557,6137,8343),
  Marriage = c(20655,328803,71614,368661,1220248,1121,87753,216886,289017,254471,256947,223055,232078,536956)
)

# Selecting numeric columns for clustering
clustering_data <- data5[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF URBAN FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 2001 CENSUS")



# TOTAL NUMBER OF FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

# Create a data frame with the provided data
data6 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(77325,1065226,1181175,2781930,20817,338067,705046,1039074,515802,855679,709939,749280,1427704),
  Work_Employment = c(3300,49034,31436,87160,696,17611,20358,28293,11740,31553,26574,61959,64494),
  Business = c(1033,16194,17529,28159,346,2678,6024,12062,8887,20146,9378,7501,13117),
  Education = c(1717,9536,8065,28571,955,2995,7498,6819,7388,9615,10327,8570,13957),
  Marriage = c(34132,595689,807462,1593334,10914,218966,484692,749383,290862,494686,401665,349056,896081)
)
print(data6)
# Selecting numeric columns for clustering
clustering_data <- data6[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF  FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS")




# TOTAL NUMBER OF RURAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

# Create a data frame with the provided data
data7 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(28858,462152,597815,909362,13449,204982,370005,644808,126514,412644,306013,278096,730304),
 Work_Employment = c(1175,19636,12957,23563,304,8084,5018,13322,1856,9696,7900,27892,30637),
  Business = c(463,6070,6250,9551,183,1186,2119,6965,1975,11184,3243,2116,6028),
  Education = c(409,2619,2373,5290,536,951,1894,2553,832,2857,2999,1894,3412),
  Marriage = c(16935,309684,491978,693499,8319,155936,308845,516302,91937,286086,212493,147350,551785)
)

# Selecting numeric columns for clustering
clustering_data <- data7[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF RURAL FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS")



# TOTAL NUMBER OF URBAN FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS					

# Create a data frame with the provided data
data8 <- data.frame(
  State = c("Jammu Kashmir", "Maharashtra", "Rajhasthan",
            "Uttar Pradesh", "Arunachal Pradesh", "Odisha", "West Bengal",
            "Madhya Pradesh", "Gujarat", "Karnataka", "Andhra Pradesh",
            "Tamil Nadu", "Bihar"),
  Total_Migrants = c(48467,603074,583360,1872568,7368,133085,335041,394266,389288,443035,403926,471184,697400),
  Work_Employment = c(2125,29398,18479,63597,392,9527,15340,14971,9884,21857,18674,34067,33857),
  Business = c(570,10124,11279,18608,163,1492,3905,5097,6912,8962,6135,5385,7089),
  Education = c(1308,6917,5692,23281,419,2044,5604,4266,6556,6758,7328,6676,10545),
  Marriage = c(17197,286005,315484,899835,2595,63030,175847,233081,198925,208600,189172,201706,344296)
)

# Selecting numeric columns for clustering
clustering_data <- data8[, c("Total_Migrants", "Work_Employment", "Business", "Education", "Marriage")]

# Hierarchical clustering using Ward's method
hc <- hclust(dist(clustering_data), method = "ward.D")

# Plotting the dendrogram
plot(hc, hang = -1, main = "DENDROGRAM OF STATES FOR TOTAL NUMBER OF URBAN FEMALE MIGRANTS IN INDIA FOR DIFFERENT REASONS IN 1991 CENSUS")



