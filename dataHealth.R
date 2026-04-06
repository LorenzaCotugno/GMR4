# Dataset Health 

# importing the data-set: 

library(haven)
library(dplyr)
library(xtable)
library(gridExtra)

file_path <- "C:/Users/Lorenza Cotugno/Desktop/GMR4/ZA8794_v1-0-0.dta"


source("C:/Users/Lorenza Cotugno/Desktop/GMR4/gmr4.R") 
source("C:/Users/Lorenza Cotugno/Desktop/GMR4/predict.R")
source("C:/Users/Lorenza Cotugno/Desktop/GMR4/gmr4.start.R")
source("C:/Users/Lorenza Cotugno/Desktop/GMR4/xval.gmr4.R")
source("C:/Users/Lorenza Cotugno/Desktop/GMR4/xval.start.R") 


data <- read_dta(file_path)
colnames(data)
sum(is.na(data))
dim(data)  # 73450 obs x 110 

# For now we filter for the Netherlands 
data <- data %>% 
  filter(country == 528)

# And we filter for data after the Covid (2021)
data <- data %>%
  filter(year == 2021)

dim(data) # 1269 x 110 obs

# Some variables are deleted because it is clear that they are the same or not interesting
data <- data[, -c(1:5, 8:9, 51, 53, 83:110)]

# Saving some negative values that are interesting:

data$PARTY_LR <- case_when(
  data$PARTY_LR %in% 1:5  ~ data$PARTY_LR,  
  data$PARTY_LR == -4     ~ 0,          
  TRUE                    ~ NA_real_        
)


data$TYPORG1 <- case_when(
  data$TYPORG1  == 1 ~ 1,
  data$TYPORG1  == 2 ~ 2,
  data$TYPORG1  == -4 ~ 0,  
  TRUE  ~ NA_real_
)


data$TYPORG2 <- case_when(
  data$TYPORG2 == 1 ~ 1,
  data$TYPORG2 == 2 ~ 2,
  data$TYPORG2 == -4     ~ 0,  
  TRUE ~ NA_real_
)


# Some columns are not informative because they have too many NA values
data[data < 0] <- NA
missing_values <- colSums(is.na(data))
missing_percent <- (missing_values / nrow(data)) * 100
missing_percent

data = data[, -c(33, 47, 50, 57, 65)] # All the columns with more than 40% null values
colnames(data) 


# Response Variables:

# v1 (ordinal)
colnames(data)[which(colnames(data) == "v1")] <- "happiness" 

# v4 (ordinal)
colnames(data)[which(colnames(data) == "v4")] <- "unnecessary use" 

# v28 (ordinal)
colnames(data)[which(colnames(data) == "v28")] <- "best treatment"  

# v29 (ordinal)
colnames(data)[which(colnames(data) == "v29")] <- "satisfaction healthcare"  

# v30 (ordinal)
colnames(data)[which(colnames(data) == "v30")] <- "satisfaction last visit"  

# v36 (ordinal)
colnames(data)[which(colnames(data) == "v36")] <- "self rated health"  



# Predictors: 
# 1. sex (binary)
colnames(data)[which(colnames(data) == "SEX")] <- "sex"

# 2. age (numeric) 
colnames(data)[which(colnames(data) == "AGE")] <- "age"

# 3.degree (ordinal)
colnames(data)[which(colnames(data) == "DEGREE")] <- "degree"

# 4. marital (ordinal)
colnames(data)[which(colnames(data) == "MARITAL")] <- "marital"

# 5. partliv (cat. nominal)
colnames(data)[which(colnames(data) == "PARTLIV")] <- "partliv"

# 6. hompop (numerical)
colnames(data)[which(colnames(data) == "HOMPOP")] <- "hompop"

# 7. hhchildr (numerical)
colnames(data)[which(colnames(data) == "HHCHILDR")] <- "hhchildr"

# 8. work (ordinal)
colnames(data)[which(colnames(data) == "WORK")] <- "work"

# 9. mainstat (ordinal)
colnames(data)[which(colnames(data) == "MAINSTAT")] <- "mainstat"

# 10. typorg1 (cat. nominal)
colnames(data)[which(colnames(data) == "TYPORG1")] <- "typorg1"

# 11. typorg2 (cat. nominal)
colnames(data)[which(colnames(data) == "TYPORG2")] <- "typorg2"

# 12. v18 (ordinal)
colnames(data)[which(colnames(data) == "v18")] <- "daily limitation" 

# 13. v22 (ordinal)
colnames(data)[which(colnames(data) == "v22")] <- "feeling overwhelmed" 

# 14. v23  (ordinal)
colnames(data)[which(colnames(data) == "v23")] <- "nvisits" 

# 15. v32 (ordinal)
colnames(data)[which(colnames(data) == "v32")] <- "smoking"

# 16. v33 (ordinal)
colnames(data)[which(colnames(data) == "v33")] <- "alcohol"

# 17. v34 (ordinal)
colnames(data)[which(colnames(data) == "v34")] <- "physical activity" 

# 18. v35 (ordinal)
colnames(data)[which(colnames(data) == "v35")] <- "diet"

# 19. v38, v39 (BMI) 
data$bmi <- data$v39 / (data$v38 / 100)^2

# 20- income (ordinal)
colnames(data)[which(colnames(data) == "INCOME")] <- "income" 

# 21. vote_le (cat. nominal)
colnames(data)[which(colnames(data) == "VOTE_LE")] <- "vote le"

# 22. party_lr (ordinal)
colnames(data)[which(colnames(data) == "PARTY_LR")] <- "partylr"

# 23. top_bot (ordinal)
colnames(data)[which(colnames(data) == "TOPBOT")] <- "topbot"

# 24. religgrp (cat. nominal)
colnames(data)[which(colnames(data) == "RELIGGRP")] <- "religgrp" 

# 25. attend (ordinal)
colnames(data)[which(colnames(data) == "ATTEND")] <- "attend"

# 26. union membership (ordinal)
colnames(data)[which(colnames(data) == "UNION")] <- "union"

# 27. urbrural (ordinal)
colnames(data)[which(colnames(data) == "URBRURAL")] <- "urbrural"



data = data[, c(1, 3, 6, 20, 24, 25, 30:37, 42:45, 48, 49, 51, 52, 58:69)]
data = na.omit(data)
dim(data) # 791 x 34


#------------------- 


# Ordering ordinal predictors

vals <- sort(unique(data$attend[!is.na(data$attend)]))
data$attend <- myrecode(data$attend, old = vals, new = rev(vals))

vals <- sort(unique(data$urbrural[!is.na(data$urbrural)]))
data$urbrural <- myrecode(data$urbrural, old = vals, new = rev(vals))

vals <- sort(unique(data$work[!is.na(data$work)]))
data$work <- myrecode(data$work, old = vals, new = rev(vals))

vals <- sort(unique(data$union[!is.na(data$union)]))
data$union <- myrecode(data$union, old = vals, new = rev(vals))

vals <- sort(unique(data$marital[!is.na(data$marital)]))
data$marital <- myrecode(data$marital, old = vals, new = rev(vals))

vals <- sort(unique(data$partliv[!is.na(data$partliv)]))
data$partliv <- myrecode(data$partliv, old = vals, new = rev(vals))


responses = data[, c(2, 3, 7, 8, 9, 14)]
Y = as.matrix(responses)

predictors = data[, c(4:6, 10:13, 15:34)]
X = as.matrix(predictors) 

Yb =  NULL
Yn = NULL
Yo =  Y

# Deleting predictors too unbalanced
X = X[, -c(20, 24)]

# Binarization
X[, 8] = X[, 8] - 1



YYo <- matrix(NA, nrow = nrow(Yo), ncol = ncol(Yo))

for (r in 1:ncol(Yo)) {
  vals <- sort(unique(Yo[, r][!is.na(Yo[, r])]))
  YYo[, r] <- myrecode(Yo[, r], old = vals, new = rev(vals))
}

colnames(YYo) <- colnames(Yo)


Xscale = c(
  "O",  # daily limitation          → ordinal (v18)
  "O",  # feeling overwhelmed       → ordinal (v22)
  "O",  # nvisits                   → ordinal (v23)
  "O",  # smoking                   → ordinal (v32)
  "O",  # alcohol                   → ordinal (v33)
  "O",  # physical activity         → ordinal (v34)
  "O",  # diet                      → ordinal (v35)
  "C",  # sex                       → C. binary 
  "N",  # age                       → numeric continuous
  "O",  # degree                    → ordinal (education level)
  "O",  # work                      → ordinal 
  "C",  # typorg1                   → cat ( options: 1 = for-profit, 2 = non-profit, 0 = not work) 
  "C",  # typorg2                   → cat (1 = public, 2 = private, 0 = not work) 
  "C",  # mainstat                  → cat nominal (principal occupation)
  "C",  # partliv                   → cat nominal (partner sì/no/in casa)
  "O",  # union                     → ordinal (current/former/never)
  "N",  # hompop                    → numeric (number people)
  "N",  # hhchildr                  → numeric (number sons)
  "C",  # marital                   → cat nominal
  "O",  # party lr                  → ordinal 0–10
  "C",  # religgrp                  → cat nominal
  "O",  # attend                    → ordinal (religious practice) 
  "O",  # income                    → ordinal (income)
  "O",  # urbrural                  → ordinal (urban → rural) 
  "N"   # bmi                       → numeric
)



# Fitting
out = gmr4( Yn = NULL, Yb = NULL, Yo = YYo, X = X, Xscale = Xscale)

# Validation 
PE.S1 = xval.start(Yn = NULL, Yb = NULL, Yo = YYo , X = X, Xscale, S = 1, K = 10, repeats = 1, lambda.lasso.seq = seq(0, 200, by = 0.1), lambda.ridge.seq = 0, lambda.glasso.seq = 0)
PE.S2 = xval.start(Yn = NULL, Yb = NULL, Yo = YYo , X = X, Xscale, S = 2, K = 10, repeats = 1, lambda.lasso.seq = seq(0, 200, by = 0.1), lambda.ridge.seq = 0, lambda.glasso.seq = 0)
PE.S3 = xval.start(Yn = NULL, Yb = NULL, Yo = YYo , X = X, Xscale, S = 3, K = 10, repeats = 1, lambda.lasso.seq = seq(0, 200, by = 0.1), lambda.ridge.seq = 0, lambda.glasso.seq = 0)
PE.S4 = xval.start(Yn = NULL, Yb = NULL, Yo = YYo , X = X, Xscale, S = 4, K = 10, repeats = 1, lambda.lasso.seq = seq(0, 200, by = 0.1), lambda.ridge.seq = 0, lambda.glasso.seq = 0)

# Plot of the validation S1
summary_valS1 = summary.function(PE.S1, "lasso", 10) 
lambda_S1 = find_lambda(summary_valS1)
# plotS1 <- plotxval(summary_valS1, s_label = "S = 1", show_y = TRUE)

# Plot of the validation S2
summary_valS2 = summary.function(PE.S2, "lasso", 10) 
lambda_S2 = find_lambda(summary_valS2)
# plotS2 <- plotxval(summary_valS2, s_label = "S = 2", show_y = TRUE)

# Plot of the validation S3
summary_valS3 = summary.function(PE.S3, "lasso", 10) 
lambda_S3 = find_lambda(summary_valS3)
# plotS3 <- plotxval(summary_valS3, s_label = "S = 3", show_y = FALSE)

# Plot of the validation S4
summary_valS4 = summary.function(PE.S4, "lasso", 10) 
lambda_S4 = find_lambda(summary_valS4)
# plotS4 <- plotxval(summary_valS4, s_label = "S = 4", show_y = FALSE)


# For selecting the minimum lambda 

minAPE_S1 <- min(as.numeric(summary_valS1$APE), na.rm = TRUE)
minAPE_S2 <- min(as.numeric(summary_valS2$APE), na.rm = TRUE)
minAPE_S3 <- min(as.numeric(summary_valS3$APE), na.rm = TRUE)
minAPE_S4 <- min(as.numeric(summary_valS4$APE), na.rm = TRUE)

print(paste("Min APE S = 1:", minAPE_S1))
print(paste("Min APE S = 2:", minAPE_S2))
print(paste("Min APE S = 3:", minAPE_S3))
print(paste("Min APE S = 4:", minAPE_S4))

minAPE_values <- c(S1 = minAPE_S1, S2 = minAPE_S2, S3 = minAPE_S3, S4 = minAPE_S4)

optimal_S <- names(minAPE_values)[which.min(minAPE_values)]
print(paste("Optimal Rank:", optimal_S))

lambda_optimal <- switch(optimal_S,
                         S1 = lambda_S1,
                         S2 = lambda_S2,
                         S3 = lambda_S3,
                         S4 = lambda_S4)
print(paste("Optimal λ :", lambda_optimal))


# Lambda 1standard error
APE_S2 <- as.numeric(summary_valS2$APE)
SEPE_S2 <- as.numeric(summary_valS2$SEPE)
lambdas_S2 <- summary_valS2$lambda
threshold_S2 <- min(APE_S2, na.rm = TRUE) + SEPE_S2[which.min(APE_S2)]
lambda1se_S2 <- max(lambdas_S2[APE_S2 <= threshold_S2])

# Lambda 2standard error
threshold2_S2 <- min(APE_S2, na.rm = TRUE) + 2 * SEPE_S2[which.min(APE_S2)]
lambda2se_S2 <- max(lambdas_S2[APE_S2 <= threshold2_S2])

# Lambda 3standard error
threshold3_S2 <- min(APE_S2, na.rm = TRUE) + 3 * SEPE_S2[which.min(APE_S2)]
lambda3se_S2 <- max(lambdas_S2[APE_S2 <= threshold3_S2])



plotS1 <- plotxval(summary_valS1, s_label = "S = 1", show_y = TRUE)
plotS2 <- plotxval(summary_valS2, s_label = "S = 2", show_y = FALSE, lambda1se = lambda1se_S2, lambda2se = lambda2se_S2)
plotS3 <- plotxval(summary_valS3, s_label = "S = 3", show_y = FALSE)
plotS4 <- plotxval(summary_valS4, s_label = "S = 4", show_y = FALSE) 


# S1
# Lambda 1standard error 
threshold_S2
APE_S1 <- as.numeric(summary_valS1$APE)
lambdas_S1 <- summary_valS1$lambda
lambda1se_S1 <- max(lambdas_S1[APE_S1 <= threshold_S2])

threshold2_S2
lambda2se_S1 <- max(lambdas_S1[APE_S1 <= threshold2_S2])

threshold3_S2
lambda3se_S1 <- max(lambdas_S1[APE_S1 <= threshold3_S2])

#---

ylims <- c(7.1, 7.6)

plotS1 <- plotxval(summary_valS1, s_label = "S = 1", show_y = TRUE,
                   lambda3se = lambda3se_S1, ylim = ylims)

plotS2 <- plotxval(summary_valS2, s_label = "S = 2", show_y = FALSE,
                   lambda1se = lambda1se_S2, lambda2se = lambda2se_S2, ylim = ylims)

plotS3 <- plotxval(summary_valS3, s_label = "S = 3", show_y = FALSE, ylim = ylims)
plotS4 <- plotxval(summary_valS4, s_label = "S = 4", show_y = FALSE, ylim = ylims)




ggsave("plotxval.png", grid.arrange(plotS1, plotS2, plotS3, plotS4, ncol = 3), width = 12, height = 20)
ggsave("validation_plots.png", grid.arrange(plotS1, plotS2, plotS3, plotS4, ncol = 3), width = 12, height = 4)

g = grid.arrange(plotS1, plotS2, plotS3, plotS4, ncol = 4)
ggsave("plotxval.png", g, width = 30, height = 4)


outS1 = gmr4( Yn = NULL, Yb = NULL, Yo = YYo, X = X, S = 1, Xscale = Xscale, lambda.lasso = lambda3se_S1, lambda.ridge = 0, lambda.glasso = 0)
B = round(outS1$B,3) 
print(B)
V = round(outS1$V, 3) 
BV = round(B%*%t(V),2)
xtab <- xtable(BV)
xtabB = xtable(B)
xtabV = xtable(V)

m = outS2$m
mm = outS2$mm 
PHI = outS2$PHI



outS2 = gmr4( Yn = NULL, Yb = NULL, Yo = YYo, X = X, S = 2, Xscale = Xscale, lambda.lasso = lambda2se_S2, lambda.ridge = 0, lambda.glasso = 0)
B = round(outS2$B,3) 
print(B)
V = round(outS2$V, 3) 
BV = round(B%*%t(V),2)
xtab <- xtable(BV)
xtabB = xtable(B)
xtabV = xtable(V)

m = outS2$m
mm = outS2$mm 
PHI = outS2$PHI



nomi <- colnames(X)

nomi[1]  <- "DL"
nomi[2]  <- "FO"
nomi[3]  <- "NV"
nomi[6]  <- "PA"
nomi[21] <- "RG"
nomi[23] <- "IN"

colnames(X) <- nomi


# Optimal Scaling of the Predictors selected by Lasso

mylabs1 = c("Never", "Seldom", "Sometimes", "Often", "Very often")
mylabs2 = c("Never", "Seldom", "Sometimes", "Often", "Very often")
mylabs3 = c("Never", "Seldom", "Sometimes", "Often", "Very often")
mylabs6 <- c("Never", "≤1/month", "Several/month","Several/week","Daily")
mylabs21 <-  c("No religion", "Roman Catholic", "Protestant", "Christian Orthodox", "Other Christian", "Jewish", "Islam", "Buddhism", "Hinduism", "Other Eastern/Asian", "Other religion")
mylabs23 <- c("Low","Middle","High")


plt1 = plot.quantifications(outS2, var = 1, mylabs1)
plt2 = plot.quantifications(outS2, var = 2, mylabs2)
plt3 = plot.quantifications(outS2, var = 3, mylabs3)
plt6 = plot.quantifications(outS2, var = 6, mylabs6)
plt21 = plot.quantifications(outS2, var = 21, mylabs21)
plt24 = plot.quantifications(outS2, var = 23, mylabs23)



plt1 <- plt1 + theme(
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)


plt2 <- plt2 + theme(
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)


plt3 <- plt3 + theme(
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)

plt6 <- plt6 + theme(
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)


plt21 <- plt21 + theme(
  axis.text.x = element_text(size = 5),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)

plt24 <- plt24 + theme(
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  plot.title = element_text(size = 12)
)

g = grid.arrange(plt1, plt2, plt3, plt6, plt21, plt24, ncol = 3)
ggsave("plotquant.png", g, width = 40, height = 20, dpi = 300)


#---


common_ylim <- c(7.085, 7.6) 

plotS2 <- plotxval(
  summary_valS2, 
  s_label = "S = 2", 
  show_y = TRUE,
  lambda1se = lambda1se_S2, 
  lambda2se = lambda2se_S2,
  ylim = common_ylim
)
plotS3 <- plotxval(summary_valS3, s_label = "S = 3", show_y = FALSE, ylim = common_ylim)
plotS4 <- plotxval(summary_valS4, s_label = "S = 4", show_y = FALSE, ylim = common_ylim)


grid.arrange(plotS2, plotS3, plotS4, ncol = 3)


ggsave("confronto_modelli.png", plot = plotS2 + plotS3 + plotS4, width = 12, height = 4)

