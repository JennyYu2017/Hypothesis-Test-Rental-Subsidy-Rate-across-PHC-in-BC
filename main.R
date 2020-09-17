library(ggplot2)
library(plyr)
library(psych)
library(gdata)
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(RColorBrewer)
library(dplyr)

### Task A

# laod data
data <- read.csv("./1_RSR.csv")
data <- data %>% select(HAUTH_NAME, CH_SA_NAME, HLT_A_NAME, ST_RTSB_RT,
                        SHLT_WN_MN, ST_RT30_RT)
## Problem 1

# calculate the mean and median of `ST_RTSB_RT` groupby `HAUTH_NAME`.
library(dplyr)
group_value <- data%>%
  group_by(HAUTH_NAME)%>% 
  summarise_at(vars(ST_RTSB_RT), list(~ mean(., trim = 0), ~ median(., na.rm = TRUE)))

# add lineplot into qplot
qplot(x = HAUTH_NAME, y = ST_RTSB_RT,
      color = HAUTH_NAME,
      data = data) + 
  geom_line(data = group_value, aes(x = HAUTH_NAME, y = mean, group=1,  size="Mean"), colour = "orange") + 
  geom_line(data = group_value, aes(x = HAUTH_NAME, y = median, group=1, size="Median"), colour = "purple") + 
  scale_color_hue("Group") +
  scale_size_manual("Statistical Info", values=rep(0.5,2),
                    guide=guide_legend(override.aes = list(colour=c("orange", "purple"))),
                    labels=c("Mean", "Median")) 


## kstest and t-test

columns <- group_value$HAUTH_NAME
  kstest.results <- matrix(1:25, nrow = 5, ncol = 5)
ttest.results <- matrix(1:25, nrow = 5, ncol = 5)

# perform the KStest for ST_RTSB_RT groupby HAUTH_NAME.
for (i in 1:5){
  v1 <- data[data$HAUTH_NAME == columns[i], "ST_RTSB_RT"]
  for (j in 1:5){
    if (i == j){
      kstest.results[i, i] <-""
      ttest.results[i, i] <-""
    }
    else{
      v2 <- data[data$HAUTH_NAME == columns[j], "ST_RTSB_RT"]
      result <- ks.test(v1, v2)
      record <- paste("P:", round(result$p.value, 2), "D:", round(result$statistic, 2))
      kstest.results[i, j] <- record
      kstest.results[j, i] <- record
      result <- t.test(v1, v2)
      record <- paste("P:", round(result$p.value, 2), "t:", round(result$statistic, 2))
      ttest.results[i, j] <- record
      ttest.results[j, i] <- record
    }
  }
}

cat("Two-sides KStest for ST_RTSB_RT")
prmatrix(kstest.results, row = paste(columns), coll=paste(columns))
cat("Two-sides Ttest for ST_RTSB_RT")
prmatrix(ttest.results, row = paste(columns), coll=paste(columns))

##Problem 2

# load data
income_data <- read.csv("./2_income.csv")

# join data
new_data <- merge(data, income_data)

filter_data <- new_data %>% select(HAUTH_NAME, CH_SA_NAME, HLT_A_NAME, ST_RTSB_RT, SHLT_WN_MN,
                                   ST_RT30_RT, H_VRG_T_NE, H_MDN_T_NE)

# analyze correlationship between columns vy plotting pairplot
pairs.panels(filter_data[, 4:8], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

group_value <- filter_data%>%
  group_by(HAUTH_NAME)%>% 
  summarise_at(vars(ST_RTSB_RT, SHLT_WN_MN, ST_RT30_RT, H_VRG_T_NE, H_MDN_T_NE),
               list(~ mean(., trim = 0), ~ median(., na.rm = TRUE)))

log_group_value <- group_value
log_group_value[, 2:11] <- lapply(group_value[, 2:11], log)


# plot mean
colors <- brewer.pal(n = 8, name = "Dark2")
ggplot() + 
  geom_line(data=log_group_value, aes(y = ST_RTSB_RT_mean, x=HAUTH_NAME, color = colors[1], group=1),  size=0.5) + 
  geom_line(data=log_group_value, aes(y = SHLT_WN_MN_mean, x=HAUTH_NAME, color = colors[2], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = ST_RT30_RT_mean, x=HAUTH_NAME, color = colors[3], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = H_VRG_T_NE_mean, x=HAUTH_NAME, color = colors[4], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = H_MDN_T_NE_mean, x=HAUTH_NAME, color = colors[5], group=1),  size=0.5) + 
  scale_color_discrete(name = "Columns", labels = c("ST_RTSB_RT", "SHLT_WN_MN", 
                                                    "ST_RT30_RT", "H_VRG_T_NE", "H_MDN_T_NE")) + 
  ylab("Mean (After logarithmic transformation)")

# plot median
colors <- brewer.pal(n = 8, name = "Dark2")
ggplot() + 
  geom_line(data=log_group_value, aes(y = ST_RTSB_RT_median, x=HAUTH_NAME, color = colors[1], group=1),  size=0.5) + 
  geom_line(data=log_group_value, aes(y = SHLT_WN_MN_median, x=HAUTH_NAME, color = colors[2], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = ST_RT30_RT_median, x=HAUTH_NAME, color = colors[3], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = H_VRG_T_NE_median, x=HAUTH_NAME, color = colors[4], group=1),  size=0.5) +
  geom_line(data=log_group_value, aes(y = H_MDN_T_NE_median, x=HAUTH_NAME, color = colors[5], group=1),  size=0.5) + 
  scale_color_discrete(name = "Columns", labels = c("ST_RTSB_RT", "SHLT_WN_MN", 
                                                    "ST_RT30_RT", "H_VRG_T_NE", "H_MDN_T_NE")) + 
  ylab("Median (After logarithmic transformation)")

## Problem 5
se <- function(x) sqrt(var(x)/(length(x)-1))


## Problem 6 

HDI_data <- read.xls("./HDI.xlsx", sheet = 1, header = TRUE)
HDI_data <- HDI_data[, 3:6]
HDI_data$Gross.national.income..GNI..per.capita <- 
  unlist(lapply(HDI_data$Gross.national.income..GNI..per.capita, function(x){
    return(as.numeric(gsub(",", "", x)))}))


# analyze correlationship between columns of training set
pairs.panels(HDI_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

## data normalization
HDI_data[, 2:4] <- scale(HDI_data[, 2:4])

## sample 90% of the original data as training data
set.seed(123)
smp_size <- floor(0.9 * nrow(HDI_data))
train_ind <- sample(seq_len(nrow(HDI_data)), size = smp_size)
train <- HDI_data[train_ind, ]
test <- HDI_data[-train_ind, ]


df.lm <- lm(formula = Human.development.index..HDI.. ~ ., data = train)
dt <- rpart(formula = Human.development.index..HDI.. ~ ., data= train,control = list(maxdepth = 4))
svm <- svm(Human.development.index..HDI.. ~ ., data = train, kernel="radial")
rf <- randomForest(formula = Human.development.index..HDI.. ~ ., data = train)

results <- matrix(0, nrow = 3, ncol = 4)
models <- list(df.lm, dt, svm, rf)

for (i in 1:4){
  y.hat <- unlist(predict(models[i], test))
  tmp1 <- (test$Human.development.index..HDI.. -  y.hat)^ 2
  results[1, i] <- round(mean(tmp1), 6)
  results[2, i] <- round(mean(abs(test$Human.development.index..HDI.. -  y.hat)), 6)  
  rss <- sum(tmp1)  ## residual sum of squares
  tss <- sum((y.hat - mean(y.hat)) ^ 2)  ## total sum of squares
  r.square <- 1 - rss/tss
  results[3, i] <- r.square
}

cat("Model Comparison")
prmatrix(results, row = c("MSE", "MAE", "R Square"), coll=c("Linear.Reg", "DecisonTree.reg", "SVM.reg", "RandomForest.reg"))


