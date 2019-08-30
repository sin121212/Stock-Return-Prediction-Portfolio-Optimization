library("caret")
library("PortfolioAnalytics")
library("quantmod")
library("LICORS")
library("IntroCompFinR")
library("ggplot2")
library("reshape")

######### load data #########
load("C:/Users/user/Desktop/cityu/Sem2/MS6217 Statistical Modelling in Economics and Finance/Final_Project/MS6217_project.RData")

set.seed(2019)
ctrl <- trainControl(method = "repeatedcv", number=5, repeats = 2)
tlength = 16

# FT <- 8 Fama-French factor from 197401 to 201712 (528 months)
# MKTX <- Market excess return 
# R <- 49 stock data from 197401 to 199712 (288 months)
# RF <- Risk-free from 197401 to 201712 (528 months)
# XT <- 10 Goyel-Welch type predictors to predict S&P500 return
# ZT_df <- 16 stock charactersitics of 49 stock 197312 - 201711

ZT_df <- data.frame(ZT)

#1. Cross Sectional Prediction with Characteristics
# Create vector to store MSE of 8 ML method
Stock_Name <- vector()

Pls_MSE <- vector()
Pcr_MSE <- vector()
Seq_MSE <- vector()
Forward_MSE <- vector()
Backward_MSE <- vector()
Ridge_MSE <- vector()
Lasso_MSE <- vector()
Enet_MSE <- vector()

# Forecast excess return of stock i at time t by using charactic at time t-1
for (Stock_Number in 1:49){
  # Compute excess return of stock i
  # Only have stock return 197401-199712
  Excess_Return <- R[,Stock_Number] - RF[1:288,1]
  Stock_Char <- vector()
  Z <- 1
  for (c in seq(Stock_Number,784,by=49)){
    # Find column that contain stock i charactersitics
    Stock_Char[Z] <- c
    Z <- Z+1
  }
  # Store the 16 charactersitic of stock i
  Stock_Char_df <- data.frame("Excess_Return"=Excess_Return,ZT_df[1:288,c(Stock_Char)])
  
  # Drop row with NA
  Stock_Char_df <- Stock_Char_df[complete.cases(Stock_Char_df), ]
  
  # Store the stock name
  Stock_Name[Stock_Number] <- Stock_Number
  
  # pls
  plsFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "pls",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Pls_MSE[Stock_Number] <- min(plsFit$results$RMSE)^2
  
  ######### PCA #########
  pcrFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "pcr",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Pcr_MSE[Stock_Number] <-  min(pcrFit$results$RMSE)^2
  
  ######### Sequential (Best Subset) #########
  seqFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "leapSeq",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Seq_MSE[Stock_Number] <-  min(seqFit$results$RMSE)^2
  
  ######### Forward #########
  forwardFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "leapForward",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Forward_MSE[Stock_Number] <-  min(forwardFit$results$RMSE)^2
  
  ######### Backward #########
  backwardFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "leapBackward",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Backward_MSE[Stock_Number] <-  min(backwardFit$results$RMSE)^2
  
  ######### Ridge #########
  # u can specify the grid to search for a good lambda
  ridgeFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "ridge",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Ridge_MSE[Stock_Number] <-  min(ridgeFit$results$RMSE)^2
  
  ######### LASSO #########
  lassoFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "lasso",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)

  Lasso_MSE[Stock_Number] <-  min(lassoFit$results$RMSE)^2
    
  ######### Elastic net #########
  enetFit <- train(
    Excess_Return ~ . ,
    data = Stock_Char_df,
    method = "enet",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  Enet_MSE[Stock_Number] <-  min(enetFit$results$RMSE)^2
  
}

ML_Char_df <- data.frame("Stock_Name"=Stock_Name,
                         "Pls_MSE"=Pls_MSE,
                         "Pcr_MSE"=Pcr_MSE,
                         "Seq_MSE"=Seq_MSE,
                         "Forward_MSE"=Forward_MSE,
                         "Backward_MSE"=Backward_MSE,
                         "Ridge_MSE"=Ridge_MSE,
                         "Lasso_MSE"=Lasso_MSE,
                         "Enet_MSE"=Enet_MSE)
# check NA
#which(is.na(Stock_Char_df$X36.mktcap_Mean))

#2. Cross Sectional Prediction with Factors
# FF3,FF5,FF8

# Create vector to store FF result
Stock_Name <- vector()

FF3_MSE <- vector()
FF5_MSE <- vector()
FF8_MSE <- vector()

for (Stock_Number in 1:49){
  # Compute excess return of stock i
  # Only have stock return 197401-199712
  Excess_Return <- R[,Stock_Number] - RF[1:288,1]
  
  # Create a dataframe for run lm
  Stock_FF_df <- data.frame("Excess_Return"=Excess_Return,FT[1:288,])
  
  # Drop row with NA
  Stock_FF_df <- Stock_FF_df[complete.cases(Stock_FF_df), ]
  
  # Store the stock name
  Stock_Name[Stock_Number] <- Stock_Number
  
  # FF3
  FF3_Fit <- train(
    Excess_Return ~ . ,
    data = Stock_FF_df[,1:4],
    method = "lm",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  FF3_MSE[Stock_Number] <-  min(FF3_Fit$results$RMSE)^2
  
  # FF5
  FF5_Fit <- train(
    Excess_Return ~ . ,
    data = Stock_FF_df[,1:6],
    method = "lm",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  FF5_MSE[Stock_Number] <-  min(FF5_Fit$results$RMSE)^2
  
  # FF8
  FF8_Fit <- train(
    Excess_Return ~ . ,
    data = Stock_FF_df[,1:9],
    method = "lm",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)
  
  FF8_MSE[Stock_Number] <-  min(FF8_Fit$results$RMSE)^2
} 

FF_df <- data.frame("Stock_Name"=Stock_Name,
                    "FF3_MSE"=FF3_MSE,
                    "FF5_MSE"=FF5_MSE,
                    "FF8_MSE"=FF8_MSE)

# Model sectetion by MSE
Model_MSE <- merge(x = ML_Char_df, y = FF_df, by = "Stock_Name", all = TRUE)

Model_Sum_MSE <- apply(Model_MSE[,2:12],2,sum)
Model_Sum_MSE


# 8ML
ML_Char_df_re <- melt(ML_Char_df, id = c("Stock_Name"))
p<-ggplot(ML_Char_df_re, aes(x=Stock_Name, y=value,fill=variable)) +
  geom_bar(stat="identity")+theme_minimal()

p + ggtitle("Algorithm Performance of 49 Stocks in-sample Excess Return") +
  xlab("Stock Number 1 to Stock Number 49") + ylab("Algorithm MSE")

# 3FF
FF_df_re <- melt(FF_df, id = c("Stock_Name"))
p<-ggplot(FF_df_re, aes(x=Stock_Name, y=value,fill=variable)) +
  geom_bar(stat="identity")+theme_minimal()

p + ggtitle("Fama-Franch models Performance of 49 Stocks in-sample Excess Return") +
  xlab("Stock Number 1 to Stock Number 49") + ylab("Fama-Franch models MSE")

# 8ML+3FF
# Reshape
Model_MSE_re <- melt(Model_MSE, id = c("Stock_Name"))

p<-ggplot(Model_MSE_re, aes(x=Stock_Name, y=value,fill=variable)) +
  geom_bar(stat="identity")+theme_minimal()

p + ggtitle("Algorithm Performance of 49 Stocks in-sample Excess Return") +
  xlab("Stock Number 1 to Stock Number 49") + ylab("Algorithm MSE")

# overall model
#barchart(Model_Sum_MSE,main="Model Comparison")
Model_Sum_MSE_df <- as.data.frame(Model_Sum_MSE)

Model_Sum_MSE_df2 <- cbind(rownames(Model_Sum_MSE_df), data.frame(Model_Sum_MSE_df, row.names=NULL))

p<-ggplot(Model_Sum_MSE_df2, aes(x=rownames(Model_Sum_MSE_df), y=Model_Sum_MSE,fill=rownames(Model_Sum_MSE_df))) +
  geom_bar(stat="identity")+theme_minimal()

p + ggtitle("Model Performance of Overall Stocks in-sample Excess Return") +
  xlab("Stock Number 1 to Stock Number 49") + ylab("MSE")

min(Model_Sum_MSE)

# Predict return by FF3 model

# Create New dataframe to store predicted excess return
#Predict_Return_df <- data.frame(num=rep(NA, 240),stringsAsFactors=FALSE)
Predict_Excess_Return_df <- data.frame("Date"=DATE[289:528],stringsAsFactors=FALSE)

for (Stock_Number in 1:49){
  # Compute excess return of stock i
  # Only have stock return 197401-199712
  Excess_Return <- R[,Stock_Number] - RF[1:288,1]
  
  # Create a dataframe for run lm
  Stock_FF_df <- data.frame("Excess_Return"=Excess_Return,FT[1:288,])
  
  # Drop row with NA
  Stock_FF_df <- Stock_FF_df[complete.cases(Stock_FF_df), ]
  
  # Store the stock name
  Stock_Name[Stock_Number] <- Stock_Number
  FF3_Fit <- train(
    Excess_Return ~ . ,
    data = Stock_FF_df[,1:4],
    method = "lm",
    preProc = c("center", "scale"),
    tuneLength = tlength,
    trControl = ctrl)

  Stock_Name <- toString(Stock_Number)
  
  Pre_Re <- predict(FF3_Fit,newdata = FT[289:528,])
  #Create new column
  Predict_Excess_Return_df[,Stock_Name] = Pre_Re
}

Predict_Excess_Return_re_df <- Predict_Excess_Return_df
for (i in 2:50){
  Predict_Excess_Return_re_df[,i] <- cumsum(log(1+Predict_Excess_Return_re_df[,i]))
}

# Excess Return
Predict_Excess_Return_gp1_df <- melt(Predict_Excess_Return_re_df, id = c("Date"))

p<-ggplot(Predict_Excess_Return_gp1_df, aes(x=Date, y=value, group=variable)) +
  geom_line(aes(color=variable))+
  geom_point(aes(color=variable))

p + ggtitle("Fama-French 3 Factors of 49 Stocks out-of-sample Cumulative Excess Return") +
  xlab("Date: From 199801 to 201712") + ylab("Cumulative Excess Return")


# Predicted stock return (not excess)
Predict_Return_df <- Predict_Excess_Return_df
# Stock return = stock excess return + risk-free return
Predict_Return_df[2:50] = Predict_Return_df[,2:50]+RF[289:528,1]

Predict_Return_re_df <- Predict_Return_df
for (i in 2:50){
  Predict_Return_re_df[,i] <- cumsum(log(1+Predict_Return_df[,i]))
}
Predict_Return_re_df <- melt(Predict_Return_re_df, id = c("Date"))

p<-ggplot(Predict_Return_re_df, aes(x=Date, y=value, group=variable)) +
  geom_line(aes(color=variable))+
  geom_point(aes(color=variable))

p + ggtitle("Fama-French 3 Factors of 49 Stocks out-of-sample Cumulative Return") +
  xlab("Date: From 199801 to 201712") + ylab("Cumulative Return")


# Index
rownames(Predict_Return_df) <- Predict_Return_df$Date
Predict_Return_df$Date <- NULL

# Merge df (past return + predicted return)
All_Period_R <- rbind(R,Predict_Return_df)

########## Generate random number matrix
W = abs(matrix(rnorm(4900000),1000000,49))
# Normalized random number as stock weighting
Normed_Random_No_mat = normalize(W, byrow = TRUE, tol = 1e-06)
# Column name
W_col <- vector()
for (n in 1:49){
  W_col[n] <- paste0("W_",n)
}

###### S&P 500
getSymbols("^GSPC", src = "yahoo",from = as.Date("1974-01-01"), to = as.Date("2017-12-31"),return.class='data.frame')

SP500_Return <- as.data.frame(periodReturn(xts(GSPC[["GSPC.Close"]], as.Date(rownames(GSPC))), 'monthly'))

SP500_plot <- SP500_Return$monthly.returns[289:528]

# plot
plot(c(289:528),cumsum(log(1+SP500_plot)),type='l', col='red',
     ylim=c(-1,2.5), xlab = "time index", ylab='return',
     main="S&P500 cumulative return")


MKTX_New <- MKTX[289:528]

