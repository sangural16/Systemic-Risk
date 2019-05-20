rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
setwd("/home/sachin/Downloads/Ugp")

libraries  = c("quantreg")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data       = read.csv("/home/sachin/Downloads/Ugp/51_banks_firms_returns_and_macro.csv", header = TRUE)

dt <- as.Date(data[,1], format = "%d-%m-%Y")


# read the macro state variables 
data_m     = as.matrix(data[, 53:58])

# read the log returns of 51 firms
data_y     = as.matrix(data[, 2:52])

# qantile level
tau        = 0.05

nncol      = ncol(data_y)
nnrow      = nrow(data_y)
lengthfull = nnrow

# window size is 50
winsize    = 50
VaR        = matrix(0, ncol = nncol, nrow = (lengthfull - winsize))

# start the moving window VaR prediction, store the predict values
for (j in 1:nncol) {
  for (i in 1:(lengthfull - winsize)) {
    ycut   = data_y[i:(i + winsize), j]
    xcut   = data_m[i:(i + winsize), ]
    xxcut  = matrix(0, nrow(xcut), ncol(xcut))
    # standardize macro state variables
    for (k in 1:ncol(xcut)) {
      xxcut[, k] = (xcut[, k] - min(xcut[, k]))/(max(xcut[, k]) - min(xcut[, k]))
    }
    fit       = rq(ycut ~ xxcut, tau)
    pre       = predict(fit, quantiles = tau)
    VaR[i, j] = pre[length(pre)]
  }
}
VaR         = round(VaR, digits = 9)
write.csv(VaR, file = "VaR_movingwindows.csv")

# extracting the VaR of PNB
VaR_ref <- subset(VaR, select = c(24))
write.csv(VaR_ref, file = "VaR_ref.csv")

# Removing ref bank from VaR data frame
VaR_withoutref <- VaR[,-24]

# Removing last window from the macro variables dataframe
data_Mnew <- data_m[1:(nnrow-50),]

# Combining the macro variables and VaR ref dataframes
Combined_data <-cbind(data_Mnew, VaR_ref)

# standardize only the macro variables
cbdcut  = Combined_data
for (k in 1:ncol(data_m)) {
    cbdcut[, k] = (cbdcut[, k] - min(cbdcut[, k]))/(max(cbdcut[, k]) - min(cbdcut[, k]))
  }

CV_q    = matrix(0, ncol = ncol(VaR_withoutref), nrow = (lengthfull - winsize))

# Calculating at the 0.05 quantile
for(l in 1:ncol(VaR_withoutref)){
  Var_temp   = VaR_withoutref[,l]
  CoVaR_qth  = rq(Var_temp ~ cbdcut, tau)
  CV_q[,l]   = predict(CoVaR_qth, quantiles = tau)
}

write.csv(CV_q, file = "CoVaR_qth.csv")

CV_median    = matrix(0, ncol = ncol(VaR_withoutref), nrow = (lengthfull - winsize))

# Calculating at the 0.5 quantile
for(m in 1:ncol(VaR_withoutref)){
  Var_temp2   = VaR_withoutref[,m]
  CoVaR_median  = rq(Var_temp2 ~ cbdcut, 0.5)
  CV_median[,m]   = predict(CoVaR_median, quantiles =0.5)
}

# Calculating delta CoVaR
del_CoVaR = CV_q-CV_median
write.csv(del_CoVaR, file = "delCoVaR.csv")
