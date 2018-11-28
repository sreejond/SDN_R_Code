#setwd("C:/Users/sreej/Desktop/SDN/SDN_R_Code")

# install packages
# install.packages("ggplot2")
# install.packages("Boruta")
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("NMF")
# install.packages("BiocManager")
# BiocManager::install("phyloseq", version = "3.8")




# import libraries
library(ggplot2)




# Start writing to an output file
#sink('cor.txt')

# Clear the workspace
rm(list = ls())

# Load training data 
train_raw = read.csv("dataset/kddcup.data_10_percent_corrected.csv", stringsAsFactors = FALSE)

# Process the data
colnames = read.table("dataset/kddcup.names.txt", skip = 1, sep = ":")
names(train_raw) = colnames$V1
d = dim(train_raw)
names(train_raw)[d[2]] = "label"

# Observe the data
names(train_raw)




# convert symbolic values to continuous values
#train_raw$protocol_type = factor(train_raw$protocol_type)
train_raw$protocol_type = as.numeric(factor(train_raw$protocol_type))
#train_raw$service = factor(train_raw$service)
#train_raw$flag = factor(train_raw$flag)
train_raw$service = as.numeric(factor(train_raw$service))
train_raw$flag = as.numeric(factor(train_raw$flag))
#train_raw$land = factor(train_raw$land)
train_raw$land = as.numeric(factor(train_raw$land))
#train_raw$logged_in = factor(train_raw$logged_in)
train_raw$logged_in = as.numeric(factor(train_raw$logged_in))
#train_raw$root_shell = factor(train_raw$root_shell)
#train_raw$su_attempted = factor(train_raw$su_attempted)
#train_raw$is_host_login = factor(train_raw$is_host_login)
#train_raw$is_guest_login = factor(train_raw$is_guest_login)
train_raw$is_host_login = as.numeric(factor(train_raw$is_host_login))
train_raw$is_guest_login = as.numeric(factor(train_raw$is_guest_login))





# label preprocess
# The result is classified into 4 groups 
# Subsetting the label variable into 4 groups.
train_raw$label[train_raw$label == "ipsweep."] = "probe"
train_raw$label[train_raw$label == "portsweep."] = "probe"
train_raw$label[train_raw$label == "nmap."] = "probe"
train_raw$label[train_raw$label == "satan."] = "probe"
train_raw$label[train_raw$label == "buffer_overflow."] = "u2r"
train_raw$label[train_raw$label == "loadmodule."] = "u2r"
train_raw$label[train_raw$label == "perl."] = "u2r"
train_raw$label[train_raw$label == "rootkit."] = "u2r"
train_raw$label[train_raw$label == "back."] = "dos"
train_raw$label[train_raw$label == "land."] = "dos"
train_raw$label[train_raw$label == "neptune."] = "dos"
train_raw$label[train_raw$label == "pod."] = "dos"
train_raw$label[train_raw$label == "smurf."] = "dos"
train_raw$label[train_raw$label == "teardrop."] = "dos"
train_raw$label[train_raw$label == "ftp_write."] = "r2l"
train_raw$label[train_raw$label == "guess_passwd."] = "r2l"
train_raw$label[train_raw$label == "imap."] = "r2l"
train_raw$label[train_raw$label == "multihop."] = "r2l"
train_raw$label[train_raw$label == "phf."] = "r2l"
train_raw$label[train_raw$label == "spy."] = "r2l"
train_raw$label[train_raw$label == "warezclient."] = "r2l"
train_raw$label[train_raw$label == "warezmaster."] = "r2l"
train_raw$label[train_raw$label == "normal."] = "normal"
train_raw$label = factor(train_raw$label, levels = c("normal", "dos", "probe", "r2l", "u2r"))
#train_raw$label = as.numeric(factor(train_raw$label))





normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_raw_scaled <- as.data.frame(lapply(train_raw[1:41], normalize))
train_raw_scaled$label <- train_raw$label

library(nnet)
# Encode as a one hot vector multilabel data
train_raw_encoded <- cbind(train_raw_scaled[, 1:41], class.ind(as.factor(train_raw_scaled$label)))
# Set labels name
names(train_raw_encoded) <- c(names(train_raw_scaled)[1:41], "normal", "dos", "probe", "r2l", "u2r")





# Load testing data 
test_raw = read.csv("dataset/corrected.csv", stringsAsFactors = FALSE)

# Process the data
colnames = read.table("dataset/kddcup.names.txt", skip = 1, sep = ":")
names(test_raw) = colnames$V1
d = dim(test_raw)
names(test_raw)[d[2]] = "label"

# Observe the data
names(test_raw)






# convert symbolic values to continuous values
#test_raw$protocol_type = factor(test_raw$protocol_type)
test_raw$protocol_type = as.numeric(factor(test_raw$protocol_type))
#test_raw$service = factor(test_raw$service)
#test_raw$flag = factor(test_raw$flag)
test_raw$service = as.numeric(factor(test_raw$service))
test_raw$flag = as.numeric(factor(test_raw$flag))
#test_raw$land = factor(test_raw$land)
test_raw$land = as.numeric(factor(test_raw$land))
#test_raw$logged_in = factor(test_raw$logged_in)
test_raw$logged_in = as.numeric(factor(test_raw$logged_in))
#test_raw$root_shell = factor(test_raw$root_shell)
#test_raw$su_attempted = factor(test_raw$su_attempted)
#test_raw$is_host_login = factor(test_raw$is_host_login)
#test_raw$is_guest_login = factor(test_raw$is_guest_login)
test_raw$is_host_login = as.numeric(factor(test_raw$is_host_login))
test_raw$is_guest_login = as.numeric(factor(test_raw$is_guest_login))





# label preprocess
# The result is classified into 4 groups 
# Subsetting the label variable into 4 groups.
test_raw$label[test_raw$label == "ipsweep."] = "probe"
test_raw$label[test_raw$label == "portsweep."] = "probe"
test_raw$label[test_raw$label == "nmap."] = "probe"
test_raw$label[test_raw$label == "satan."] = "probe"
test_raw$label[test_raw$label == "mscan."] = "probe"
test_raw$label[test_raw$label == "saint."] = "probe"
test_raw$label[test_raw$label == "buffer_overflow."] = "u2r"
test_raw$label[test_raw$label == "loadmodule."] = "u2r"
test_raw$label[test_raw$label == "perl."] = "u2r"
test_raw$label[test_raw$label == "rootkit."] = "u2r"
test_raw$label[test_raw$label == "httptunnel."] = "u2r"
test_raw$label[test_raw$label == "ps."] = "u2r"
test_raw$label[test_raw$label == "sqlattack."] = "u2r"
test_raw$label[test_raw$label == "xterm."] = "u2r"
test_raw$label[test_raw$label == "back."] = "dos"
test_raw$label[test_raw$label == "land."] = "dos"
test_raw$label[test_raw$label == "neptune."] = "dos"
test_raw$label[test_raw$label == "pod."] = "dos"
test_raw$label[test_raw$label == "smurf."] = "dos"
test_raw$label[test_raw$label == "teardrop."] = "dos"
test_raw$label[test_raw$label == "apache2."] = "dos"
test_raw$label[test_raw$label == "mailbomb."] = "dos"
test_raw$label[test_raw$label == "processtable."] = "dos"
test_raw$label[test_raw$label == "udpstorm."] = "dos"
test_raw$label[test_raw$label == "ftp_write."] = "r2l"
test_raw$label[test_raw$label == "guess_passwd."] = "r2l"
test_raw$label[test_raw$label == "imap."] = "r2l"
test_raw$label[test_raw$label == "multihop."] = "r2l"
test_raw$label[test_raw$label == "phf."] = "r2l"
test_raw$label[test_raw$label == "warezclient."] = "r2l"
test_raw$label[test_raw$label == "warezmaster."] = "r2l"
test_raw$label[test_raw$label == "sendmail."] = "r2l"
test_raw$label[test_raw$label == "named."] = "r2l"
test_raw$label[test_raw$label == "snmpgetattack."] = "r2l"
test_raw$label[test_raw$label == "snmpguess."] = "r2l"
test_raw$label[test_raw$label == "xlock."] = "r2l"
test_raw$label[test_raw$label == "xsnoop."] = "r2l"
test_raw$label[test_raw$label == "worm."] = "r2l"
test_raw$label[test_raw$label == "normal."] = "normal"
test_raw$label = factor(test_raw$label, levels = c("normal", "dos", "probe", "r2l", "u2r"))
#test_raw$label = as.numeric(factor(test_raw$label))




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
test_raw_scaled <- as.data.frame(lapply(test_raw[1:41], normalize))
test_raw_scaled$label <- test_raw$label

# Encode as a one hot vector multilabel data
test_raw_encoded <- cbind(test_raw_scaled[, 1:41], class.ind(as.factor(test_raw_scaled$label)))
# Set labels name
names(test_raw_encoded) <- c(names(test_raw_scaled)[1:41], "normal", "dos", "probe", "r2l", "u2r")




# Create final training data with the important features
sample_train = train_raw_encoded[sample(nrow(train_raw_encoded), replace=F, size=0.01*nrow(train_raw_encoded)), ]
sample_test = test_raw_encoded[sample(nrow(test_raw_encoded), replace=F, size=0.01*nrow(test_raw_encoded)), ]




# selected 19 features
# train_raw_imp_features <- sample_train[, c("flag", "dst_host_rerror_rate", "logged_in", "dst_bytes", 
#                                            "src_bytes", "num_compromised", "dst_host_srv_count", "duration", 
#                                            "dst_host_same_src_port_rate", "dst_host_diff_srv_rate", "dst_host_count", 
#                                            "dst_host_srv_serror_rate", "count", "hot", "dst_host_same_srv_rate", 
#                                            "dst_host_serror_rate", "protocol_type", "wrong_fragment", "srv_count",
#                                            "normal", "dos", "probe", "r2l", "u2r" )] 
# 
# test_raw_imp_features <- sample_test[, c("flag", "dst_host_rerror_rate", "logged_in", "dst_bytes", 
#                                          "src_bytes", "num_compromised", "dst_host_srv_count", "duration", 
#                                          "dst_host_same_src_port_rate", "dst_host_diff_srv_rate", "dst_host_count", 
#                                          "dst_host_srv_serror_rate", "count", "hot", "dst_host_same_srv_rate", 
#                                          "dst_host_serror_rate", "protocol_type", "wrong_fragment", "srv_count", 
#                                          "normal", "dos", "probe", "r2l", "u2r" )] 

train_raw_imp_features <- train_raw_encoded[, c("flag", "dst_host_rerror_rate", "logged_in", "dst_bytes", 
                                           "src_bytes", "num_compromised", "dst_host_srv_count", "duration", 
                                           "dst_host_same_src_port_rate", "dst_host_diff_srv_rate", "dst_host_count", 
                                           "dst_host_srv_serror_rate", "count", "hot", "dst_host_same_srv_rate", 
                                           "dst_host_serror_rate", "protocol_type", "wrong_fragment", "srv_count",
                                           "normal", "dos", "probe", "r2l", "u2r" )] 

test_raw_imp_features <- test_raw_encoded[, c("flag", "dst_host_rerror_rate", "logged_in", "dst_bytes", 
                                         "src_bytes", "num_compromised", "dst_host_srv_count", "duration", 
                                         "dst_host_same_src_port_rate", "dst_host_diff_srv_rate", "dst_host_count", 
                                         "dst_host_srv_serror_rate", "count", "hot", "dst_host_same_srv_rate", 
                                         "dst_host_serror_rate", "protocol_type", "wrong_fragment", "srv_count", 
                                         "normal", "dos", "probe", "r2l", "u2r" )] 

library("caret")
inTrain <- createDataPartition(y = paste(train_raw_imp_features$normal, train_raw_imp_features$dos, 
                                         train_raw_imp_features$probe, train_raw_imp_features$r2l, 
                                         train_raw_imp_features$u2r), p = 0.5, list = FALSE)
inTest <- createDataPartition(y = paste(test_raw_imp_features$normal, test_raw_imp_features$dos, 
                                        test_raw_imp_features$probe, test_raw_imp_features$r2l, 
                                        test_raw_imp_features$u2r), p = 0.5, list = FALSE)

# final_subset_train <- train_raw_imp_features[inTrain,]
# final_subset_test <- test_raw_imp_features[inTest,]
final_subset_train <- train_raw_imp_features
final_subset_test <- test_raw_imp_features
dim(final_subset_train)
dim(final_subset_test)





library(neuralnet)

n <- names(final_subset_train)
f <- as.formula(paste("normal + dos + probe + r2l + u2r ~", 
                      paste(n[!n %in% c("normal", "dos", "probe", "r2l", "u2r")], collapse = " + ")))
f

nnModel <- neuralnet(formula = f, data = final_subset_train, hidden=c(10,5,3,2), linear.output=FALSE, threshold=0.01)
saveRDS(object = nnModel, file = "nnModel_on_100_test_set.rds")
#rfModelFit = readRDS("rfModelFitFile_on_50_test_set.rds")
nnModel$result.matrix
plot(nnModel)






# Compute predictions
pr.nn <- compute(nnModel, final_subset_test[, 1:19])
pr.nn

results <- data.frame(pr.nn$net.result)
names(results) = c("normal", "dos", "probe", "r2l", "u2r")

roundedresults <- sapply(results, round, digits = 0)
roundedresultsdf = data.frame(roundedresults)

predictedResults = cbind(1:nrow(roundedresultsdf), max.col(roundedresultsdf))
predictedResults = predictedResults[,2]
actualResults = cbind(1:nrow(final_subset_train[, 20:24]), max.col(final_subset_train[, 20:24]))
actualResults = actualResults[,2]

confusion_matrix = table(predictedResults, actualResults)
confusion_matrix
round(prop.table(confusion_matrix, 1) * 100, 2)

