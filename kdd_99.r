# install packages
# install.packages("ggplot2")




# import libraries
library(ggplot2)





# Start writing to an output file
#sink('cor.txt')

# Clear the workspace
rm(list = ls())

# Load the data 
train_raw = read.csv("dataset/kddcup.data_10_percent_corrected.csv", stringsAsFactors = FALSE)

# Process the data
colnames = read.table("dataset/kddcup.names.txt", skip = 1, sep = ":")
names(train_raw) = colnames$V1
d = dim(train_raw)
names(train_raw)[d[2]] = "label"

# Observe the data
names(train_raw)





# Observe the distribution of labels
sum_label <- aggregate(rep(1, d[1]), by = list(train_raw$label), FUN = sum)
names(sum_label) <- c("label", "count")
barplot(beside = TRUE, log10(sum_label$count), 
        names.arg = sum_label$label, ylim = c(0,6),
        xlab = "Label", ylab = "log(Count)",
        col = "Blue", main = "The distribution of labels")




# convert symbolic values to continuous values
train_raw$protocol_type = factor(train_raw$protocol_type)
#train_raw$protocol_type = as.numeric(factor(train_raw$protocol_type))
train_raw$service = factor(train_raw$service)
train_raw$flag = factor(train_raw$flag)
#train_raw$service = as.numeric(factor(train_raw$service))
#train_raw$flag = as.numeric(factor(train_raw$flag))
train_raw$land = as.numeric(factor(train_raw$land))
train_raw$logged_in = as.numeric(factor(train_raw$logged_in))
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
train_raw$label = factor(train_raw$label)
#train_raw$label = as.numeric(factor(train_raw$label))






#cor(train_raw)
# corelation between label vs all other predictors
#cor(train_raw$label, train_raw)





# Exploratory Analysis

# Observation: dst_host_same_src_port_rate has slight effect on the intrusion type.
# for "dst_host_same_src_port_rate" value greater than equal to 1 it can be "probe" and "r2l""
#qplot(dst_host_same_src_port_rate, dst_host_srv_diff_host_rate, colour=label, data=train_raw)



# Observation: "flag" is a strong predictor. for flag= "REG" and "S0" it is "dos"
#qplot(service, flag, colour=label, data=train_raw)


# Observation: For duration Greater than 30000 we can see it's 'probe'
# Therefore duration itself is a strong predictor
#qplot(duration, src_bytes, colour=label, data=train_raw) 



# Observation: protocol-type "tcp" has "DOS" intrusion type. It is also a strong predictor of "dos" type.
#qplot(service, protocol_type, colour=label, data=train_raw)



# Observation: No such clear identification
#qplot(flag, land, colour=label, data=train_raw)



# Observation: For serror_rate and srv_serror_rate=0 or 1 its "dos" and
# serror_rate between 0.25 to 0.5 its "probe""
qplot(serror_rate, srv_serror_rate, colour=label, data=train_raw)



# Observation:For duration Greater than 30000 we can see it's 'probe'
#qplot(duration, src_bytes, colour=label, data=train_raw)



A=table(train_raw$flag,train_raw$label)
round(prop.table(A)*100,1)








#sink()
