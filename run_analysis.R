xts <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "" )
xtr <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "" )
yts <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
ytr <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
cols <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/features.txt", header = FALSE, sep = "")
actlbl <- read.table("/users/ayman/Downloads/R/UCI HAR Dataset/activity_labels.txt", header= FALSE, sep= "")


colslist <- as.list(cols$V2)
library(dplyr)
library(tidyr)
head(colslist)


xall <- bind_rows(xtr, xts)
yall <- bind_rows(ytr, yts)
xyall <- bind_cols(xall, yall)
extract <- function(x){x[[1]]}
colslistv <- extract(colslist)
colslistvall <- sapply(colslist, extract)
colslistvall
colnames(xyall) <- colslistvall
names(xyall)[562] <- "label"
xylbl <- merge(xyall, actlbl, by.x = "label", by.y = "V1", all = TRUE)
names(xylbl)[563] <- "activity"

table(grepl("[Mm]ean|std", names(xylbl)))
morstd <- grep("[Mm]ean|std|label|activity", names(xylbl), value = TRUE)
xyfinal <- xylbl[,morstd]
xygroup <- group_by(xyfinal, activity)
head(xygroup)
xytidy <- summarize_all(xygroup, funs(mean))
head(xytidy)
