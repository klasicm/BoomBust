#####
##### Policy Entrepreneurship Survey Setup


#packages
library(tidyr)
library(dplyr)

data_910 <- read.csv("PE_Sheets9-10.csv" ,header = TRUE, stringsAsFactors = FALSE)

View(data_910)

#Create comma list of emails
data_910$EmailsAll <- paste0(data_910$Email,",",data_910$Email2)

#Unnest emails to create new rows- all the same data except emails
data_910All <- data_910 %>%
  mutate(EmailsAll = strsplit(as.character(EmailsAll),",")) %>%
  unnest(EmailsAll)

#######Clean up database for Gwen to Email
colnames(data_910All)
#Replace N/A with blanks in ResponseID Column
data_910All$ResponseID <-""

#Remove Email1 and Email 2 columns
data_910All <- data_910All[,c(1:4,11,7:10)]

#Change name of Email column
names(data_910All)[names(data_910All) == "EmailsAll"] <- "Email"

#Remove NA from End.Date
data_910All$End.Date <- ""
dim(data_910All)

#Export to Show Gwen
write.csv(data_910All, "PESurveyEmailDistribution_Lists9and10.csv")

#Split back into file 9 and file 10
which(data_910All$LastName == "Kernan")
File9 <- data_910All[c(1:200),]
File10 <- data_910All[c(1,201:354),]

#Write files
write.csv(File9, "PE_9_UniqueLinks_TwoEmail.csv")
write.csv(File10,"PE_10_UniqueLinks_TwoEmail.csv")


############################################################################
############################################################################
############################################################################

data_11 <- read.csv("PE_11_UniqueLinks.csv" ,header = TRUE, stringsAsFactors = FALSE)
data_12 <- read.csv("PE_12_UniqueLinks.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(data_11);colnames(data_12)

#Create comma list of emails
data_11$EmailsAll <- paste0(data_11$Email,",",data_11$Email2,",",data_11$Email3)
data_12$EmailsAll <- paste0(data_12$Email,",",data_12$Email2,",",data_12$Email3)

#Unnest emails to create new rows- all the same data except emails
data_11All <- data_11 %>%
  mutate(EmailsAll = strsplit(as.character(EmailsAll),",")) %>%
  unnest(EmailsAll)

data_12All <- data_12 %>%
  mutate(EmailsAll = strsplit(as.character(EmailsAll),",")) %>%
  unnest(EmailsAll)

#######Clean up database for Gwen to Email
colnames(data_11All)
colnames(data_12All)

#Replace N/A with blanks in ResponseID Column
data_11All$ResponseID <-""
data_12All$Response.ID <- ""

#Remove Email1, Email 2, Email 3 columns
data_11All <- data_11All[,c(1:4,13,6:10)]
data_12All <- data_12All[,c(1:4,13,6:9)]

#Change name of Email column
names(data_11All)[names(data_11All) == "EmailsAll"] <- "Email"
names(data_12All)[names(data_12All) == "EmailsALl"] <- "Email"

#Remove NA from End.Date
data_11All$End.Date <- ""
data_12All$End.Date <- ""

dim(data_11All)
dim(data_12All)

#Export for Survey Emails
write.csv(data_11All, "PE_11_UniqueLinks_ThreeEmail.csv")
write.csv(data_12All,"PE_12_UniqueLinks_ThreeEmail.csv")



############################################################################
############################################################################
############################################################################

data1 <- read.csv("PE1.csv",header = TRUE,stringsAsFactors = FALSE)
data2 <- read.csv("PE2.csv",header = TRUE,stringsAsFactors = FALSE)
data3 <- read.csv("PE3.csv",header = TRUE,stringsAsFactors = FALSE)
data4 <- read.csv("PE4.csv",header = TRUE,stringsAsFactors = FALSE)
data5 <- read.csv("PE5.csv",header = TRUE,stringsAsFactors = FALSE)
data6 <- read.csv("PE6.csv",header = TRUE,stringsAsFactors = FALSE)
data7 <- read.csv("PE7.csv",header = TRUE,stringsAsFactors = FALSE)
data8 <- read.csv("PE8.csv",header = TRUE,stringsAsFactors = FALSE)
data9 <- read.csv("PE_9_UniqueLinks_TwoEmail.csv",header = TRUE,stringsAsFactors = FALSE)
data10 <- read.csv("PE_10_UniqueLinks_TwoEmail.csv",header = TRUE,stringsAsFactors = FALSE)
data11 <- read.csv("PE_11_UniqueLinks_ThreeEmail.csv",header = TRUE,stringsAsFactors = FALSE)
data12 <- read.csv("PE_12_UniqueLinks_ThreeEmail.csv",header = TRUE,stringsAsFactors = FALSE)

colnames(data1);colnames(data2);colnames(data3)
colnames(data4);colnames(data5);colnames(data6)
colnames(data7);colnames(data8);colnames(data9)
colnames(data10); colnames(data11); colnames(data12)

#Remove extra columns from Data11
data11 <- data11[,1:9]

#Change colnames in Data9-Data10 to match others
colnames(data9) <- c("Response.ID","Last.Name","First.Name","External.Data.Reference",
                     "Email","Status","End.Date","Link","Link.Expiration")
colnames(data10) <- c("Response.ID","Last.Name","First.Name","External.Data.Reference",
                     "Email","Status","End.Date","Link","Link.Expiration")
colnames(data12) <- c("Response.ID","Last.Name","First.Name","External.Data.Reference",
                      "Email","Status","End.Date","Link","Link.Expiration")

#Combine all files together
data_all <- do.call("rbind",list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12))

##Read in sheets with address, business, other data
singleemail <- read.csv("PE_SingleEmail_FullData.csv",header = TRUE,stringsAsFactors = FALSE)
twoemail <- read.csv("PE_TwoEmail_FullData.csv",header = TRUE,stringsAsFactors = FALSE)
threemail <- read.csv("PE_ThreeEmail_FullData.csv",header = TRUE,stringsAsFactors = FALSE)
colnames(singleemail);colnames(twoemail);colnames(threemail)

#Add extra column to twoemail to make same column numbers
twoemail$X <- ""

#Rename columms to match across files
colnames(singleemail) <- c("External.Data.Reference","Last.Name","First.Name","Title.1","Org.1","Title.2",
                           "Org.2","Former.Title","Former.Org","Website.1","Website.2","Email.1","Email.2",
                           "Email.3","Phone.Number.1","Phone.Number.2","Address.1","Address.2","Notes","Minor",
                           "Deceased","FoundOnline","Relevant","CheckedBy","DataLocation","Sector","SectorNotes","Other")
colnames(twoemail) <- c("External.Data.Reference","Last.Name","First.Name","Title.1","Org.1","Title.2",
                        "Org.2","Former.Title","Former.Org","Website.1","Website.2","Email.1","Email.2",
                        "Email.3","Phone.Number.1","Phone.Number.2","Address.1","Address.2","Notes","Minor",
                        "Deceased","FoundOnline","Relevant","CheckedBy","DataLocation","Sector","SectorNotes","Other")
colnames(threemail) <- c("External.Data.Reference","Last.Name","First.Name","Title.1","Org.1","Title.2",
                         "Org.2","Former.Title","Former.Org","Website.1","Website.2","Email.1","Email.2",
                         "Email.3","Phone.Number.1","Phone.Number.2","Address.1","Address.2","Notes","Minor",
                         "Deceased","FoundOnline","Relevant","CheckedBy","DataLocation","Sector","SectorNotes","Other")
#Combine sheets together
MetaData <- do.call("rbind",list(singleemail,twoemail,threemail))

##Merge survey links data with Metada
DataTracker <- merge(x=data_all,y=MetaData,by = "External.Data.Reference")

#Clean up Dataset
DataTracker <- DataTracker[,c(2,1,3:9,12:36)]

#Write DataTracker
write.csv(DataTracker,"PE_SurveyLinksMetaData_Tracker.csv")
