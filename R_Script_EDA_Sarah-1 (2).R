#' Author: Sarah Kaczmarek
#' Date: Mar 25, 2023

##################################################################################
#Libraries
library(powerjoin)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(maps)

##################################################################################

# List all the files paths of interest from your local machine
# Chance the path parameter to your own folder!
allFiles <- list.files(path = 'C:/Users/Sarah/Desktop/Hult/ST23/R/Hult_Visualizing-Analyzing-Data-with-R/BAN1_Case_Info/A1_Household_Direct_Mail',
                       pattern = '*.csv',
                       full.names = T)

# Read in a list, list apply a function to an object ie read.csv to the file paths
allDF <- lapply(allFiles, read.csv)

# Using data.table rbindlist put all the data together
households <- power_left_join(allDF, by = "tmpID")

# Since the "external" data has additional households not part of the internal BBY loyalty program, you can use complete.cases() to obtain the best records for EDA analysis
bbyLoyalty <- households[complete.cases(households),]

##################################################################################

#finding NA values per column
colSums(is.na(bbyLoyalty))

# finding empty values
colSums(bbyLoyalty == "")
##################################################################################
#Combining Columns
# combining Horse, Dog, Cat and other to pet owner column
# combining all donations column into one

# select all pet columns for the new column
colsP <- c( "HorseOwner",
           "DogOwner",
           "CatOwner",
           "OtherPetOwner")
colsP
bbyLoyalty[,colsP]
# create a new column that combines the selected columns
bbyLoyalty$Pet <- ifelse(rowSums(bbyLoyalty[,colsP] == "Yes") > 0, "Yes", "no")

head(bbyLoyalty$Pet)
#counting 
df_counts_p<- count(bbyLoyalty,Pet)
df_counts_p
ggplot(data = df_counts_p, aes(x = Pet, y = n)) + 
  geom_bar(stat = "identity")

# now delete the individual pet colums from the df

bbyLoyalty[,colsP]<- NULL

##################################################################################
#Combing Columns
# combining all donations column into one

# select all donation columns for the new column
cols <- c( "DonatesEnvironmentCauseInHome", 
           "DonatesToCharityInHome",
           "DonatestoAnimalWelfare",
           "DonatestoArtsandCulture",
           "DonatestoChildrensCauses",
           "DonatestoHealthcare",
           "DonatestoInternationalAidCauses",
           "DonatestoVeteransCauses",
           "DonatestoWildlifePreservation",
           "DonatestoLocalCommunity")
cols
bbyLoyalty[,cols]

# create a new column that combines the selected columns
bbyLoyalty$donations <- ifelse(rowSums(bbyLoyalty[,cols] == "Yes") > 0, "Yes", "no")

# now delete the individual donation colums from the df

bbyLoyalty[,cols]<- NULL

##################################################################################
#delete columns that are obviously irrelevant: 

del <- c("FirstName",
         "LastName",
         "TelephonesFullPhone",
         "city")

bbyLoyalty[,del]<- NULL
##################################################################################
#delete duplicated columns

dup <- c("DonatestoHealthcare1",
         "DonatestoInternationalAidCauses1")

bbyLoyalty[,dup]<- NULL

##################################################################################
# delete columns with more than 5000 empties
# some of the columns with many missing values are yes/ no, and contain only yes
# therefore the missing can be interpreted as no

# finding empty values
colSums(bbyLoyalty == "")


#checking for unique values if a colums contains no
unique(bbyLoyalty$Investor)
unique(bbyLoyalty$HomeOffice)
unique(bbyLoyalty$BuyerofArtinHousehold)
unique(bbyLoyalty$BooksAudioReadinginHousehold)
unique(bbyLoyalty$LikelyUnionMember)
unique(bbyLoyalty$DonatestoConservativeCauses)
unique(bbyLoyalty$BusinessOwner)
unique(bbyLoyalty$GeneralCollectorinHousehold)
unique(bbyLoyalty$InterestinCurrentAffairsPoliticsInHousehold)
unique(bbyLoyalty$DonatestoLiberalCauses)

# All these variables only contain Yes, therefore empty could be no


#checking for unique values if a colums contains no
unique(bbyLoyalty$UpscaleBuyerInHome)
unique(bbyLoyalty$ReligiousContributorInHome)
unique(bbyLoyalty$FemaleOrientedMagazineInHome)
unique(bbyLoyalty$GardeningMagazineInHome)
unique(bbyLoyalty$HealthFitnessMagazineInHome)
unique(bbyLoyalty$FinancialMagazineInHome)
unique(bbyLoyalty$BookBuyerInHome)
unique(bbyLoyalty$PoliticalContributerInHome)
unique(bbyLoyalty$FamilyMagazineInHome)
unique(bbyLoyalty$ReligiousMagazineInHome)
unique(bbyLoyalty$CulinaryInterestMagazineInHome)
unique(bbyLoyalty$DoItYourselfMagazineInHome)

# These variables contain numbers starting at 1  
# empty can be interpreted as 0  --keep

#delete: 
HomePurchasePrice
unique(bbyLoyalty$supportsGayMarriage) #contains support and oppose, 9000 empty - delete
unique(bbyLoyalty$supportsTaxesRaise) #contains support and oppose 9000 empty - delete
unique(bbyLoyalty$MosaicZ4) # contains many different types, 7718 empty - delete
unique(bbyLoyalty$BuyerofAntiquesinHousehold) # all empty
unique(bbyLoyalty$supportsAffordableCareAct) #contains support and oppose 9000 empty - delete
unique(bbyLoyalty$supportsGunControl) #contains support and oppose 9000 empty - delete
unique(bbyLoyalty$overallsocialviews) #contains conservative and liberal 9000 empty - delete

delete <- c("HomePurchasePrice",
            "supportsGayMarriage",
            "supportsTaxesRaise",
            "MosaicZ4",
            "BuyerofAntiquesinHousehold",
            "supportsAffordableCareAct",
            "supportsGunControl",
            "overallsocialviews",
            "Education")

bbyLoyalty[,delete]<- NULL


##################################################################################
# You can save a copy of the joined and cleaned data with this.  It will save to your local working directory.
write.csv(bbyLoyalty, '2bbyLoyalty.csv', row.names = F)

str(bbyLoyalty)
head(bbyLoyalty)

##################################################################################
# visualizing single columns to get an initial overview

#ResindeceGenderDescription
df_counts <- count(bbyLoyalty, ResidenceHHGenderDescription)
df_counts

ggplot(data = df_counts, aes(x = ResidenceHHGenderDescription, y = n)) + 
  geom_bar(stat = "identity")
  

# Count and visualize kids
df_counts_kids <- count(na.omit(bbyLoyalty$PresenceOfChildrenCode))
ggplot(data = df_counts_kids, aes(x = PresenceOfChildrenCode, y = n)) + 
  geom_bar(stat = "identity")


# visualizing Median Education Years
df_counts_edu <- count(bbyLoyalty,MedianEducationYears)
df_counts_edu
ggplot(data = df_counts_edu, aes(x = MedianEducationYears, y = n)) + 
  geom_bar(stat = "identity")

# visualizing netWorth
# Calculate average net worth


# Visualize net worth
df_counts_nw <- count(bbyLoyalty, NetWorth)
ggplot(data = df_counts_nw, aes(x = NetWorth, y = n)) + 
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  labs(title = "Distribution of Net Worth", x = "Net Worth", y = "Count") +
  theme_minimal()

# visualising political parties

df_counts_p <- count(bbyLoyalty, PartiesDescription)
ggplot(data = df_counts_p, aes(x = PartiesDescription, y = n)) + 
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())


# visualizing religions
df_counts_r<- count(bbyLoyalty,ReligionsDescription)
df_counts_r
ggplot(data = df_counts_r, aes(x = ReligionsDescription, y = n)) + 
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


# visualizing HomeOwnerRenter
df_counts_H<- count(bbyLoyalty,HomeOwnerRenter)
df_counts_H
ggplot(data = df_counts_H, aes(x = HomeOwnerRenter, y = n)) + 
  geom_bar(stat = "identity",fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())


# visualizing states
df_counts_H<- count(bbyLoyalty,state)
df_counts_H
ggplot(data = df_counts_H, aes(x = state, y = n)) + 
  geom_bar(stat = "identity",fill = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#states on a map

# Count number of customers by state
df_counts_H <- count(bbyLoyalty, state)

# Load map data frame
states_map <- map_data("state")

# Join customer counts to map data frame
states_map_counts <- merge(states_map, df_counts_H, by.x = "region", by.y = "state")

# Plot map with color indicating customer counts
ggplot(states_map_counts, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_map() +
  labs(title = "Number of Customers by State") +
  theme_void()

# visualizing property type
df_counts_p<- count(bbyLoyalty,PropertyType)
df_counts_p
ggplot(data = df_counts_p, aes(x = PropertyType, y = n)) + 
  geom_bar(stat = "identity")

#Store visit Frequency
df_counts_s<- count(bbyLoyalty,storeVisitFrequency)
df_counts_s
ggplot(data = df_counts_s, aes(x = storeVisitFrequency, y = n)) + 
  geom_bar(stat = "identity",fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())

#Gender
df_counts_g<- count(bbyLoyalty,Gender)
df_counts_g
ggplot(data = df_counts_g, aes(x = Gender, y = n)) + 
  geom_bar(stat = "identity",fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())

#donations

head(bbyLoyalty$donations)
#counting 
df_counts_d<- count(bbyLoyalty,donations)
df_counts_d
ggplot(data = df_counts_d, aes(x = donations, y = n)) + 
  geom_bar(stat = "identity",fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())

##################################################################################

# creating bins for household spending
# define the custom breakpoints for the 'household spending' column
breakpoints <- c(-Inf, 50.000, 100.000, 150.000, 200.000, 250.000, 300.000, 350.000,
                 400.000, 450.000, 500.000, Inf)

# create bins using the 'cut' function
bbyLoyalty$spending_bin <- cut(bbyLoyalty$y_householdSpend, breaks = breakpoints)

# count the number of values in each bin using the 'table' function
bin_counts <- as.data.frame(table(bbyLoyalty$spending_bin))

# rename the columns in the 'bin_counts' data frame
names(bin_counts) <- c("bin", "count")

# plot the distribution of spending bins
ggplot(bin_counts, aes(x = bin, y = count)) +
  geom_bar(stat = "identity") +
  xlab("Spending Bins") +
  ylab("Count") +
  ggtitle("Spending Distribution")

##################################################################################

# creating bins for age
# define the custom breakpoints for the 'household spending' column
breakpoints <- c(-Inf,30,40,50,60,70,80,90,Inf)

# create bins using the 'cut' function
bbyLoyalty$age_bin <- cut(bbyLoyalty$Age, breaks = breakpoints)

# count the number of values in each bin using the 'table' function
bin_counts <- as.data.frame(table(bbyLoyalty$age_bin))

# rename the columns in the 'bin_counts' data frame
names(bin_counts) <- c("bin", "count")

# plot the distribution of spending bins
ggplot(bin_counts, aes(x = bin, y = count)) +
  geom_bar(stat = "identity") +
  xlab("Age Bins") +
  ylab("Count") +
  ggtitle("Age Distribution")

# mean age
mean_age <- mean(bbyLoyalty$Age)
mean_age


##################################################################################
# creating a new column that combines all donations collumns

# create a data frame containing only the columns of interest for donations to different causes
df_donations <- bbyLoyalty[, c("DonatesEnvironmentCauseInHome", "DonatesToCharityInHome",
                               "DonatestoAnimalWelfare", "DonatestoArtsandCulture",
                               "DonatestoChildrensCauses", "DonatestoHealthcare",
                               "DonatestoInternationalAidCauses", "DonatestoVeteransCauses",
                               "DonatestoWildlifePreservation", "DonatestoLocalCommunity")]

# count the number of "yes" in each column
yes_count <- sapply(df_donations, function(x) sum(x == "Yes"))
df_bar <- data.frame(column = names(df_donations), count = yes_count)

# create the bar chart
ggplot(df_bar, aes(x = column, y = count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Cause", y = "Count of Yes") + 
  ggtitle("Count of Yes per Cause") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################################################################
# spending by store visit frequency

avgSpend_by_frequency <- bbyLoyalty %>%
  group_by(storeVisitFrequency) %>%
  summarize(avgSpend = mean(y_householdSpend))
overall_avgSpend <- mean(bbyLoyalty$y_householdSpend)

ggplot(avgSpend_by_frequency, aes(x = storeVisitFrequency, y = avgSpend)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = overall_avgSpend, color = "red", linetype = "dashed") +
  labs(x = "Store Visit Frequency", y = "Average Spend", title = "Average Spend by Store Visit Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank())

  
##################################################################################
# spending by age group

avgSpend_by_age <- bbyLoyalty %>%
  group_by(age_bin) %>%
  summarise(avgSpend = mean(y_householdSpend))

ggplot(avgSpend_by_age, aes(x = age_bin, y = avgSpend)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Age Group", y = "Average Spending")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##################################################################################

#spending by age non grouped 

# Create the scatterplot with trend line
ggplot(bbyLoyalty, aes(x = Age, y = y_householdSpend)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Age") +
  ylab("y_householdSpend") +
  ggtitle("Relationship between age and y_householdSpend")

##################################################################################
# spending by state

avgSpend_by_state <- bbyLoyalty %>%
  group_by(state) %>%
  summarise(avgSpend = mean(y_householdSpend))

ggplot(avgSpend_by_state, aes(x = state, y = avgSpend)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "State", y = "Average Spending") +
  geom_hline(yintercept = mean(avgSpend_by_state$avgSpend), color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##################################################################################
# spending by religion

avgSpend_by_religion <- bbyLoyalty %>%
  group_by(ReligionsDescription) %>%
  summarise(avgSpend = mean(y_householdSpend))

ggplot(avgSpend_by_religion, aes(x = ReligionsDescription, y = avgSpend)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "State", y = "Average Spending") +
  geom_hline(yintercept = mean(avgSpend_by_state$avgSpend), color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##################################################################################
# cleaning the  magazines columns so that empties are replaced with 0

'FamilyMagazineInHome
FemaleOrientedMagazineInHome	
ReligiousMagazineInHome	
GardeningMagazineInHome	
CulinaryInterestMagazineInHome	
HealthFitnessMagazineInHome	
DoItYourselfMagazineInHome	
FinancialMagazineInHome'

# replacing the empty values with 0

# Specify the columns to replace empty values with 0
cols_to_replace <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome", "ReligiousMagazineInHome",
                     "GardeningMagazineInHome",	
                     "CulinaryInterestMagazineInHome",
                     "HealthFitnessMagazineInHome",	
                     "DoItYourselfMagazineInHome",
                     "FinancialMagazineInHome")

# Replace empty values with 0 for the specified columns
bbyLoyalty[cols_to_replace] <- lapply(bbyLoyalty[cols_to_replace], function(x) ifelse(x == "", 0, x))



# Select the columns to clean
cols_to_clean <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome", "ReligiousMagazineInHome",
                   "GardeningMagazineInHome", "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome",
                   "DoItYourselfMagazineInHome", "FinancialMagazineInHome")

# Clean the selected columns
bbyLoyalty <- bbyLoyalty %>%
  mutate(across(all_of(cols_to_clean), ~gsub("[^0-9]", "", .)))

unique(bbyLoyalty$FamilyMagazineInHome)

##################################################################################

# average age per store visit frequency

overall_mean_age <- mean(bbyLoyalty$Age, na.rm = TRUE)

# Create the scatterplot with average age per store visit frequency and overall average age
ggplot(bbyLoyalty, aes(x = storeVisitFrequency, y = Age)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  geom_hline(yintercept = overall_mean_age, linetype = "dashed", color = "blue") +
  xlab("Store Visit Frequency") +
  ylab("Average Age") +
  ggtitle("Average Age per Store Visit Frequency and Overall Average Age")

##################################################################################

# creating bins for age
# define the custom breakpoints for the 'household spending' column
breakpoints <- c(-Inf,30,40,50,60,70,80,90,Inf)

age_labels <- c("0-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90+")

# Create age bin variable with custom labels
bbyLoyalty$age_bin <- cut(bbyLoyalty$Age, 
                          breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, Inf), 
                          right = FALSE, 
                          labels = age_labels)

bin_counts
##################################################################################
# creating an age pyramid
# Summarize data by gender and age bin
age_gender <- bbyLoyalty %>% 
  group_by(Gender, age_bin) %>% 
  summarize(count = n()) %>% 
  ungroup()

age_gender <- subset(age_gender, Gender %in% c("M", "F"))

ggplot(data = age_gender, aes(x = age_bin, y = ifelse(Gender == "M", -count, count), fill = Gender)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Female", "Male")) +
  geom_text(aes(label = count), hjust = ifelse(age_gender$Gender == "Male", 1, 0), 
            size = 3, fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(limits = c(-max(age_gender$count), max(age_gender$count)), 
                     breaks = seq(-3000, 3000, by = 100), labels = abs) +
  scale_x_discrete(breaks = c("0-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90+")) +
  theme_minimal() +
  labs(title = "Population Pyramid by Age and Gender", 
       x = "Age Group", y = "Count") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

##################################################################################
#investigating the largest demographic group, which is female, 90+

subset_data <- subset(bbyLoyalty, Gender == "F" & age_bin == "90+")

ggplot(data = subset_data, aes(x = spending_bin)) +
    geom_bar(fill = "#2c7bb6") +
    stat_count(aes(label=..count..), geom="text", size=3, vjust=-0.5) +
    labs(title = "Spending Bins for Females aged 90+", x = "Spending Bins", y = "Count") +
    theme_minimal() +
    theme(panel.grid = element_blank())
  

##################################################################################
#presentation of females 90 + in the spending bins

# Subset data for females 90+
subset_data <- bbyLoyalty %>%
  filter(Gender == "F", age_bin == "90+") %>%
  group_by(spending_bin) %>%
  summarise(count = n()) %>%
  mutate(Gender = "Female 90+")

# Overall data
ggdata <- bbyLoyalty %>%
  mutate(Gender = ifelse(Gender == "F", "Female", "Male")) %>%
  group_by(Gender, spending_bin) %>%
  summarise(count = n()) 

# Combine data for both plots
combined_data <- bind_rows(ggdata, subset_data) %>%
  group_by(spending_bin) %>%
  mutate(percent = count/sum(count)*100)

# Calculate percentages for each spending bin
combined_data <- combined_data %>%
  group_by(spending_bin) %>%
  mutate(percent = ifelse(Gender == "Female 90+", 
                          count/sum(count) * 100, 
                          NA)) 


# Create plot
ggplot(data = combined_data, aes(x = spending_bin, y = count, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#7CAE00"), 
                    labels = c("Female", "Male", "Female 90+")) +
  geom_text(data = subset_data, aes(label = paste0(round(count/sum(count)*100), "%"), y = count/2 + lag(count)/2)) +
  theme_minimal() +
  labs(title = "Spending by Gender and Age Group",
       x = "Spending Bin", y = "Count") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

##################################################################################

##################################################################################

# Calculate average number of magazines for the entire dataset
all_avg <- bbyLoyalty %>% 
  summarise(
    FamilyMagazineInHome_avg = mean(FamilyMagazineInHome, na.rm = TRUE),
    FemaleOrientedMagazineInHome_avg = mean(FemaleOrientedMagazineInHome, na.rm = TRUE),
    ReligiousMagazineInHome_avg = mean(ReligiousMagazineInHome, na.rm = TRUE),
    GardeningMagazineInHome_avg = mean(GardeningMagazineInHome, na.rm = TRUE),
    CulinaryInterestMagazineInHome_avg = mean(CulinaryInterestMagazineInHome, na.rm = TRUE),
    HealthFitnessMagazineInHome_avg = mean(HealthFitnessMagazineInHome, na.rm = TRUE),
    DoItYourselfMagazineInHome_avg = mean(DoItYourselfMagazineInHome, na.rm = TRUE),
    FinancialMagazineInHome_avg = mean(FinancialMagazineInHome, na.rm = TRUE)
  ) %>% 
  gather(key = "Magazine", value = "Avg_Number")
search()

# Create a bar chart of average number of magazines
ggplot(all_avg, aes(x = Magazine, y = Avg_Number)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Magazine", y = "Average Number", title = "Average Number of Magazines by Magazine Type and Age Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################################################################

# Calculate average number of magazines for women in the age group of 90+
women_90_avg <- bbyLoyalty %>% 
  filter(Gender == "F", age_bin == c("80-90","90+")) %>% 
  summarise(
    FamilyMagazineInHome_avg = mean(FamilyMagazineInHome, na.rm = TRUE),
    FemaleOrientedMagazineInHome_avg = mean(FemaleOrientedMagazineInHome, na.rm = TRUE),
    
    ReligiousMagazineInHome_avg = mean(ReligiousMagazineInHome, na.rm = TRUE),
    GardeningMagazineInHome_avg = mean(GardeningMagazineInHome, na.rm = TRUE),
    CulinaryInterestMagazineInHome_avg = mean(CulinaryInterestMagazineInHome, na.rm = TRUE),
    HealthFitnessMagazineInHome_avg = mean(HealthFitnessMagazineInHome, na.rm = TRUE),
    DoItYourselfMagazineInHome_avg = mean(DoItYourselfMagazineInHome, na.rm = TRUE),
    FinancialMagazineInHome_avg = mean(FinancialMagazineInHome, na.rm = TRUE)
  )

# Calculate average number of magazines for the entire dataset
all_avg <- bbyLoyalty %>% 
  summarise(
    FamilyMagazineInHome_avg = mean(FamilyMagazineInHome, na.rm = TRUE),
    FemaleOrientedMagazineInHome_avg = mean(FemaleOrientedMagazineInHome, na.rm = TRUE),
    ReligiousMagazineInHome_avg = mean(ReligiousMagazineInHome, na.rm = TRUE),
    GardeningMagazineInHome_avg = mean(GardeningMagazineInHome, na.rm = TRUE),
    CulinaryInterestMagazineInHome_avg = mean(CulinaryInterestMagazineInHome, na.rm = TRUE),
    HealthFitnessMagazineInHome_avg = mean(HealthFitnessMagazineInHome, na.rm = TRUE),
    DoItYourselfMagazineInHome_avg = mean(DoItYourselfMagazineInHome, na.rm = TRUE),
    FinancialMagazineInHome_avg = mean(FinancialMagazineInHome, na.rm = TRUE)
  ) %>% 
  mutate(age_bin = "90+")

# Combine the two datasets
combined_data <- bind_rows(women_90_avg, all_avg) #%>% 
 # pivot_longer(cols = age_bin, names_to = "Magazine", values_to = "Avg_Number")
#combined_data <- subset(combined_data,combined_data$Gender==F )

t(combined_data)

# no relevant insights old women do not have more magazines
##################################################################################

#Donation by age group

# Group data by age_bin and donations
age_donations <- bbyLoyalty %>% 
  group_by(age_bin, donations) %>% 
  summarise(count = n())

# Calculate the proportion of "Yes" and "No" responses for each age_bin
age_donations_prop <- age_donations %>% 
  group_by(age_bin) %>% 
  mutate(prop = count/sum(count)) %>% 
  ungroup()

# Create stacked bar chart
ggplot(age_donations_prop, aes(x = age_bin, y = prop, fill = donations)) + 
  geom_col() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), name = "Donations") +
  labs(title = "Proportion of Donations by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal()

#donation frequency is very similar over different age groups

##################################################################################

# Define the desired order for NetWorth
nw_order <-  c("$499999+", "$250000-499999", "$100000-249999", "$50000-99999", "$25000-49999", "$10000-24999", "$5000-9999", "$1-4999")

# Filter out empty NetWorth values
bbyLoyalty <- bbyLoyalty %>% filter(!is.na(NetWorth))

# Convert NetWorth to factor variable with the desired order
bbyLoyalty$NetWorth <- factor(bbyLoyalty$NetWorth, levels = nw_order)

# Group data by age_bin and NetWorth
age_networth <- bbyLoyalty %>%
  group_by(age_bin, NetWorth) %>%
  summarise(count = n())

# Create stacked bar chart
ggplot(age_networth, aes(x = age_bin, y = count, fill = NetWorth)) +
  geom_col() +
  scale_fill_manual(values =  c("#377eb8", "#4daf4a", "#e41a1c", "#ff7f00", "#984ea3", "#ffff33", "#a65628", "#f781bf"), 
                    name = "NetWorth") +
  labs(title = "Number of Customers by NetWorth Categories and Age Group", 
       x = "Age Group", y = "Number of Customers") +
  theme_minimal()

# hard to gain insights from this 
#maybe change it to show age composition by net worth group
##################################################################################

# Filter out empty NetWorth values
bbyLoyalty <- bbyLoyalty %>% filter(!is.na(NetWorth))

# Group data by NetWorth and age_bin
networth_age <- bbyLoyalty %>%
  group_by(NetWorth, age_bin) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate proportion of each age group within each NetWorth category
networth_age_prop <- networth_age %>%
  group_by(NetWorth) %>%
  mutate(prop = count/sum(count)) %>%
  ungroup()

# Create stacked bar chart
ggplot(networth_age_prop, aes(x = NetWorth, y = prop, fill = age_bin)) +
  geom_col() +
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a6cee3", "#1f78b4","#e41a1c"), name = "Age Group") +
  labs(title = "Age Composition by NetWorth Category", x = "NetWorth Category", y = "Proportion") +
  theme_minimal()


##################################################################################
# Arrange data by age_bin and NetWorth
df <- df %>% arrange(bbyLoyalty, age_bin, NetWorth)

# Create a new column that represents the cumulative sum of counts within each age_bin
df <- df %>% group_by(age_bin) %>%
  mutate(cum_count = cumsum(count))

# Create a new data frame with values of start and end y position of each segment
lines_df <- df %>% group_by(age_bin) %>% 
  mutate(start_y = lag(cum_count), end_y = cum_count) %>% 
  ungroup()

# Plot stacked bar chart with connecting lines
ggplot(df, aes(x = age_bin, y = count, fill = NetWorth)) +
  geom_bar(stat = "identity") +
  scale_fill_hue(name = "Net Worth") +
  geom_segment(data = lines_df, aes(x = age_bin, xend = age_bin, y = start_y, yend = end_y),
               color = "black", size = 1.2, alpha = 0.5) +
  labs(title = "Age Composition per Net Worth Bin", x = "Age Group", y = "Count") +
  theme_minimal()

##################################################################################

# Get number of customers by state
df_counts_H <- aggregate(cbind(n = tmpID) ~ state, data = bbyLoyalty, FUN = length)

df_counts_H$state <- toupper(df_counts_H$state)
states_map_counts$region <- toupper(states_map_counts$region)
states_map_counts <- merge(states_map_counts, df_counts_H, by.x = "region", by.y = "state", all.x = TRUE)


# Join customer counts to map data
states_map_counts <- map_data("state")
states_map_counts$region <- toupper(states_map_counts$region)
states_map_counts <- merge(states_map_counts, df_counts_H, by.x = "region", by.y = "state", all.x = TRUE)

# Plot map with color indicating customer counts
ggplot(states_map_counts, aes(long, lat, group = group, fill = n)) +
  geom_polygon(color = "white") +  # Add white outlines
  geom_path(color = "black", alpha = 0.5) +  # Add black outlines with alpha transparency
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_quickmap() +
  labs(title = "Number of Customers by State") +
  theme_void()


##################################################################################
prot <- subset(bbyLoyalty, bbyLoyalty$ReligionsDescription == "Protestant")
ggplot(prot, aes(x=y_householdSpend, y= MedianEducationYears, color=Gender)) + geom_point()

##################################################################################

#stack pet by
plotDF <- data.frame(table(prot$ReligiousMagazineInHome,  prot$Pet))
plotDF <- subset(plotDF, plotDF$Var1 !='')

# Stacked
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")
# Filled
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity")

##################################################################################
# Define the desired order for NetWorth
nw_order <-  c("$499999+", "$250000-499999", "$100000-249999", "$50000-99999", "$25000-49999", "$10000-24999", "$5000-9999", "$1-4999")

# Filter out empty NetWorth values
bbyLoyalty <- bbyLoyalty %>% filter(!is.na(NetWorth))

# Convert NetWorth to factor variable with the desired order
bbyLoyalty$NetWorth <- factor(bbyLoyalty$NetWorth, levels = nw_order)

# Group data by age_bin and NetWorth
age_networth <- bbyLoyalty %>%
  group_by(age_bin, NetWorth) %>%
  summarise(count = n())

# Create stacked bar chart
ggplot(age_networth, aes(x = age_bin, y = count, fill = NetWorth)) +
  geom_col() +
  scale_fill_manual(values =  c("#377eb8", "#4daf4a", "#e41a1c", "#ff7f00", "#984ea3", "#ffff33", "#a65628", "#f781bf"), 
                    name = "NetWorth") +
  labs(title = "Number of Customers by NetWorth Categories and Age Group", 
       x = "Age Group", y = "Number of Customers") +
  theme_minimal()

# hard to gain insights from this 
#maybe change it to show age composition by net worth group



##################################################################################


# Group data by age_bin and NetWorth
age_networth <- bbyLoyalty %>%
  group_by(NetWorth,age_bin) %>%
  summarise(y_householdSpend = mean(y_householdSpend))

# Create stacked bar chart
ggplot(age_networth, aes(x = age_bin, y = y_householdSpend, fill = NetWorth)) +
  geom_col() +
  scale_fill_manual(values =  c("#377eb8", "#4daf4a", "#e41a1c", "#ff7f00", "#984ea3", "#ffff33", "#a65628", "#f781bf"), 
                    name = "NetWorth") +
  labs(title = "Number of Customers by NetWorth Categories and Age Group", 
       x = "Age Group", y = "Average Household Expenditure") +
  theme_minimal()
##################################################################################

#mexicans

# Create subset of bbyLoyalty where state is Coahuila or Tamaulipas
mexico_subset <- bbyLoyalty[bbyLoyalty$state %in% c("Coahuila", "Tamaulipas"), ]
dim(mexico_subset)

# the number of mexicans is too small

##################################################################################

# Subset the data for selected religions
selected_religions <- c("Catholic", "Jewish", "Protestant", "")
bbyLoyalty_subset <- bbyLoyalty %>% filter(ReligionsDescription %in% selected_religions)
bbyLoyalty_subset$ReligionsDescription[bbyLoyalty_subset$ReligionsDescription == ""] <- "None"

# Group data by religion and donations
donations_religion <- bbyLoyalty_subset %>%
  group_by(ReligionsDescription, donations) %>%
  summarise(n = n())

# Create stacked bar chart
ggplot(donations_religion, aes(x = ReligionsDescription, y = n, fill = donations)) +
  geom_col() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),
                    name = "Donations") +
  labs(title = "Number of Customers by Donations and Religion",
       x = "Religion", y = "Number of Customers") +
  theme_minimal()+
  theme(panel.grid = element_blank())

##################################################################################

# Subset the data for selected parties
selected_parties <- c("Democratic", "Republican", "Non-Partisan")
bbyLoyalty_subset <- bbyLoyalty %>% filter(PartiesDescription %in% selected_parties)

# Group data by party and age_bin
parties_age <- bbyLoyalty_subset %>%
  group_by(PartiesDescription, age_bin) %>%
  summarise(n = n())

# Create stacked bar chart
ggplot(parties_age, aes(x = age_bin, y = n, fill = PartiesDescription)) +
  geom_col() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#bdbdbd"),
                    name = "Parties") +
  labs(title = "Number of Customers by Parties and Age Group",
       x = "Age Group", y = "Number of Customers") +
  theme_minimal() +
  theme(panel.grid = element_blank())

