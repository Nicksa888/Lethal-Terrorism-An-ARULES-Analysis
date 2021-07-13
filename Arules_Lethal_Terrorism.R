
# This code presents a rule based analysis of lethal terrorism since 1970. 

#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("arules",
            "arulesSequences",
            "arulesViz",
            "DataExplorer",
            "data.table", # for set.names function
            "dplyr",
            "psych", # for pairs panel function
            "data.table", # for set.names function
            "ggplot2",
            "RColorBrewer" # for colour palette options
  ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0919dist.csv")

########################
########################
#### Filtering Data ####
########################
########################

################################
# Filter Doubt Terrorism == No #
################################

# This removes all the non - terrorism violence as such incidents are coded yes
# Yes means it is doubted such violence is terrorism

GTDDT <- GTD %>% dplyr::filter(doubtterr == 0)
str(GTDDT)
dim(GTDDT)

###########################
# Filter Specificity == 1 #
###########################

# This removes all rows where the geographic coordinates have not been verified
# This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.

GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)

##################
# Rename Columns #
##################

library(data.table) # for set.names function

setnames(GTDDTS, old = c("iyear", "imonth", "provstate", "city",    "attacktype1_txt", "gname", "targtype1_txt", "weaptype1_txt", "nkill", "nwound"), 
         new = c("Year", "Month", "Province", "City", "Attack", "Group", "Target", "Weapon", "Dead", "Wounded"))

#################
#################
# Arules Analysis
#################
#################

####################
# Attack Based Rules 
####################

# As Weapon and attack types are correlated, weapon type is removed from the analysis

GTDDTS_FCS <- dplyr::select(GTDDTS, Province, City, Attack, Target, Group, Weapon, Dead)

##############################
# Rename Dead Column as Lethal
##############################

GTDDTS_FCS <- rename(GTDDTS_FCS, Lethal = Dead)

GTDDTS_FCS

###################################
# Remove NA values from each column
###################################

colSums(is.na(GTDDTS_FCS)) # count the NA's in each column

GTDDTS_FCS_NNA <- na.omit(GTDDTS_FCS) # remove NA's
colSums(is.na(GTDDTS_FCS_NNA))

write.csv(GTDDTS_FCS, file = "GTDDTS.csv", row.names = F)

###################
# Variable Counts #
###################

t <- GTDDTS_FCS_NNA %>%
  count(Lethal, sort = T)
View(t)

##############################################
# Convert Lethal Variable into binary format #
##############################################

GTDDTS_NNA <- GTDDTS_FCS_NNA %>% dplyr::mutate(Lethal = if_else(Lethal == 0, "0", "1"))
glimpse(GTDDTS_NNA)

##########################################
# Filter only lethal terrorism incidents #
##########################################

GTDDTS_NNA <- GTDDTS_NNA %>% dplyr::filter(Lethal == 1)

# filter of lethal  == 1, selects only data rows where lethal terrorism events are evident

############################
# Convert columns to factors
############################

GTDDTS_NNA$Province <- as.factor(GTDDTS_NNA$Province)
GTDDTS_NNA$City <- as.factor(GTDDTS_NNA$City)
GTDDTS_NNA$Attack <- as.factor(GTDDTS_NNA$Attack)
GTDDTS_NNA$Target <- as.factor(GTDDTS_NNA$Target)
GTDDTS_NNA$Group <- as.factor(GTDDTS_NNA$Group)
GTDDTS_NNA$Weapon <- as.factor(GTDDTS_NNA$Weapon)
GTDDTS_NNA$Lethal <- as.factor(GTDDTS_NNA$Lethal)

###################################
# Create transactions data format #
###################################

# The ARules algorithym requires data in transactional format

GTDDTS_FCS_T <- as(GTDDTS_NNA, "transactions") 

summary(GTDDTS_FCS_T)

image(GTDDTS_FCS_T)

########################
# Item Frequency Plots #
########################

# Relative Plot #

# Relative plots indicate the proportion that each item appears in the data set

itemFrequencyPlot(GTDDTS_FCS_T, topN = 30,  
                  cex.names = .5)

arules::itemFrequencyPlot(GTDDTS_FCS_T, 
                          topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

# Absolute #

# Absolute plot indicate the total count that each item appears in the data set

arules::itemFrequencyPlot(GTDDTS_FCS_T, 
                          topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Absolute Item Frequency Plot',
                          type = "absolute",
                          ylab = "Item Frequency (Absolute)")

# Explosives Weapon Relative

# Here, we select explosives weapons and plot the relative item frequency for such a weapon and lethal attacks. 
# The black distribution line indicates the proportion of items when all weapon types are included. 
# Inclusion of such a line enables direct comparison between the two distributions of item frequencies.

WE <- subset(GTDDTS_FCS_T, 
             items %in% "Weapon=Explosives")
itemFrequencyPlot(WE, 
                  topN = 20, 
                  col = brewer.pal(8, 'Pastel2'),
                  main = 'Relative Item Frequency Plot',
                  type = "relative",
                  ylab = "Item Frequency (Relative)",
                  population = GTDDTS_FCS_T, 
                  cex.names=.5)

# Explosives Weapon Absolute

WE <- subset(GTDDTS_FCS_T, 
             items %in% "Weapon=Explosives")
itemFrequencyPlot(WE, 
                  topN = 20, 
                  col = brewer.pal(8, 'Pastel2'),
                  main = 'Absolute Item Frequency Plot',
                  type = "absolute",
                  ylab = "Item Frequency (Absolute)",
                  population = GTDDTS_FCS_T, 
                  cex.names=.5)


# Apriori model on Lethal Variable

params <- list(support = 0.10, 
               confidence = 0.5, 
               minlen = 3)

rules <- apriori(GTDDTS_FCS_T, 
                 parameter = params,
                 appearance = list(default = "lhs", 
                                   rhs = "Lethal=1"))

# Remove redundant rules if any

rules <- rules[!is.redundant(rules)]
inspect(rules)
rules.sorted <- sort(rules, 
                     by = "support")

##################
# Visualisations #
##################

plot(rules, 
     engine = "ggplot2") +
     theme_classic()

plot(rules, 
     method = "two-key plot", 
     engine = "ggplot2") +
     theme_classic()

plot(rules, 
     method = "graph", 
     control = list(type = "items"))

plot(rules, 
     method = "grouped")

plot(rules, 
     method = "paracoord")



