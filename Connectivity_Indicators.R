# -----------------------------------------------------------#
# -----------         Indicators Scripts      ---------------#
# ---------         Analysis of Connectivity          -------#
# ---------          Measuring variability            -------#
# -----------------------------------------------------------#

# Written by: Morane Clavel-Henry morane@icm.csic.es
# Clean first Edition : 5 Oct 2023
# Hosting institute: ICM-CSIC
# R-Version 4.2.2

# *********** Packages, Functions ******** 
setwd("~/")
library(dplyr) # vs 1.1.0
library(reshape2) # vs 1.4.4
library(tibble) # vs 3.2.1
library(tidyr) # vs 1.3.0

'%nin%' = Negate('%in%')

### OCCURRENCE INDICATORS
# - Nlink
N_Link_func <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name,'Link')]) %>% 
          group_by(ID_Source,across(Time_name)) %>% 
          summarise_at(vars(Link),list(n=sum)) %>%
          group_by(ID_Source) %>% 
          summarise_at(vars(n),list(n_link=mean)) %>%
          ungroup() %>%
          mutate(scale=scale(n_link))}


Multi_link_rate <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name,'Link')]) %>% 
          group_by(ID_Source,across(ID_Dest)) %>% 
          summarise_at(vars(Link),list(link_per_year=sum)) %>%
          group_by(ID_Source) %>% 
          mutate(tot_link=sum(link_per_year)) %>%
          filter(link_per_year > 1) %>%
          group_by(ID_Source,tot_link) %>%
          summarise_at(vars(link_per_year),list(tot_multiple=sum)) %>%
          mutate(rate=tot_multiple/tot_link) 
}



### STRENGTH INDICATORS
# - Beta Hill q=5
q <- 5
Hill_lambda <- function(x,Sink_name,q){
     x[,c('ID_Source',Sink_name,'Link')] %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          summarise_at(vars(Link),list(n=sum)) %>%
          group_by(ID_Source) %>% 
          mutate(tot=sum(n)) %>%
          mutate(Hill=(n/tot)^q) %>%
          summarise_at(vars(Hill),list(Hill_l = function(x) sum(x)^(1/(1-q))))}

Hill_alpha <- function(x,Sink_name,Time_name,q){
     x[,c('ID_Source',Sink_name,Time_name,'Link')] %>% 
          group_by(ID_Source,across(Time_name),across(Sink_name)) %>% 
          summarise_at(vars(Link),list(n=sum)) %>%
          group_by(ID_Source,across(Time_name)) %>% 
          mutate(tot=sum(n)) %>%
          mutate(Hill=(n/tot)^q) %>%
          dplyr::summarise(across(Hill, sum),
                           across(tot,mean)) %>%
          group_by(ID_Source) %>% 
          mutate(W=tot/sum(tot)) %>%
          mutate(Hill_inter=Hill*W) %>%
          summarise_at(vars(Hill_inter),list(Hill_a = function(x) sum(x)^(1/(1-q))))
}

# - P_N|X=1
Single_Part <- function(x, Sink_name,Time_name){
     x[,c('ID_Source',Sink_name,Time_name,'Link')] %>% 
          group_by(ID_Source,across(Sink_name),across(Time_name)) %>% 
          summarise_at(vars(Link),list(n=sum)) %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          # mutate(Bay.dup = ifelse(duplicated(acrosst(Sink_name)) | duplicated(across(Sink_name), fromLast = TRUE), 1,0)) %>% filter(Bay.dup == 0) %>%
          filter(n() <= 1) %>%
          group_by(ID_Source) %>% 
          summarise_at(vars(n),list(single.p=sum))
}

Tot_Part <- function(x,Sink_name,Time_name){
     x[,c('ID_Source',Sink_name,Time_name,'Link')] %>% 
          group_by(ID_Source) %>% 
          summarise_at(vars(Link),list(tot.p=sum))}

### FREQUENCY INDICATORS

Frequency_info <- function(Table,Tot_time){
     Table[is.na(Table)] <- 0 # if NA are present in third to n-th column, put 0
     Table[Table > 0] <- 1
     if (ncol(unique(Table[,-c(1,2)])) != Tot_time){ # In case a Time has been simulated but no link has established, make that year accounted in the table

          missing_time <- which(1:Tot_time %nin% colnames(Table[,-c(1,2)])) # Find which time is missing in the table
          cat('Missing release time',missing_time,'will be added\n')
          
          for (t_missing in missing_time){
               Table <- add_column(Table, n_month = 0 , .after = t_missing+2-1) # Add a column
               colnames(Table)[t_missing+2] <- paste(t_missing,sep='') # rename the column
          }
     }
     
Table_info <- Table[,1:2]
Table_info$Freq <- 0;  Table_info$MaxConsecutive <- 0 ; Table_info$AvgConsecutive <- 0
for (i in 1:dim(Table)[1]) {
     Length_Occ <- rle(as.vector(Table[i,-c(1,2)],mode='numeric'))
     Info <- Length_Occ$lengths[Length_Occ$values==1]     
     Table_info$Freq[i] <- sum(Info)
     Table_info$MaxConsecutive[i] <- max(Info)
     Table_info$AvgConsecutive[i] <- mean(Info)
}
return(Table_info)
}

# ********  Case study ******** 
# associated with Supplementary File 
case1 <- data.frame(ID_Source='A',ID_Dest=rep(c('A','B','C','D','E','F','G'),each=3),Release_time=rep(1:3,7), time_unit = 'Year', Link=c(0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,0,1,1,1,1),
                    Part=c(0,50,10,0,0,10,0,0,10,10,0,10,10,10,10,0,0,10,50,10,10),T_D=NA)

# example for Frequency Indicators when no links at a specific release time (here year 2003 and 2005)  
case2 <- data.frame(ID_Source='A',ID_Dest=rep(c('A','B','C','D','E','F','G'),each=5),Release_time=rep(2001:2005,7), time_unit = 'Year', 
                    Link=c(0, 1,0,1,0, 0,0,0, 1,0,0,0,0, 1,0, 1,0,0,1, 0,  1, 1,0, 1,0, 0,0,0,1, 0,  1, 1,0, 1,0),
                    Part=c(0,50,0,10,0,0,0,0,10,0,0,0,0,10,0,10,0,0,10,0, 10,10,0, 10,0,0,0,0,10,0,50,10,0, 10,0),T_D=NA)

# Format table "case1" as needed for indicator calculation
case <- case1
case <- case[rep(seq(nrow(case)), case$Part),]

# Occurrence indicators
N_Link_func(case,'ID_Dest','Release_time')
Multi_link_rate(case,'ID_Dest','Release_time')$rate

# Stength indicators

Hill_lambda(case,'ID_Dest',5)$Hill_l/Hill_alpha(case,'ID_Dest','Release_time',5)$Hill_a

P_part_single_link_1 <-  Single_Part(case,'ID_Dest','Release_time')
P_part_single_link_2 <-  Tot_Part(case,'ID_Dest','Release_time')

P_part_single_link_df <- merge(P_part_single_link_1,P_part_single_link_2,by='ID_Source',all.y=T)
P_part_single_link_df$single.p/P_part_single_link_df$tot.p

# Frequency Indicators

# Formating Case into a dataframe 
# ----- with first 2 columns: ID_Source and ID_Dest. A
# ----- third and other columns: the Release_time
Table <- dcast(case,ID_Source + ID_Dest ~ Release_time ,value.var='Link')

# OPTIONAL CODE
# if "release time" are in a specific format: e.g., 2004, 2005, 2006, transform the Table column names into a simple order, e.g., 1=2004, 2= 2005, 3=2006
# See application with case2
# df_time <- data.frame(ID=as.numeric(unique(as.factor(case2$Release_time))),
#                       Release_time=unique(as.factor(case2$Release_time)))
# colnames(Table)[-c(1,2)] <-  df_time$ID[df_time$Release_time %in% colnames(Table[,-c(1,2)])]


# !!! 1) Checking up if the time is sorted from old time to recent time should be carried out
# !!! 2) If missing a release time because no link was established, the function Frequency_info finds the gap and adds a column in Table

Tot_time = length(unique(case1$Release_time)) # total number of years with or without links
Table_info <- Frequency_info(Table,Tot_time)
aggregate(cbind(MaxConsecutive,AvgConsecutive,Freq)~ID_Source, Table_info, function(x) mean(x)/Tot_time)

