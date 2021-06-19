# Overview
# Part_1 Environment Setup
# Part_2 Quick view of dataset
# Part_3 Data Cleaning - Add new features into dataset
# Part_4 Data Visualization & Analysis
# Part_5 Not selected visualizations

# Part_1 Environment Setup

# Empty environment
rm(list=ls())
# Install funModeling, extrafont packages
install.packages("funModeling")
install.packages("extrafont")
library(extrafont)
# Please enter "y" in console in order to continue the process
# Please NOTE: It takes a few minutes to import the font
font_import()
# Load libraries
library(plotrix)
library(ggplot2)
library(moments)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(funModeling)


# Part_2 Quick view of dataset
# Import data
boardgame_df <- read.csv("bgg_db_1806.csv")

attach(boardgame_df)

# Take a look at data
head(boardgame_df,3)
# Shows the rows and columns 
dim(boardgame_df)
# Check structure of the data
str(boardgame_df)
# Display the number of unique value in each column
rapply(boardgame_df,function(x)length(unique(x)))
# Check summary of the data
summary(boardgame_df)
# Calculate skewness for numeric data
skew <- apply(select(boardgame_df, min_players, max_players,
                     avg_time, min_time, max_time, avg_rating,
                     geek_rating, num_votes, age, owned,weight)
              ,2, skewness)
print(skew)

sd<- apply(select(boardgame_df, min_players, max_players,
                  avg_time, min_time, max_time, avg_rating,
                  geek_rating, num_votes, age, owned,weight)
           ,2, sd)
print(sd)

detach(boardgame_df)

# Part_3 Data Cleaning - Add new features into dataset

# Add new column to categorize data for further use
# Add new column "rank_group" & "rank_group_name" that categorized the rank
boardgame_df$rank_group <- floor((boardgame_df$rank-1)/20)
for(i in 1 : length(boardgame_df$rank_group)) {
  boardgame_df$rank_group_name[i] <- paste("Rank", boardgame_df$rank_group[i]*20+1,"-", boardgame_df$rank_group[i]*20+20)
}

# Add new column "weight_group" & "weight_group_name" weight
summary(boardgame_df$weight) # notes: Max of weight = 4.905
boardgame_df$weight_group <- floor(boardgame_df$weight)
for(i in 1 : length(boardgame_df$weight_group)) {
  boardgame_df$weight_group_name[i] <- paste(boardgame_df$weight_group[i],"-", boardgame_df$weight_group[i]+1)
}


# Quick view of weight v.s. age
boxplot(boardgame_df$weight~boardgame_df$age) # age=0 is not reasonable, it might indicate no info
# Frequency of age
table(boardgame_df$age)

# Add new column "age_group_name" to categorize age 
for(i in 1:length(boardgame_df$age)){
  if(boardgame_df$age[i]==0){
    boardgame_df$age_group_name[i] <- "NA"
  }
  else{
    if(boardgame_df$age[i] > 0 & boardgame_df$age[i] < 5){
      boardgame_df$age_group_name[i] <- "Toddler~Preschool (1-4)"
    }
    else{
      if(boardgame_df$age[i] > 4 & boardgame_df$age[i] < 12){
        boardgame_df$age_group_name[i] <- "Gradeschooler (5-11)"
      }
      else{
        if(boardgame_df$age[i] > 11 & boardgame_df$age[i] < 18){
          boardgame_df$age_group_name[i] <- "Teen (12-17)"
        }
        else
          boardgame_df$age_group_name[i] <- "Adult (18+)"
      }
    }
  }
}

# Sort the column
boardgame_df$age_group_name <- factor(boardgame_df$age_group_name,
                                      levels = c("NA", "Toddler~Preschool (1-4)", 
                                                 "Gradeschooler (5-11)", "Teen (12-17)",
                                                 "Adult (18+)"))

# Frequency of age_group_names
table(boardgame_df$age_group_name)


# Part_4 Data Visualization & Analysis

# Generate common theme for further plotting use
common_theme <- function() {
  ptcolor <- 'grey20' 
  theme(
    plot.title=element_text(size=14, lineheight=0.8, color=ptcolor, hjust=0.5),
    axis.title.x=element_text(color=ptcolor),
    axis.title.y=element_text(color=ptcolor),
    text=element_text(family="Comic Sans MS", face="bold"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.line = element_line(colour = "#2C3E50"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
}

# Generate dataframe with rank 1-100 data for further use
boardgame_rank100 <- boardgame_df[1:100,]

# Check the year column 
table(boardgame_df$year)
# select year from 1968-2018 (51 years)
select_year <- subset(boardgame_df, year>1967 & year<2018)
year_frequency <- as.data.frame(table(select_year$year))
names(year_frequency)[1] <- "year"
year_frequency

# Generate game_released_year plot
game_released_year <- 
  ggplot(year_frequency, aes(x =year, y = Freq, group=1))+
  geom_line(col="#F39C12",size=2)+
  geom_point(col="#0E6655", size=1.5)+
  ggtitle("Board Game Released by Year")+
  labs(x="Year", y="Count")+
  theme(axis.text.x=element_text(angle=75,hjust=1))+
  common_theme()

game_released_year
ggsave("game_released_year.png", game_released_year, bg = "transparent")

# Generate Frequency table of min_players 
min_players_df <- as.data.frame(table(boardgame_df$min_players))
names(min_players_df)[1] <- "min_players"
min_players_df
# Generate BarPlot to show frequency table of min_palyers
ggplot(min_players_df , aes(x=min_players, y=Freq)) +
  geom_bar(stat="identity",color="black", fill="blue", alpha=0.3) + 
  ggtitle("Count of Minimum Players") +
  labs(x="Number of Minimum Players", y="Count")+
  common_theme() 

# Gnerate Percentage table for min_players
min_players_perc <- as.data.frame(prop.table(table(boardgame_df$min_players)))
names(min_players_perc)[1] <- "min_players_perc"
min_players_perc
# Generate BarPlot to show percentage table of min_palyers
min_players_perc_plot <-
  ggplot(min_players_perc , aes(x=min_players_perc, y=Freq)) +
  geom_bar(stat="identity",color="#999999", fill="#F5B7B1", alpha=0.7) + 
  ggtitle("Relative Frequency of Minimum Players") +
  labs(x="Number of Minimum Players", y="Percentage")+
  scale_y_continuous(labels = function(Freq) paste0(round(Freq, 2) * 100, "%"),breaks=seq(0,0.7,by=0.1))+
  common_theme() 
min_players_perc_plot
ggsave("min_players_perc_plot.png", min_players_perc_plot, bg = "transparent")

# Generate Frequency table of max_players 
max_players_df <- as.data.frame(table(boardgame_df$max_players))
names(max_players_df)[1] <- "max_players"
max_players_df
# Generate BarPlot to show frequency table of max_palyers
ggplot(max_players_df , aes(x=max_players, y=Freq)) +
  geom_bar(stat="identity",color="black", fill="blue", alpha=0.3) + 
  ggtitle("Count of Maximum Players") +
  labs(x="Number of Maximum Players", y="Count")+
  common_theme() 

# Gnerate Percentage table for max_players
max_players_perc <- as.data.frame(prop.table(table(boardgame_df$max_players)))
names(max_players_perc)[1] <- "max_players_perc"
max_players_perc
# Generate BarPlot to show percentage table of max_palyers
max_players_perc_plot <-
  ggplot(max_players_perc , aes(x=max_players_perc, y=Freq)) +
  geom_bar(stat="identity",color="#999999", fill="#F5B7B1", alpha=0.7) + 
  ggtitle("Relative Frequency of Maximum Players") +
  labs(x="Number of Maximum Players", y="Percentage")+
  scale_y_continuous(labels = function(Freq) paste0(round(Freq, 2) * 100, "%"),breaks=seq(0,0.35,by=0.02))+
  common_theme()

max_players_perc_plot
ggsave("max_players_perc_plot.png", max_players_perc_plot, bg = "transparent")

# Generate density plot for "weight" & "avg_time" column 
# Weight density plot
weight_mean <- mean(boardgame_df$weight)
weight_density <- ggplot(boardgame_df, aes(x=weight)) + 
  geom_density(color="#999999", fill="#E59866", alpha=0.7)+
  geom_vline(aes(xintercept=mean(weight)),
             color="#A04000", linetype="dashed", size=1)+
  geom_text(aes(label=paste("Mean =",round(weight_mean,1)),x=2.8, y=0.5),col='#A04000',size=4, family="Comic Sans MS")+
  ggtitle("Density Plot of Difficulty")+
  labs(x="Difficulty", y = "Density")+
  common_theme()

weight_density
ggsave("weight_density.png", weight_density, bg = "transparent")


# avg_time density plot (remove outlier in the plot)

avg_time_density <- ggplot(boardgame_df, aes(x=avg_time)) + 
  geom_density(color="#999999", fill="#E59866", alpha=0.7)+
  geom_vline(aes(xintercept=mean(owned)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Density Plot of Average Time")+
  labs(x = "Average Time", y="Density")+
  scale_x_continuous(breaks=seq(0,300,by=20), limits = c(0,300))+
  common_theme()

avg_time_density
ggsave("avg_time_density.png", avg_time_density, bg = "transparent")


# Generate Frequency table of "age_group_name"
age_group_freq <- as.data.frame(table(boardgame_df$age_group_name))
names(age_group_freq)[1] <- "age_group_name"
age_group_freq
# Generate BarPlot to show frequency table of Age group
age_group_freq_plot <-
  ggplot(subset(age_group_freq,!age_group_name %in% c("NA")), aes(x=age_group_name, y=Freq)) +
  geom_bar(stat="identity",color="#999999", fill="#AF7AC5", alpha=0.7) + 
  ggtitle("Frequency of Age Group (All Board Games)") +
  labs(x="Age Group", y="Count")+
  common_theme() 

age_group_freq_plot
ggsave("age_group_freq_plot.png", age_group_freq_plot, bg = "transparent")



# Category Frequency Plot (whole dataset)
# Clean "category" column
attach(boardgame_df)
clean_category <- str_trim(unlist(strsplit(str_trim(as.character(category)),",")))
clean_category
# Generate Frequency table of "category"
category_df <- as.data.frame(table(clean_category))
category_df
# Generate category barplot
category_barplot <-ggplot(category_df, aes(x=Freq, y=reorder(clean_category, Freq))) +
  geom_bar(stat="identity") + ggtitle("Frequency of Board Game Category") +
  labs(x="Frequency", y="Category")+
  common_theme() + theme(axis.text=element_text(size=4),
                         axis.title=element_text(size=10,face="bold"))
# Only display first 10 in plot
top10_df <- category_df[tail(order(category_df$Freq), 10), ]
top10_df
category_freq_plot <-
  ggplot(top10_df, aes(x=Freq, y=reorder(clean_category, Freq),fill=Freq)) +
  geom_bar(stat="identity") + ggtitle("Frequency of Board Game Category (Overall)") +
  labs(x="Frequency", y="Category")+
  common_theme()+
  theme(legend.title = element_blank(),legend.position='none',
        axis.text.x=element_text(color='grey20', size=14),
        axis.text.y=element_text(color='grey20', size=14))
  
category_freq_plot
ggsave("category_freq_plot.png",category_freq_plot, bg = "transparent")
detach(boardgame_df)

# Category Frequency Plot (only rank 1-100 dataset)
# The same as above process
# Clean "category" column
clean_category_rank100 <- str_trim(unlist(strsplit(str_trim(as.character(boardgame_rank100$category)),",")))
clean_category_rank100 
# Generate frequency table
category_df_rank100 <- as.data.frame(table(clean_category_rank100))
category_df_rank100
# Assign dataframe for displaying only 10 data in plot
top10_df_rank100 <- category_df_rank100[tail(order(category_df_rank100$Freq), 10), ]
top10_df_rank100
# Generate rank100 category frequency plot
category_freq_plot_100 <-
  ggplot(top10_df_rank100, aes(x=Freq, y=reorder(clean_category_rank100, Freq), fill= Freq)) +
  geom_bar(stat="identity") + ggtitle("Frequency of Board Game Category (Top 100)") +
  labs(x="Frequency", y="Category")+
  common_theme() + 
  theme(legend.title = element_blank(),legend.position='none',
        axis.text.x=element_text(color='grey20', size=14),
        axis.text.y=element_text(color='grey20', size=14))


category_freq_plot_100
ggsave("category_freq_plot_100.png",category_freq_plot_100, bg = "transparent")


# Generate BoxPlot to explore the category data
# Average Rating versus Difficulty
difficulty_versus_average_rating <-
  ggplot(boardgame_df, aes(x=weight_group_name, y=avg_rating, fill=weight_group_name, alpha=0.9)) +
  geom_boxplot(color="#999999")+common_theme()+ggtitle("Average Rating versus Difficulty") +
  labs(x="Difficulty", y="Average Rating")+
  common_theme()+
  theme(legend.position='none')
difficulty_versus_average_rating
ggsave("difficulty_versus_average_rating.png",difficulty_versus_average_rating, bg = "transparent")


# Designer Frequency
attach(boardgame_df)
clean_designer <- str_trim(unlist(strsplit(str_trim(as.character(designer)),",")))
designer_df <-  as.data.frame(table(clean_designer))
# Remove useless value
remove_designer <- c("(Uncredited)", "Jr.","none")
designer_df <- filter(designer_df, !clean_designer %in% remove_designer)
designer_df <- designer_df[tail(order(designer_df$Freq), 10), ]
designer_df
designer_top10 <-
  ggplot(designer_df, aes(x=Freq, y=reorder(clean_designer, Freq), fill= Freq)) +
  geom_bar(stat="identity", color="#999999") + ggtitle("Frequency of Designer (All Board Games)") +
  labs(x="Frequency", y="Designer")+
  common_theme()+theme(legend.position='none')
designer_top10
ggsave("designer_top10.png",designer_top10, bg = "transparent")

# Designer Frequency rank 100 
clean_designer_rank100 <- str_trim(unlist(strsplit(str_trim(as.character(boardgame_rank100$designer)),",")))
designer_df_rank100 <-  as.data.frame(table(clean_designer_rank100))
designer_df_rank100 <- designer_df_rank100[tail(order(designer_df_rank100$Freq), 10), ]
designer_df_rank100
designer_top10_rank100 <-ggplot(designer_df_rank100, aes(x=Freq, y=reorder(clean_designer_rank100, Freq), fill= Freq)) +
  geom_bar(stat="identity",color="#999999") + ggtitle("Frequency of Designer (Top 100 Board Games)") +
  labs(x="Frequency", y="Designer")+
  common_theme() + theme(legend.position='none',
                         axis.text.x=element_text(color='grey20', size=14),
                         axis.text.y=element_text(color='grey20', size=14))
designer_top10_rank100
ggsave("designer_top10_rank100.png",designer_top10_rank100, bg = "transparent")

# generate rank 1-10 designer name list
df_first_10 <- boardgame_df[1:10,]
df_first_10 <- str_trim(unlist(strsplit(str_trim(as.character(df_first_10$designer)),",")))
df_first_10 

# Frequency of designer for overall board game, also highlight the designer who has game in rank 10
designer_fre_plot <-
  ggplot(designer_df, aes(x=Freq, y=reorder(clean_designer, Freq),alpha=0.7)) +
  geom_bar(stat="identity", color="#999999", fill=ifelse(designer_df$clean_designer %in% df_first_10,"#EC7063","#F4D03F")) + 
  ggtitle("Frequency of Designer (Overall Board Games)",subtitle = "Hightlight in Red for Rank 10 Designers") +
  labs(x="Frequency", y="Designer")+
  common_theme() + theme(legend.position='none')+
  theme(legend.position='none',plot.subtitle = element_text(size=12, lineheight=0.8, color='grey20', hjust=0.5),
        axis.text.x=element_text(color='grey20', size=14),
        axis.text.y=element_text(color='grey20', size=14))

designer_fre_plot
ggsave("designer_fre_plot.png",designer_fre_plot, bg = "transparent")

# Top10 designer of rank 100 boardgame, highligh top10 freqency
designer_fre_plot_100 <-
  ggplot(designer_df_rank100, aes(x=Freq, y=reorder(clean_designer_rank100, Freq),alpha=0.7)) +
  geom_bar(stat="identity", color="#999999", fill=ifelse(designer_df_rank100$clean_designer_rank100 %in% df_first_10, "#EC7063","#F4D03F")) + 
  ggtitle("Frequency of Top 10 Designers (Top 100 Board Games)",subtitle = "Hightlight in Red for Rank 10 Designers") +
  labs(x="Frequency", y="Designer")+
  common_theme() + 
  theme(legend.position='none',plot.subtitle = element_text(size=12, lineheight=0.8, color='grey20', hjust=0.5),
        axis.text.x=element_text(color='grey20', size=14),
        axis.text.y=element_text(color='grey20', size=14))

designer_fre_plot_100
ggsave("designer_fre_plot_100.png",designer_fre_plot_100, bg = "transparent")


# Owned frequency 
boardgame_df %>% 
  arrange(desc(owned)) %>% 
  slice(1:10)%>%
  ggplot(., aes(x=owned, y=reorder(names,owned)))+
  geom_bar(stat='identity')+
  common_theme()

# Owned frequency rank100
owned_freq_plot <-
  boardgame_rank100 %>% 
  arrange(desc(owned)) %>% 
  slice(1:10)%>%
  ggplot(., aes(x=owned, y=reorder(names,owned)))+
  geom_bar(stat='identity', color="#999999", fill="#EB984E", alpha=0.7)+
  ggtitle("Frequency of Owned (Top 100 Board Games) ")+
  labs(x="Owned", y="Board Game Name")+
  common_theme()+
  theme(axis.text.x=element_text(color='grey20', size=14),
        axis.text.y=element_text(color='grey20', size=14))

owned_freq_plot
ggsave("owned_freq_plot.png",owned_freq_plot, bg = "transparent")

# correlation between factors
correlation_table(data=boardgame_df, target="owned")
#correlation_table(data=boardgame_df, target="geek_rating")
#correlation_table(data=boardgame_df, target="avg_rating")
#correlation_table(data=boardgame_df, target="weight")

#-------------------------------------------------------------------------
# Part_5 Not selected visualizations

# Visualization and exploration (Not selected to include in the final ppt)

# Age density plot
age_density <- ggplot(boardgame_df, aes(x=age)) + 
  geom_density(color="black", fill="blue", alpha=0.3)+
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Density Plot of Age")+
  common_theme()

age_density

# Avg rating density plot
avg_rating_density <- ggplot(boardgame_df, aes(x=avg_rating)) + 
  geom_density(color="black", fill="blue", alpha=0.3)+
  geom_vline(aes(xintercept=mean(avg_rating)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Density Plot of Average Rating")+
  common_theme()

avg_rating_density

# Geek rating density plot
geek_rating_density <- ggplot(boardgame_df, aes(x=geek_rating)) + 
  geom_density(color="black", fill="blue", alpha=0.3)+
  geom_vline(aes(xintercept=mean(geek_rating)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Density Plot of Geek Rating")+
  common_theme()

geek_rating_density

# owned density plot

owned_density <- ggplot(boardgame_df, aes(x=owned)) + 
  geom_density(color="black", fill="blue", alpha=0.3)+
  geom_vline(aes(xintercept=mean(owned)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Density Plot of Owned")+
  common_theme()

owned_density


# Generate Frequency table of min_players for rank100
min_players_df_100 <- as.data.frame(table(boardgame_rank100$min_players))
names(min_players_df_100)[1] <- "min_players"
min_players_df_100
# Generate BarPlot to show frequency table of min_palyers for rank100
ggplot(min_players_df_100 , aes(x=min_players, y=Freq)) +
  geom_bar(stat="identity",color="black", fill="blue", alpha=0.3) + 
  ggtitle("Frequency of min players") +
  labs(x="min_players", y="Count")+
  common_theme() 


# Generate Frequency table of max_players for rank100
max_players_df_100 <- as.data.frame(table(boardgame_rank100$max_players))
names(max_players_df_100)[1] <- "max_players"
max_players_df_100
# Generate BarPlot to show frequency table of max_palyers for rank100
ggplot(max_players_df_100 , aes(x=max_players, y=Freq)) +
  geom_bar(stat="identity",color="black", fill="blue", alpha=0.3) + 
  ggtitle("Frequency of Max Players for Rank100") +
  labs(x="Age Group", y="Count")+
  common_theme() 

# Difficulty versus Age Group
ggplot(subset(boardgame_df, !age_group_name %in% c("NA")), aes(x=age_group_name, y=weight, fill=age_group_name)) +
  geom_boxplot()+common_theme()+ggtitle("Difficulty versus Age Group") +
  labs(x="Age", y="Difficulty")+
  theme(legend.position='none')

# Geek Rating versus Difficulty
ggplot(boardgame_df, aes(x=weight_group_name, y=geek_rating, fill=weight_group_name)) +
  geom_boxplot()+common_theme()+ggtitle("Geek Rating versus Difficulty") +
  labs(x="Difficulty", y="Geek Rating")+
  theme(legend.position='none')

# Rank 100 Difficulty versus Rank group
difficulty_versus_rankgroup <-
  ggplot(boardgame_rank100, aes(x=rank_group_name, y=weight, fill=rank_group_name, alpha=0.7)) +
  geom_boxplot()+common_theme()+ggtitle("Difficulty versus Rank Group") +
  labs(x="Rank Group", y="Difficulty")+ 
  common_theme()+
  theme(legend.position='none')
difficulty_versus_rankgroup


# Generate Frequency table of "age_group_name" for rank100
age_group_freq_100 <- as.data.frame(table(boardgame_rank100$age_group_name))
names(age_group_freq_100 )[1] <- "age_group_name"
age_group_freq_100
# Generate BarPlot to show frequency table of Age group for rank100
age_group_freq_100_plot <-
  ggplot(subset(age_group_freq_100 ,!age_group_name %in% c("NA")), aes(x=age_group_name, y=Freq)) +
  geom_bar(stat="identity",color="#999999", fill="#AF7AC5", alpha=0.7) + 
  ggtitle("Frequency of Age Group (Top 100 Board Games)") +
  labs(x="Age Group", y="Count")+
  common_theme() 

age_group_freq_100_plot

