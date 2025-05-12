install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("tidyverse")


library("tidyverse")
library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")

data <- read.csv("E:/RStudio/RStudio practice/Sanga-s-Stat-Dashboard/sanga_matches_test.csv")
View(data)

#calc not out Innings
notout1 <-sum(grepl("\\*",data$Innings1),na.rm = TRUE)
notout2 <-sum(grepl("\\*",data$Innings2),na.rm = TRUE)
totalnotout <- notout1+notout2

#display result
cat("Not Out (NO)") +
print(totalnotout)

#calc Total Innings
matches <- nrow(data)
dnb <- sum(data$Innings1=="DNB" | data$Innings2=="DNB", na.rm = TRUE)
missing <- sum(data$Innings1=="-" | data$Innings2=="-",na.rm = TRUE)
Innings <- (matches *2) - (dnb + missing)

#display Result
cat("Innings") +
print(Innings)

#calc Highest runs
clean <- function(inn){
  inn <- gsub("\\*","",inn)
  inn <- gsub("DNB",NA,inn)
  inn <- gsub("-",NA,inn)
  as.numeric(inn)
}

inn1_clean <- clean(data$Innings1)
inn2_clean <- clean(data$Innings2)

max_inn1 <- max(inn1_clean,na.rm = TRUE)
max_inn2 <- max(inn2_clean,na.rm = TRUE)
max <- max(max_inn1,max_inn2,na.rm = TRUE)

#Generate stat
stat <- data.frame(
  Metric = c("Matches","Innings","Not Outs","Runs","Highest","Average"),
  value = c(
    as.integer(matches),  # Convert to integer
    as.integer(Innings),
    as.integer(totalnotout),
    as.integer(sum(as.numeric(gsub("[^0-9]","",data$Runs)), na.rm = TRUE)),
    as.integer(max),
    round(sum(as.numeric(gsub("[^0-9]","",data$Runs)), na.rm = TRUE)/
            (Innings - totalnotout), 2)  # Keep decimal for average
  )
)

stat$value[-6] <- as.integer(stat$value[-6]) 

#display result
print(stat)

#plot stat data
ggplot(stat,aes(x=Metric,y=1,label=format(value,big.mark=",")))+
  geom_tile(fill="white",color="black",width=0.9,height=0.5)+
  geom_text(size = 6,fontface ="bold",vjust=0.5)+
  scale_x_discrete(position = "top")+
  labs(title = "Kumar Sangakkara Test Stats")+
  theme_void()+
  theme(
    axis.text.x = element_text(size = 12,face = "bold",margin = margin(b=10)),
    plot.title = element_text(size = 16,face = "bold",hjust = 0.5,margin = margin(b=15)),
    plot.margin = margin(5,5,5,5)
  )
View(data)

library("lubridate")

#calc runs in year
runbyyear <- data %>%
  mutate(
    Runs=as.numeric(gsub("[^0-9]","",Runs)),
    year = year(dmy(Start.Date))
  ) %>%
  group_by(year) %>%
  summarise(Total_Runs=sum(Runs,na.rm = TRUE))

View(runbyyear)

#plot run by year
ggplot(runbyyear,aes(x=year,y=Total_Runs))+
  geom_line(color = "deepskyblue2", linewidth = 2) +
  geom_point(color = "firebrick2", size = 5) +
  labs(
    title = "Kumar Sangakkara: Annual Test Runs",
    x = "Year",
    y = "Total Runs"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(runbyyear$year), max(runbyyear$year), by = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )


#calc milestones
milestone <- data %>%
  mutate(Runs1=as.numeric(gsub("[^0-9]","",Innings1)),
         Runs2=as.numeric(gsub("[^0-9]","",Innings2))
         )

View(milestone)

#1st Innings Milestones
counts1 <- list(
  fifties = sum(milestone$Runs1 >= 50 & milestone$Runs1 < 100, na.rm = TRUE),
  hundreds = sum(milestone$Runs1 >= 100 & milestone$Runs1 < 150, na.rm = TRUE),
  onefifties = sum(milestone$Runs1 >= 150 & milestone$Runs1 < 200, na.rm = TRUE),
  twohundreds = sum(milestone$Runs1 >= 200, na.rm = TRUE)
)

print(counts1)

#2st Innings Milestones
counts2 <- list(
  fifties = sum(milestone$Runs2 >= 50 & milestone$Runs2 < 100, na.rm = TRUE),
  hundreds = sum(milestone$Runs2 >= 100 & milestone$Runs2 < 150, na.rm = TRUE),
  onefifties = sum(milestone$Runs2 >= 150 & milestone$Runs2 < 200, na.rm = TRUE),
  twohundreds = sum(milestone$Runs2 >= 200, na.rm = TRUE)
)

print(counts2)


#make table
table1 <- data.frame(
  milestone =factor(rep(c("50s","100s","150s","200s"),2),
                    levels = c("50s","100s","150s","200s"),
                    ordered = TRUE),
  Innin =rep(c("1st","2nd"),each=4),
  Count = c(counts1$fifties, counts1$hundreds, counts1$onefifties, counts1$twohundreds,
            counts2$fifties, counts2$hundreds, counts2$onefifties, counts2$twohundreds)
)

View(table1)

finalmilestone <-table1 %>%
  group_by(milestone) %>%
  summarise(Total=sum(Count))
  
View(finalmilestone)

#plot milestone data
ggplot(finalmilestone,aes(x=Total,y=milestone,fill = milestone))+
  geom_bar(stat = "identity",width = 0.7)+
  geom_text(aes(label = Total),vjust=0.5,hjust=-0.5,size = 5,fontface="bold")+
  labs(title = "Milestone Summary",
       x="Milestone",
       y="count")+
  scale_fill_manual(values = c("50s" = "lightskyblue4",
                               "100s" = "lightskyblue4",
                               "150s" = "lightskyblue4",
                               "200s" = "lightskyblue4"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 16,face = "bold"),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )

#calc 1st inning range
inn1range <- milestone %>%
  mutate(
    score_range = case_when(
      Runs1 < 1 ~"0",
      Runs1 >=1 & Runs1 < 50 ~ "1-49",
      Runs1 >=50 & Runs1 < 100 ~ "50-99",
      Runs1 >=100 & Runs1 < 150 ~ "100-149",
      Runs1 >=150 & Runs1 < 200 ~ "150-199",
      Runs1 >=200 ~ "200+",
      TRUE ~ "DNB"
    ) 
  ) %>%
  count(score_range) %>%
  mutate(score_range=factor(score_range,
                            levels = c("0", "1-49", "50-99", "100-149", "150-199", "200+", "DNB"),
                            ordered = TRUE)) %>%
  arrange(score_range)

#print result
print(inn1range)

#calc 2nd Innings range
inn2range <- milestone %>%
  mutate(
    score_range = case_when(
      Runs2 < 1 ~"0",
      Runs2 >=1 & Runs2 < 50 ~ "1-49",
      Runs2 >=50 & Runs2 < 100 ~ "50-99",
      Runs2 >=100 & Runs2 < 150 ~ "100-149",
      Runs2 >=150 & Runs2 < 200 ~ "150-199",
      Runs2 >=200 ~ "200+",
      TRUE ~ "DNB"
    ) 
  ) %>%
  count(score_range) %>%
  mutate(score_range=factor(score_range,
                            levels = c("0", "1-49", "50-99", "100-149", "150-199", "200+", "DNB"),
                            ordered = TRUE)) %>%
  arrange(score_range)

#print result
print(inn2range)

#make conbine table
com_ranges <- bind_rows(
  inn1range %>% mutate(Innings = "1st"),
  inn2range %>% mutate(Innings = "2nd")
) %>%
  pivot_wider(names_from = Innings,values_from = n) %>%
  mutate(Total = `1st` + `2nd`)

#display result
print(com_ranges)

#plot com_ranges

ggplot(com_ranges,aes(x=Total,y=score_range,fill = score_range))+
  geom_bar(stat = "identity",width = 0.7)+
  geom_text(aes(label = Total),vjust=0.5,hjust=-0.3,size = 5,fontface="bold")+
  labs(title = "Summary Of Runs Range",
       x="Range",
       y="count")+
  scale_fill_manual(values = c("0" = "lightskyblue4",
                               "1-49" = "lightskyblue4",
                               "50-99" = "lightskyblue4",
                               "100-149" = "lightskyblue4",
                               "150-199" = "lightskyblue4",
                               "200+" = "lightskyblue4",
                               "DNB" = "lightskyblue4"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 16,face = "bold"),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )

# Calculate total catches and stumpings
feilding_stats <- data %>%
  summarise(
    Total_Catches = sum(Catches, na.rm = TRUE),
    Total_Stumpings = sum(Stumpings, na.rm = TRUE)
  )

#display result
print(feilding_stats)

# By opposition team
feilding_opposition <- data %>%
  group_by(Opposition) %>%
  summarise(
    Catches = sum(Catches, na.rm = TRUE),
    Stumpings = sum(Stumpings, na.rm = TRUE),
    Total = sum(Catches +Stumpings,na.rm = TRUE),
  )

#display result
print(feilding_opposition)

#percentages
feilding_opposition <- feilding_opposition %>%
  mutate(Percentage = round(Total/sum(Total)*100, 1))

#display result
print(feilding_opposition)

#plot the pie chart
ggplot(feilding_opposition, aes(x = "", y = Total, fill = Opposition)) +  
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Opposition, "\n", Total)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Dismissals by Opposition",
       fill = "Opposition") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")
  
# Convert to long format first
feilding_stats_long <- feilding_stats %>% 
  pivot_longer(everything(), names_to = "Stat", values_to = "Count")

#plot feildings
ggplot(feilding_stats_long, aes(x = Stat, y = Count)) +
  geom_col(fill = c("steelblue", "darkorange")) +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Fielding Statistics") +
  theme_minimal()

# By year
feilding_by_year <- data %>%
  mutate(Year = lubridate::year(lubridate::dmy(Start.Date))) %>%
  group_by(Year) %>%
  summarise(
    Catches = sum(Catches, na.rm = TRUE),
    Stumpings = sum(Stumpings, na.rm = TRUE)
  )

#display result
print(feilding_by_year)

# Convert to long format for ggplot
feilding_long <- feilding_by_year %>%
  pivot_longer(cols = c(Catches, Stumpings), names_to = "Type", values_to = "Count")

# Plot
ggplot(feilding_long, aes(x = factor(Year), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fielding Performance by Year",
       x = "Year",
       y = "Count",
       fill = "Type") +
  theme_minimal()


























