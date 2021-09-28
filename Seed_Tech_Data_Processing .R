#Seed_Tech_Data
library(tidyverse)

#Exploratatory Bar charts 
# import data: Compiled_Seed_Tech_Emergence_and_Drydown.csv
Compiled_data <- read.csv(file.choose(), header = TRUE)
Seed_Tech_Drydown <- Compiled_data %>%
  filter(days.alive.exclude != "Y")

#size
#average across replicates
Drydown_size <- Seed_Tech_Drydown %>%
  group_by(water.trt, species, seed.trt) %>%
  summarise(Size = mean(Total.leaf.area.cm, na.rm = TRUE), sd = sd(Total.leaf.area.cm, na.rm = TRUE)) %>%
  select(water.trt, species, seed.trt, Size, sd)
#Create bar graph 1
ggplot(data = Drydown_size, aes(x = seed.trt, y = Size, fill = water.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species, nrow = 1) +
  geom_errorbar(aes(ymin = Size-sd, ymax = Size+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)
#Create bar graph 2
ggplot(data = Drydown_size, aes(x = water.trt, y = Size, fill = seed.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species, nrow = 1) +
  geom_errorbar(aes(ymin = Size-sd, ymax = Size+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)

#survival time 
#average across replicates
Drydown_survival <- Seed_Tech_Drydown_exclude %>%
  group_by(water.trt, species, seed.trt) %>%
  summarise(survival = mean(days.alive, na.rm = TRUE), sd = sd(days.alive, na.rm = TRUE)) %>%
  select(water.trt, species, seed.trt, survival, sd)
#Create bar graph
ggplot(data = Drydown_survival, aes(x = water.trt, y = survival, fill = seed.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species) +
  geom_errorbar(aes(ymin = survival-sd, ymax = survival+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)

#leaf width
Drydown_width <- Seed_Tech_Drydown %>%
  group_by(water.trt, species, seed.trt) %>%
  summarise(width = mean(Average.leaf.width, na.rm = TRUE), sd = sd(Average.leaf.width, na.rm = TRUE)) %>%
  select(water.trt, species, seed.trt, width, sd)
#plot 1
ggplot(data = Drydown_width, aes(x = seed.trt, y = width, fill = water.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species) +
  geom_errorbar(aes(ymin = width-sd, ymax = width+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)
#plot 2
ggplot(data = Drydown_width, aes(x = water.trt, y = width, fill = seed.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species) +
  geom_errorbar(aes(ymin = width-sd, ymax = width+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)

#Growth rate 
Drydown_growthrate <- Seed_Tech_Drydown %>%
  group_by(water.trt, species, seed.trt) %>%
  summarise(growth = mean(Growth.rate..cm.growth.day., na.rm = TRUE), sd = sd(Growth.rate..cm.growth.day., na.rm = TRUE)) %>%
  select(water.trt, species, seed.trt, growth, sd)
#plot 1 
ggplot(data = Drydown_growthrate, aes(x = water.trt, y = growth, fill = seed.trt)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  facet_wrap(~species) +
  geom_errorbar(aes(ymin = growth-sd, ymax = growth+sd), position = position_dodge(.9), width = .5, na.rm = TRUE)


#Exploratory Regressions
#average across replicates
Regression <- Seed_Tech_Drydown %>%
  group_by(water.trt, species, seed.trt) %>%
  select(water.trt, species, seed.trt, days.alive, Total.leaf.area.cm, T50)

#Create regression size vs. survival
ggplot(data = Regression, aes(x = Total.leaf.area.cm, y = days.alive)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(species ~ water.trt, scale = "free_x")

#experimental size vs. survival plot
Regression_1 <- Regression %>%
  filter(species == "EE")
EE <- ggplot(data = Regression_1, aes(x = Total.leaf.area.cm, y = days.alive)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE) 

Regression_2 <- filter(Regression, species == "PS")
PS <- ggplot(data = Regression_2, aes(Total.leaf.area.cm, y = days.alive)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE)
  
Regression_3 <- filter(Regression, species == "PV")  
PV <- ggplot(data = Regression_3, aes(Total.leaf.area.cm, y = days.alive)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE)


#Regression size vs. T50 
ggplot(data = Regression, aes(x = T50, y = Total.leaf.area.cm)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(species ~ water.trt, scale = "free", ncol = 3)

#Regression survival vs. T50
ggplot(data = Regression, aes(x = T50, y = days.alive)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(species ~ water.trt, scale = "free")

#leaf length vs. width
leaf_analysis <- Seed_Tech_Drydown %>%
  group_by(water.trt, species, seed.trt) %>%
  mutate(average.leaf.length = tot.leaf.length/leaf.num) %>%
  select(water.trt, species, seed.trt, Average.leaf.width.mm, average.leaf.length)
ggplot(data = leaf_analysis, aes(x = average.leaf.length, y = Average.leaf.width.mm)) +
  geom_point(aes(color = seed.trt)) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(species ~ water.trt, scale = "free")


#Seed_Tech_Emergence
#import: Bea_2020 - Seed_Tech_Emergence_Proportion
Seed_Tech_Emergence <- read.csv(file.choose(), header = TRUE)
library(tidyverse)

#average across replicates 
by_trt <- group_by(Seed_Tech_Emergence, water.trt, species, seed.trt)
Averaged_reps <- by_trt %>%
  summarise_all(list(mean)) %>%
  select(-Position..block...) %>%
  select(-rep) %>%
  select(-max_num_of_seeds)

#Wide to long
#Transform dates to one column
Long_Data <- gather(Averaged_reps, Date, Percentage_Seeds_Emerged, X7.29:X10.8, factor_key=TRUE)

#Try different plots 
# comparing % seeds emerged by water trt
ggplot(data = Long_Data) +
  geom_line(mapping = aes(x = Date, y = Percentage_Seeds_Emerged, group = water.trt, color = water.trt)) +
  facet_wrap(species ~ seed.trt) +
  geom_vline(xintercept = 8.24)

# comparing % seeds emerged by species 
ggplot(data = Long_Data) +
  geom_line(mapping = aes(x = Date, y = Percentage_Seeds_Emerged, group = species, color = species)) +
  facet_wrap(water.trt ~ seed.trt)

# comparing % seeds emerged by seed trt
ggplot(data = Long_Data) +
  geom_line(mapping = aes(x = Date, y = Percentage_Seeds_Emerged, group = seed.trt, color = seed.trt)) +
  facet_wrap(water.trt ~ species) +
  geom_vline(xintercept = 8.24) +
  labs(y = "Percentage Seeds Emerged", x = "Days Since Planting", color = "Water Treatment")


#Creating T50 and T10 emergence plots 
#Calculate mean germination times
Mean_Days_Emergence <- Compiled_data %>% 
  group_by(water.trt, seed.trt, species) %>%
  summarise(mean_1 = mean(T50, na.rm = TRUE), sd.1 = sd(T50, na.rm = TRUE), 
            mean_2 = mean(T10, na.rm = TRUE), st.2 = sd(T10, na.rm = TRUE))

#factoring data
Mean_Days_Emergence_factored <- Mean_Days_Emergence
Mean_Days_Emergence_factored$seed.trt <- factor(Mean_Days_Emergence$seed.trt, levels = c("c", "f", "p", "aba"))

#T50 plot 
ggplot(data = Mean_Days_Emergence_factored, aes(x = water.trt, y = mean_1, fill=seed.trt, na.rm = TRUE)) +
  geom_bar(stat="identity", position=position_dodge(), na.rm = TRUE) +
  labs(x = "Water Treatment", y = "Days to 50% Emergence", fill = "Seed Treatment") +
  facet_wrap(~ species) +
  scale_fill_discrete(labels = c("Control", "Fungicide", "Primer", "Abscisic Acid")) +
  geom_hline(yintercept = 28, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_1 - sd.1, ymax= mean_1 + sd.1), width = .5, position = position_dodge(.9), color = "blue", na.rm = TRUE)
#T10 plot
ggplot(data = Mean_Days_Emergence_factored, aes(x = water.trt, y = mean_2, fill=seed.trt, na.rm = TRUE)) +
  geom_bar(stat="identity", position=position_dodge(), na.rm = TRUE) +
  labs(x = "Water Treatment", y = "Days to 10% Emergence", fill = "Seed Treatment") +
  facet_wrap(~ species) +
  scale_fill_discrete(labels = c("Control", "Fungicide", "Primer", "Abscisic Acid")) +
  geom_hline(yintercept = 28, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_2 - st.2, ymax= mean_2 + st.2), width = .5, position = position_dodge(.9), color = "blue", na.rm = TRUE)


#Seed_Tech_Watering
#Creating plot of vwc over time  
#import data: Bea_2020 - Watering_for_R.csv
Seed_Tech_Watering <- read.csv(file.choose(), header = TRUE)

library(tidyverse)
#average across replicates
Seed_Tech_Watering$average_vwc <- rowMeans(subset(Seed_Tech_Watering, select = c(rep1, rep2, rep3)), na.rm = TRUE)

#select needed columns 
#group_by treatment and date
#calculate mean
Averaged_data <- Seed_Tech_Watering %>%
  transmute(Time = date, treatment, average_vwc) %>%
  select(Time, treatment, average_vwc) %>%
  group_by(treatment, Time) %>%
  summarise(Volumetric_Water_Content = mean(average_vwc, na.rm = TRUE))

#create plot 
ggplot(data = Averaged_data) +
  geom_line(mapping = aes(x = Time, y = Volumetric_Water_Content, group = treatment, color = treatment)) +
  scale_x_discrete(breaks = c("7/27","8/02","8/08","8/14","8/20","8/26","9/01","9/07","9/13","9/19")) +
  labs(y = "% Volumetric Water Content", color = "Treatment") + 
  scale_color_hue(labels = c("Control", "Early Water", "Late Water"))



