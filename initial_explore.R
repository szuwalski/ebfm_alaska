#==catches in AK

dat<-read.csv("data/FOSS_landings_deflated.csv")
library(dplyr)
library(stringr)
library(ggplot2)
dat <- dat %>%
  mutate(
    NMFS_group = case_when(
      str_detect(NMFS.Name, regex("salmon", ignore_case = TRUE)) ~ "SALMON",
      str_detect(NMFS.Name, regex("crab", ignore_case = TRUE)) ~ "CRAB",
      str_detect(NMFS.Name, regex("rockfish", ignore_case = TRUE)) ~ "ROCKFISH",
      str_detect(NMFS.Name, regex("halibut", ignore_case = TRUE)) ~ "HALIBUT",
      str_detect(NMFS.Name, regex("sole", ignore_case = TRUE)) ~ "SOLE",
      str_detect(NMFS.Name, regex("shrimp", ignore_case = TRUE)) ~ "SHRIMP",
      TRUE ~ NMFS.Name
    )
  )


#==volume
top10 <- dat %>%
  group_by(NMFS_group) %>%
  summarise(volume = sum(Metric.Tons, na.rm = TRUE)) %>%
  arrange(desc(volume)) %>%
  slice_head(n = 10) %>%
  pull(NMFS_group)

dat %>%
  filter(NMFS_group %in% top10) %>%
  group_by(Year, NMFS_group) %>%
  summarise(volume = sum(Metric.Tons, na.rm = TRUE)) %>%
  ggplot(aes(Year, volume, fill = NMFS_group)) +
  geom_col() +
  theme_bw()

#==value
top10 <- dat %>%
  group_by(NMFS_group) %>%
  summarise(value = sum(Dollars, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  slice_head(n = 10) %>%
  pull(NMFS_group)

dat %>%
  filter(NMFS_group %in% top10) %>%
  group_by(Year, NMFS_group) %>%
  summarise(value = sum(Dollars , na.rm = TRUE)) %>%
  ggplot(aes(Year, value, fill = NMFS_group)) +
  geom_col() +
  theme_bw()

top10 <- dat %>%
  group_by(NMFS_group) %>%
  summarise(value = sum(Deflated.Dollars, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  slice_head(n = 10) %>%
  pull(NMFS_group)

dat %>%
  filter(NMFS_group %in% top10) %>%
  group_by(Year, NMFS_group) %>%
  summarise(value = sum(Deflated.Dollars , na.rm = TRUE)) %>%
  ggplot(aes(Year, value, fill = NMFS_group)) +
  geom_col() +
  theme_bw()

top10 <- dat %>%
  group_by(NMFS.Name) %>%
  summarise(value = sum(Deflated.Dollars, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  slice_head(n = 10) %>%
  pull(NMFS.Name)

dat %>%
  filter(NMFS.Name %in% top10) %>%
  group_by(Year, NMFS.Name) %>%
  summarise(value = sum(Deflated.Dollars , na.rm = TRUE)) %>%
  ggplot(aes(Year, value, fill = NMFS.Name)) +
  geom_col() +
  theme_bw()

#==========================
# managing body
dat <- dat %>%
  mutate(
    management_body = case_when(
      
      str_detect(NMFS_group, "SALMON") ~ "State of Alaska",
      str_detect(NMFS_group, "HERRING") ~ "State of Alaska",
      
      str_detect(NMFS_group, "HALIBUT") ~ "IPHC",
      
      str_detect(NMFS_group, "CRAB") ~ "State of Alaska",
      
      str_detect(NMFS_group, "POLLOCK") ~ "NPFMC",
      str_detect(NMFS_group, "COD") ~ "NPFMC",
      str_detect(NMFS_group, "ROCKFISH") ~ "NPFMC",
      str_detect(NMFS_group, "SABLEFISH") ~ "NPFMC",
      str_detect(NMFS_group, "SOLE") ~ "NPFMC",
      str_detect(NMFS_group, "MACKEREL") ~ "NPFMC",
      
      TRUE ~ "Other"
    )
  )

#==change in value 'controlled' over time
dat %>%
  group_by(Year, management_body) %>%
  summarise(value = sum(Deflated.Dollars, na.rm = TRUE)) %>%
  ggplot(aes(Year, value, fill = management_body)) +
  geom_area(position = "stack") +
  theme_bw()

