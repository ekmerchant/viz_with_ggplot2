#Code only for visualization with ggplot2

library(tidyverse)
#The following are needed only for choropleth maps:
library(tigris)
library(sf)
library(viridis)
library(cowplot)

#Fill in path to raw data file, which should be in .csv format
ipumsdata <- "ipumsdata.csv"
ipums <- read_csv(ipumsdata) %>% filter(AGE < 90) %>%
  mutate(MARITAL = factor(ifelse(MARST %in% 2:4, 3,
                                 ifelse(MARST == 6, 0, MARST)),
                          labels = c("Single", "Married", "Divorced", "Widowed")),
         SEX = factor(SEX, labels = c("Male", "Female")))
marital_by_sex <- ipums %>% group_by(YEAR, SEX) %>% 
  count(MARITAL, wt = PERWT) %>% mutate(TOTAL = sum(n))
marital_by_sex_age <- ipums %>% filter(YEAR %in% c(1900, 1950, 2000)) %>%
  group_by(YEAR, SEX, AGE) %>% 
  count(MARITAL, wt = PERWT) %>% mutate(TOTAL = sum(n))
sex_ratio <- ipums %>% count(YEAR, STATEFIP, SEX) %>% 
  spread(SEX, n) %>% mutate(RATIO = Male/Female * 100)

#write_csv(marital_by_sex, "marital_by_sex.csv")
#write_csv(marital_by_sex_age, "marital_by_sex_age.csv")
#write_csv(sex_ratio, "sex_ratio.csv")

#marital_by_sex <- read_csv("marital_by_sex.csv")
#marital_by_sex_age <- read_csv("marital_by_sex_age.csv")
#sex_ratio <- read_csv("sex_ratio.csv")

#Stacked column graph
stacked_column <- ggplot(marital_by_sex, aes(x = YEAR, y = n, fill = MARITAL)) + 
                    geom_col() +
                    facet_grid(rows = vars(SEX)) +
                    scale_x_continuous(breaks = seq(1900, 2000, 20)) +
                    scale_y_continuous(labels = scales::comma) +
                    labs(title = "Marital Status by Sex, 1900-2000",
                          x = "Year", y = "Number of People", fill = "Marital Status") +
                    theme_minimal(base_size = 20)
saveRDS(stacked_column, "graphs/stacked_column.RDS")

#Percent column graph
percent_column <- ggplot(marital_by_sex, aes(x = YEAR, y = n, fill = MARITAL)) + 
                    geom_col(position = "fill") +
                    facet_grid(rows = vars(SEX)) +
                    scale_x_continuous(breaks = seq(1900, 2000, 20)) +
                    scale_y_continuous(labels = scales::percent) +
                    labs(title = "Marital Status by Sex, 1900-2000",
                          x = "Year", y = "Percent of People", fill = "Marital Status") +
                    theme_minimal(base_size = 20)
saveRDS(percent_column, "graphs/percent_column.RDS")

#Side-by-side column graph
side_by_side <- ggplot(marital_by_sex, aes(x = YEAR, y = n, fill = MARITAL)) + 
                  geom_col(position = "dodge") +
                  facet_grid(rows = vars(SEX)) +
                  scale_x_continuous(breaks = seq(1900, 2000, 20)) +
                  scale_y_continuous(labels = scales::comma) +
                  labs(title = "Marital Status by Sex, 1900-2000",
                        x = "Year", y = "Number of People", fill = "Marital Status") +
                  theme_minimal(base_size = 20)
saveRDS(side_by_side, "graphs/side_by_side.RDS")

#Area graph
area <- ggplot(marital_by_sex_age, aes(x = AGE, y = n, fill = MARITAL)) +
          geom_area() +
          facet_grid(rows = vars(YEAR), cols = vars(SEX)) +
          scale_y_continuous(labels = scales::comma) +
          labs(title = "Marital Status by Sex, 1900-2000",
              x = "Age", y = "Number of People", fill = "Marital") +
          theme_minimal(base_size = 40)
saveRDS(area, "graphs/area.RDS")

#Percent area graph
area_percent <- ggplot(marital_by_sex_age, aes(x = AGE, y = n, fill = MARITAL)) +
                  geom_area(position = "fill") +
                  facet_grid(rows = vars(YEAR), cols = vars(SEX)) +
                  scale_y_continuous(labels = scales::percent) +
                  labs(title = "Marital Status by Sex, 1900-2000",
                        x = "Age", y = "percent of People", fill = "Marital") +
                  theme_minimal(base_size = 40)
saveRDS(area_percent, "graphs/area_percent.RDS")

#Line graph (sex)
line_sex <- ggplot(marital_by_sex_age, aes(x = AGE, y = n/TOTAL, color = SEX)) + 
              geom_line() +
              facet_grid(rows = vars(YEAR), cols = vars(MARITAL)) +
              scale_y_continuous(labels = scales::percent) +
              labs(title = "Marital Status by Sex, 1900-2000",
                    x = "Age", y = "Percent of People", color = "Sex") +
              theme_minimal(base_size = 40)
saveRDS(line_sex, "graphs/line_sex.RDS")

#Line graph (year)
line_year <- ggplot(marital_by_sex_age, aes(x = AGE, y = n/TOTAL, color = factor(YEAR))) + 
              geom_line() +
              facet_grid(rows = vars(SEX), cols = vars(MARITAL)) +
              scale_y_continuous(labels = scales::percent) +
              labs(title = "Marital Status by Sex, 1900-2000",
                x = "Age", y = "Percent of People", color = "Year") +
              theme_minimal(base_size = 40)
saveRDS(line_year, "graphs/line_year.RDS")

#Heat map
heat_map <- ggplot(marital_by_sex, aes(x = SEX, y = MARITAL, fill = n/TOTAL)) +
              geom_tile() +
              facet_wrap(vars(YEAR)) +
              scale_fill_gradient(low = "white", high = "steelblue") +
              labs(title = "Marital Status by Sex, 1900-2000", x = "", y = "") +
              theme_minimal(base_size = 20) +
              theme(legend.position = "none")
saveRDS(heat_map, "graphs/heat_map.RDS")

#Choropleth map
states <- states(class = "sf") %>% 
  filter(!NAME %in% c("Hawaii", "Alaska")) %>%
  mutate(STATEFIP = as.numeric(STATEFP)) 
choropleth <- inner_join(states, filter(sex_ratio, YEAR %in% c(1900, 2000))) %>% 
                ggplot() +
                  geom_sf(aes(fill = RATIO)) +
                  facet_grid(rows = vars(YEAR)) +
                  scale_fill_viridis(direction = -1) +
                  theme_map(base_size = 20) +
                  coord_sf(datum = NA) +
                  labs(title = "Sex Ratio by State", fill = "M/F*100")
saveRDS(choropleth, "graphs/choropleth.RDS")

#Scatter plot (jittered)
scatter <- inner_join(states, sex_ratio) %>% 
            ggplot(aes(x = YEAR, y = RATIO, color = NAME)) +
              geom_jitter() +
              theme_minimal(base_size = 20) +
              theme(legend.position = "bottom") +
              labs(title = "Sex Ratio by State, 1900-2000",
                  x = "Year", y = "Sex Ratio", color = "State")
saveRDS(scatter, "graphs/scatter.RDS")

#Cleveland dot plot
cleveland <- inner_join(states, sex_ratio) %>% 
              ggplot(aes(x = YEAR, y = RATIO, color = NAME)) +
                geom_point(size = 2) +
                scale_x_continuous(breaks = seq(1900, 1990, 30)) +
                facet_wrap(vars(NAME)) +
                theme_minimal(base_size = 20) +
                theme(legend.position = "none") +
                labs(title = "Sex Ratio by State, 1900-2000",
                  x = "Year", y = "Sex Ratio")
saveRDS(cleveland, "graphs/cleveland.RDS")