# State Income Tax Project 
# Tax Notes Analysis
# December 2023
# Code by: Luis Navarro 
rm(list = ls())
### Setup 
if(!require(pacman)) {install.packages("pacman")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(tidycensus)) {install.packages("tidycensus")}
if(!require(tigris)) {install.packages("tigris")}
if(!require(cowplot)) {install.packages("cowplot")}
if(!require(ggthemes)) {install.packages("ggthemes")}
library(pacman)
pacman::p_load(tidyverse,cowplot, ggthemes, 
               tidycensus, tigris)
options(scipen = 999)
path = "C:/Users/luise/OneDrive - Indiana University/Research/State_Income_Tax_Project/"
bi = paste(path,"build/input/", sep ="")
bc = paste(path,"build/code/", sep ="")
bo = paste(path,"build/output/", sep ="")
bt = paste(path,"build/temp/", sep ="")
ai = paste(path,"analysis/input/", sep ="")
ac = paste(path,"analysis/code/", sep ="")
ao = paste(path,"analysis/output/TaxNotes/", sep ="")
at = paste(path,"analysis/temp/", sep ="")
source(paste(ac, "graph_functions.R", sep = ""))
## Read the Data  
tax_data <- read_csv(file = paste(ai, "PIT_Rate_clean.csv", sep = ""))

#-------------------------------------------------------------------------------
# Compute Tax Changes
taxchanges_data <- tax_data %>% 
  ## dummy = 1 if there was a tax rate change
  mutate(taxdum = ifelse(delta_tax != 0, 1, 0), 
         taxinc = ifelse(delta_tax > 0, 1, 0), 
         taxdec = ifelse(delta_tax < 0, 1, 0)) %>%
  mutate(delta_tax_inc = delta_tax*taxinc, 
         delta_tax_dec = delta_tax*taxdec) %>% 
  arrange(state, year) 

# Table 1 and Figure 1. 
taxchanges_rate <- taxchanges_data %>% 
  filter(tax_policy != "No PIT Rate") %>% 
  ## sum all tax changes across years 
  group_by(year) %>% 
  summarize(`Average PIT Rate` = mean(taxrate, na.rm = TRUE), 
            `Number of Tax Changes` = sum(taxdum, na.rm = TRUE),
            `Number of Tax Increases` = sum(taxinc, na.rm = TRUE),
            `Number of Tax Decreases` = sum(taxdec, na.rm = TRUE)) %>% 
  mutate(`Cumulative Number of Tax Increases` = cumsum(`Number of Tax Increases`), 
         `Cumulative Number of Tax Decreases` = cumsum(`Number of Tax Decreases`))

taxchanges_deltarate <- taxchanges_data %>% 
  filter(tax_policy != "No PIT Rate") %>% 
  ## Consider only state-year that observed tax changes
  filter(delta_tax != 0) %>% 
  ## sum all tax changes across years 
  group_by(year) %>% 
  summarize(`Average Tax Increase` = mean(delta_tax_inc, na.rm = TRUE), 
            `Average Tax Decrease` = mean(delta_tax_dec, na.rm = TRUE))

taxchanges_table <- taxchanges_rate %>% 
  left_join(taxchanges_deltarate, by = "year", relationship = "one-to-one") 

taxchanges_mean <- taxchanges_table %>% 
  summarize(
    `Average PIT Rate` = mean(`Average PIT Rate`, na.rm = TRUE),                 
    `Number of Tax Changes` = sum(`Number of Tax Changes`, na.rm = TRUE),             
    `Number of Tax Increases` = sum( `Number of Tax Increases`, na.rm = TRUE),          
    `Number of Tax Decreases` = sum(`Number of Tax Decreases`, na.rm = TRUE),
    `Average Tax Increase`  = mean(`Average Tax Increase`, na.rm = TRUE),              
    `Average Tax Decrease` = mean( `Average Tax Decrease`, na.rm = TRUE)
  ) 

taxchanges_year <- taxchanges_table %>% 
  bind_rows(taxchanges_mean) 

## Table 1. Tax Changes by Year 
table1 <- taxchanges_year %>% 
  select(year, `Average PIT Rate`, 
         `Number of Tax Increases`, `Average Tax Increase`, 
         `Number of Tax Decreases`,  `Average Tax Decrease`) %>% 
  rename(Year = year) %>% 
  mutate(`Average PIT Rate` = ifelse(is.na(`Average PIT Rate`) == TRUE, round(0,2), round(`Average PIT Rate`, 2)), 
         `Average Tax Increase` = ifelse(is.na(`Average Tax Increase`) == TRUE, round(0,2), round(`Average Tax Increase`, 2)),
         `Average Tax Decrease` = ifelse(is.na(`Average Tax Decrease`) == TRUE, round(0,2), round(`Average Tax Decrease`, 2))) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Year = ifelse(is.na(Year) == TRUE, "2002-2023", Year))


write_csv(x = table1, 
          file = paste(ao, "table1.csv", sep = ""))

#-------------------------------------------------------------------------------
## Figure 1: Tax Changes by Increases and Decreases
graph1_counts <- taxchanges_year %>% 
  drop_na(year) %>% 
  select(year, 
         `Number of Tax Increases`, 
         `Number of Tax Decreases`) %>% 
  rename(`Tax Increases` = `Number of Tax Increases`) %>% 
  rename(`Tax Decreases` = `Number of Tax Decreases`) %>% 
  gather(key = "variable", value = "taxchanges", -year) %>% 
  ggplot(mapping = aes(x = year, y = taxchanges, color = variable, fill = variable, 
                       shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line() + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "", y = "Number of Tax Changes", title = "Panel A: Number of Tax Changes") +
  scale_x_continuous(breaks = 2002:2023) + scale_y_continuous(n.breaks = 10) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

graph1_cumulative <- taxchanges_year %>% 
  drop_na(year) %>% 
  select(year, 
         `Cumulative Number of Tax Increases`, 
         `Cumulative Number of Tax Decreases`) %>% 
  rename(`Tax Increases` = `Cumulative Number of Tax Increases`) %>% 
  rename(`Tax Decreases` = `Cumulative Number of Tax Decreases`) %>% 
  gather(key = "variable", value = "taxchanges", -year) %>% 
  ggplot(mapping = aes(x = year, y = taxchanges, color = variable, fill = variable, 
                       shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line() + geom_point() + 
  labs(x = "", y = "Cumulative Number of Tax Changes", title = "Panel B: Cumulative Number of Tax Changes") + 
  scale_x_continuous(breaks = 2002:2023) + scale_y_continuous(n.breaks = 10) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

graph1 <- plot_grid(format_plot(graph1_counts) + scale_color_wsj() + scale_fill_wsj(), 
                    format_plot(graph1_cumulative) + scale_color_wsj() + scale_fill_wsj(), 
                    nrow = 1)

cowplot::ggsave2(filename = paste(ao, "figure1.png", sep = ""), 
                 plot = graph1, 
                 width = 40, height = 20, units = "cm")

# Graph Aux. Tax Rate Changes 
graph_deltatax <- taxchanges_year %>% 
  drop_na(year) %>% 
  select(year, 
         `Average Tax Increase`, 
         `Average Tax Decrease`) %>% 
  mutate(`Tax Increases` = abs(`Average Tax Increase`))%>% 
  mutate(`Tax Decreases` = abs(`Average Tax Decrease`)) %>% 
  select(-c(`Average Tax Increase`, `Average Tax Decrease`)) %>% 
  gather(key = "variable", value = "taxchanges", -year) %>% 
  ggplot(mapping = aes(x = year, y = taxchanges, color = variable, fill = variable, 
                       shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line() + geom_point() + 
  #geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "", y = "Percentage Points", title = "Panel C: Average Tax Changes") +
  scale_x_continuous(breaks = 2002:2023) + scale_y_continuous(n.breaks = 10) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

format_plot(graph_deltatax) + scale_color_wsj() + scale_fill_wsj()
#-------------------------------------------------------------------------------
# Table 2. Tax Changes by State 

taxchanges_rate <- taxchanges_data %>% 
  filter(tax_policy != "No PIT Rate") %>% 
  ## sum all tax changes across years 
  group_by(state) %>% 
  summarize(`Average PIT Rate` = mean(taxrate, na.rm = TRUE), 
            `Number of Tax Changes` = sum(taxdum, na.rm = TRUE),
            `Number of Tax Increases` = sum(taxinc, na.rm = TRUE),
            `Number of Tax Decreases` = sum(taxdec, na.rm = TRUE)) %>% 
  mutate(`Cumulative Number of Tax Increases` = cumsum(`Number of Tax Increases`), 
         `Cumulative Number of Tax Decreases` = cumsum(`Number of Tax Decreases`))

taxchanges_deltarate <- taxchanges_data %>% 
  filter(tax_policy != "No PIT Rate") %>% 
  ## Consider only state-year that observed tax changes
  filter(delta_tax != 0) %>% 
  ## sum all tax changes across years 
  group_by(state) %>% 
  summarize(`Average Tax Increase` = mean(delta_tax_inc, na.rm = TRUE), 
            `Average Tax Decrease` = mean(delta_tax_dec, na.rm = TRUE))

taxchanges_table <- taxchanges_rate %>% 
  left_join(taxchanges_deltarate, by = "state", relationship = "one-to-one") 

taxchanges_state <- taxchanges_table %>% 
  bind_rows(taxchanges_mean) 

## Table 1. Tax Changes by Year 
table2 <- taxchanges_state %>% 
  select(state, `Average PIT Rate`, 
         `Number of Tax Increases`, `Average Tax Increase`, 
         `Number of Tax Decreases`,  `Average Tax Decrease`) %>% 
  rename(State = state) %>% 
  mutate(`Average PIT Rate` = ifelse(is.na(`Average PIT Rate`) == TRUE, round(0,2), round(`Average PIT Rate`, 2)), 
         `Average Tax Increase` = ifelse(is.na(`Average Tax Increase`) == TRUE, round(0,2), round(`Average Tax Increase`, 2)),
         `Average Tax Decrease` = ifelse(is.na(`Average Tax Decrease`) == TRUE, round(0,2), round(`Average Tax Decrease`, 2))) %>% 
  mutate(State = ifelse(is.na(State) == TRUE, "2002-2023", State))


write_csv(x = table2, 
          file = paste(ao, "table2.csv", sep = ""))
#-------------------------------------------------------------------------------
# Table 3. Number of States by Number of tax Change 
table3 <- taxchanges_state %>% 
  select(state, `Number of Tax Changes`) %>% 
  rename(State = state) %>% 
  mutate_at(vars(`Number of Tax Changes`), as.integer) %>% 
  group_by(`Number of Tax Changes`) %>%
  summarise(States = toString(State)) %>% 
  arrange(-`Number of Tax Changes`) %>% 
  filter(`Number of Tax Changes` > 0)

write_csv(x = table3, 
          file = paste(ao, "table3.csv", sep = ""))
#-------------------------------------------------------------------------------
# Figure 2. Average Tax Rate for tax changers 
taxrate_yeardist <- taxchanges_data %>% 
  select(year, state, tax_policy, taxrate) %>% 
  filter(tax_policy != "No PIT Rate") %>% 
  group_by(year) %>% 
  summarize(mean = mean(taxrate, na.rm = TRUE), 
            sd = sd(taxrate, na.rm = TRUE), 
            p5 = quantile(taxrate, probs = 0.05), 
            p95 = quantile(taxrate, probs = 0.95), 
            min = min(taxrate, na.rm = TRUE), 
            max = max(taxrate, na.rm = TRUE),
            p1 = quantile(taxrate, probs = 0.01), 
            p99 = quantile(taxrate, probs = 0.99)) %>% 
  mutate(sd1 = mean - sd, 
         sd2 = mean + sd) %>% 
  select(-sd) 


graph2 <- taxrate_yeardist %>%  
  ggplot(mapping = aes(x = year, y = mean)) + 
  geom_ribbon(mapping = aes(ymin = p1, ymax = p99), alpha = 0.4, fill = "lightblue1", show.legend = TRUE) + 
  geom_ribbon(mapping = aes(ymin = p5, ymax = p95), alpha = 0.4, fill = "lightblue3", show.legend = TRUE) + 
  geom_ribbon(mapping = aes(ymin = sd1,  ymax = sd2), alpha = 0.4, fill = "steelblue4", show.legend = TRUE) +
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = 2002:2023) + 
  scale_y_continuous(n.breaks = 10) +
  labs(x = "", y = "PIT Rate", title = "Trends on Top PIT Rates", subtitle = "Distribution of PIT Rates by Year")

graph2 <- format_plot(graph2) 

cowplot::ggsave2(filename = paste(ao, "figure2.png", sep = ""), 
                 plot = graph2, 
                 width = 40, height = 20, units = "cm")

#-------------------------------------------------------------------------------
# Figure 3. Graphs by Tax Policy Type 
taxplot <- function(data, main) {
  # Determine the number of unique states
  num_states <- length(unique(data$state))
  
  # Convert state to a factor to ensure consistent ordering
  data$state <- factor(data$state, levels = unique(data$state))
  
  # Create a color palette that ranges between red, green, and blue
  color_palette <- colorRampPalette(c("firebrick", "forestgreen", "blue"))(num_states)
  
  plot <- ggplot(data, aes(x = year, y = taxrate, group = state, color = state, 
                           linetype = state, shape = state)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = 1:num_states) +
    scale_linetype_manual(values = 1:num_states) +
    labs(x = "Year", y = "Tax Rate", title = main) +
    guides(color = guide_legend("state", ncol = 4), linetype = guide_legend("state"), shape = guide_legend("state"))+ 
    scale_x_continuous(breaks = 2002:2023) + scale_y_continuous(n.breaks = 10)
  
  fplot <- format_plot(plot) + theme(legend.position = "top", legend.justification = "left") +
    theme(axis.text.x = element_text(angle = 90))
  return(fplot)
}



taxincreasers_plot <- taxplot(data = tax_data %>% 
                                filter(tax_policy_seg == "Tax Increaser"), 
                              main = "PIT Rate - Tax Increasers")
taxdecreasers_plot <- taxplot(data = tax_data %>% 
                                filter(tax_policy_seg == "Tax Decreaser"), 
                              main = "PIT Rate - Tax Decreasers")
taxswitchersinc_plot <- taxplot(data = tax_data %>% 
                               filter(tax_policy_seg == "Tax Switcher - Net Increaser"), 
                             main = "PIT Rate - Tax Switchers - Net Increaser")
taxswitchersdec_plot <- taxplot(data = tax_data %>% 
                                  filter(tax_policy_seg == "Tax Switcher - Net Decreaser"), 
                                main = "PIT Rate - Tax Switchers - Net Decreaser")
notaxchange_plot <- taxplot(data = tax_data %>% 
                              filter(tax_policy_seg == "No Tax Change"), 
                            main = "PIT Rate - No Tax Change")
nopitrate_plot <- taxplot(data = tax_data %>% 
                            filter(tax_policy_seg == "No PIT Rate"), 
                          main = "PIT Rate - No PIT Rate")


cowplot::ggsave2(filename = paste(ao, "figure3_increasers.png", sep = ""), 
                 plot = taxincreasers_plot, 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "figure3_decreasers.png", sep = ""), 
                 plot = taxdecreasers_plot, 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "figure3_switchers_netinc.png", sep = ""), 
                 plot = taxswitchersinc_plot, 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "figure3_switchers_netdec.png", sep = ""), 
                 plot = taxswitchersdec_plot, 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "figure3_nopitrate.png", sep = ""), 
                 plot = nopitrate_plot, 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "figure3_notaxchange.png", sep = ""), 
                 plot = notaxchange_plot, 
                 width = 40, height = 20, units = "cm")
#-------------------------------------------------------------------------------
#- Table 4: Categorizations of States 
fips_codes <- tigris::fips_codes %>% 
  group_by(state_name) %>% 
  summarize(state_fips = first(state_code)) %>% 
  mutate(fips = as.double(state_fips)) %>% 
  rename(state = state_name)


states_categories <- tax_data %>% 
  ## sum all tax changes across years 
  group_by(state) %>% 
  summarize(tax_policy = first(tax_policy), 
            tax_policy_seg = first(tax_policy_seg)) %>%
  left_join(fips_codes, by = "state", relationship = "one-to-one")
  

table4a <- states_categories %>% 
  rename(State = state) %>% 
  group_by(tax_policy) %>% 
  summarise(States = toString(State)) 

table4b <- states_categories %>% 
  rename(State = state) %>% 
  group_by(tax_policy_seg) %>% 
  summarise(States = toString(State)) 

write_csv(x = table4a, 
          file = paste(ao, "table4_categories1.csv", sep = ""))

write_csv(x = table4b, 
          file = paste(ao, "table4_categories2.csv", sep = ""))

# Figure 4. Maps 

us_states <- tigris::states(cb = TRUE, resolution = "20m") %>%
  filter(NAME != "Puerto Rico") %>% mutate(fips = as.integer(STATEFP)) %>% 
  shift_geometry()
# data for maps 
datamaps <- left_join(us_states, states_categories, 
                      by = "fips") %>% 
            select(state, fips, tax_policy, tax_policy_seg, geometry, STUSPS)
#-------------------------------------------------------------------------------
# Draw the Maps

wong_colors <- c("#8A8A8A", "#E69F00", "#56B4E9", "#009E73", "#F0E442",  "#CC79A7", "#0072B2", "#D55E00")
# Create a 5-color palette from the Wong colors (excluding the last two colors)
wong_palette_5 <- wong_colors[1:5]
# Create a 7-color palette from the Wong colors
wong_palette_7 <- wong_colors[1:7]

mapgroups1 <- datamaps %>% 
  ggplot(mapping = aes(fill = tax_policy)) + 
  geom_sf(color = "black", lwd = 0.2, alpha = 0.8) +
  geom_sf_text(mapping = aes(label = STUSPS), size = 2.5) + 
  labs(title = "States by Type of Income Tax Policy") + guides(fill = guide_legend(ncol = 3)) + 
  scale_fill_manual(values=wong_palette_5)

mapgroups2 <- datamaps %>% 
  ggplot(mapping = aes(fill = tax_policy_seg)) + 
  geom_sf(color = "black", lwd = 0.2, alpha = 0.8) +
  geom_sf_text(mapping = aes(label = STUSPS), size = 2.5) + 
  labs(title = "States by Type of Income Tax Policy") + 
  guides(fill = guide_legend(ncol = 3)) + 
  scale_fill_manual(values=wong_palette_7)



cowplot::ggsave2(filename = paste(ao, "map1.png", sep = ""), 
                 plot = format_map(mapgroups1), 
                 width = 40, height = 20, units = "cm")

cowplot::ggsave2(filename = paste(ao, "map2.png", sep = ""), 
                 plot = format_map(mapgroups2),
                 width = 40, height = 20, units = "cm")

# -----------------------------------------------------------------------------
# Tax Increasers and Tax Decreasers in 2008 and 2009
table_great_recession <- taxchanges_data %>% 
  filter(year == 2007 | year == 2008 | year == 2009) %>% 
  filter(delta_tax != 0)

# End Script 
#------------------------------------------------------------------------------
#-----------------------------------------------------------------------------