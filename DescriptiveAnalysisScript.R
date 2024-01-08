# State Income Tax Project Code 
library(pacman)
p_load(tidyverse, magrittr, broom, tmap,     
       modelsummary,rmarkdown, lubridate, cowplot, tigris, 
       gridExtra, ggthemes, ggdark, grid, rmdformats, lmtest, fixest)

# Directory 
setwd("C:/Users/luise/OneDrive - Indiana University/Research/State_Income_Tax_Project/")
path = "C:/Users/luise/OneDrive - Indiana University/Research/State_Income_Tax_Project/"
bi = paste(path,"build/input/", sep ="")
bc = paste(path,"build/code/", sep ="")
bo = paste(path,"build/output/", sep ="")
bt = paste(path,"build/temp/", sep ="")
ai = paste(path,"analysis/input/", sep ="")
ac = paste(path,"analysis/code/", sep ="")
ao = paste(path,"analysis/output/", sep ="")
at = paste(path,"analysis/temp/", sep ="")
# Load Functions 
source(paste(ac,"state_income_tax_rfunctions.R", sep =""))
################################################################################
#```{r data, echo=FALSE, eval=TRUE}
# Load data for analysis 
# All Controls
filedata <- paste(ai,"descriptive_controls0223.csv", sep ="")
data_controls <- read.csv(filedata, header=TRUE) %>% tibble() %>% arrange(by_group=state_fips,year)
# Tax Data clean 
filedata1 <- paste(bt,"taxdataclean0223.csv", sep ="")
data_taxes <- read.csv(filedata1, header=TRUE) %>% tibble() %>% arrange(by_group=state_fips,year)
# Financial Data 
filedata2 <- paste(bt,"acfr_states.csv", sep ="")
acfr <- read.csv(filedata2, header=TRUE) %>% tibble() 

#Data from Taxsim 
filedata4 <- paste(ai, "taxsimresults.csv", sep = "")
taxsim_res <- read.csv(filedata4, header = TRUE) %>% tibble() %>% arrange(by_group=state,year) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(state = str_to_upper(state)) %>% 
  filter(mstat == "single")
# Merge Data For Graphs 
taxes_merge <- data_taxes %>% 
              left_join(acfr,relationship = "one-to-one", 
                        by = join_by(state_abbrev == state_abbrev, year == year)) %>% 
              left_join(taxsim_res, relationship = "one-to-one", 
                        by = join_by(state == state, year == year))

# State Bonds Full Sample for OLS Analysis
filedata3 <- paste(ai,"state_bonds_full.csv", sep ="")
data_bonds <- read.csv(filedata3, header=TRUE) %>% tibble() %>% arrange(by_group=state_fips,year) %>% 
  mutate(gf_surplus_rev = gf_surplus/(total_revenue*1000))  %>% 
  left_join(taxsim_res, by = join_by(state == state, year == year))


#Variable Formatting 
attr(data_bonds$taxchangedum, "label") <- "Income Tax Rate Change"
attr(data_bonds$mat_yr, "label") <- "Years to Maturity"
attr(data_bonds$rating, "label") <- "Credit Rating"
attr(data_bonds$negotiated, "label") <- "Negotiated Sale"
attr(data_bonds$salestax, "label") <- "Sales Tax Rev % Tot Rev"
attr(data_bonds$igrev, "label") <- "IG Rev % Tot Rev"
attr(data_bonds$currexp, "label") <- "Current Exp % Tot Exp"
attr(data_bonds$gdp_gr, "label") <- "GDP Growth Rate"
attr(data_bonds$gf_surplus_rev, "label") <- "GF Surplus % Tot Rev"
attr(data_bonds$own_int_exempt, "label") <- "Within-State Tax Exempt"
#```
################################################################################
#```{r taxchanges1, echo=FALSE, eval=TRUE}
# Tax Changes Graphs 
data_graph <- taxes_merge %>% group_by(state) %>% mutate(taxincrease = ifelse(taxchange>0,1,0)) %>% 
  mutate(taxdecrease = ifelse(taxchange<0,1,0)) %>% 
  mutate(taxrate = taxrate*100) %>% 
  mutate(taxchange = taxchange*100) %>% 
  filter(year > 2002) %>% 
  group_by(year) %>%
  summarize(taxrate = mean(taxrate, na.rm = TRUE), 
            `Tax Increase` = sum(taxincrease, na.rm = TRUE), 
            `Tax Decrease` = sum(taxdecrease, na.rm = TRUE),
            `Sales Tax Change` = sum(salestax_taxchangedum, na.rm = TRUE)) %>% 
  mutate(taxchangetotal = `Tax Increase` + `Tax Decrease`) %>% 
  mutate(cumultaxchanges = cumsum(taxchangetotal)) %>% 
  mutate(`Tax Increase (Cumul)` = cumsum(`Tax Increase`)) %>% 
  mutate(`Tax Decrease (Cumul)` = cumsum(`Tax Decrease`)) %>% 
  gather(key = "variable", value = "value", -year) 


cumulgraph <- data_graph %>% filter(variable == "cumultaxchanges") %>% 
  ggplot(mapping = aes(x = year, y = value, shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line(size = 1, col = "steelblue4") + geom_point(size = 2, col = "steelblue4", fill = "steelblue4", alpha = 0.5) + labs(x = "", y = "Number of Changes", title = "Cumulative Tax Rate Changes") +
  scale_x_continuous(breaks = pretty(data_graph$year, n = 21)) + scale_y_continuous(n.breaks = 10)


taxchangesall <- data_graph %>% filter(variable == "taxchangetotal") %>% 
  ggplot(mapping = aes(x = year, y = value, shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line(size = 1, col = "steelblue4") + geom_point(size = 2, col = "steelblue4", fill = "steelblue4") + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  labs(x = "", y = "Number of Changes", title = "Tax Rate Changes")+ 
  scale_x_continuous(breaks = pretty(data_graph$year, n = 21)) + scale_y_continuous(n.breaks = 10)


taxchanges <- data_graph %>% filter(variable == "Tax Increase" | variable == "Tax Decrease") %>% 
  ggplot(mapping = aes(x = year, y = value, color = variable, shape = variable, label = value, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line(size = 1) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  labs(x = "", y = "Number of Changes", title = "Tax Rate Changes")+ 
  scale_x_continuous(breaks = pretty(data_graph$year, n = 21)) + scale_y_continuous(n.breaks = 10)


cumultaxchanges <- data_graph %>% filter(variable == "Tax Increase (Cumul)" | variable == "Tax Decrease (Cumul)") %>% 
  ggplot(mapping = aes(x = year, y = value, color = variable, shape = variable, label = value, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line(size = 1) + geom_point(size = 2)  + 
  labs(x = "", y = "Number of Changes", title = "Cumulative Tax Rate Changes")+ 
  scale_x_continuous(breaks = pretty(data_graph$year, n = 21)) + scale_y_continuous(n.breaks = 10)

cumultaxchanges <- format_plot(cumultaxchanges) + theme(legend.position = c(0.28,0.94)) + scale_color_wsj() 
taxchanges <- format_plot(taxchanges) + theme(legend.position = c(0.17,0.94)) + scale_color_wsj() 
cumulgraph<- format_plot(cumulgraph) + theme(legend.position = "none") 
taxchangesall<- format_plot(taxchangesall) + theme(legend.position = "none") 
#```

#```{r taxchanges2, echo=FALSE, eval=TRUE}
# Selected States Tax Rates
data_graph <- taxes_merge %>% group_by(state, year) %>% mutate(`Income Tax Rate` = taxrate*100) %>% 
  mutate(taxchange = ifelse(is.na(taxchange) == TRUE, 0, taxchange)) %>% 
  mutate(taxchange = taxchange*100) %>% 
  filter(year >= 2002) %>% 
  select(state, year, `Income Tax Rate`, taxchange, tax_policy_type) %>% 
  filter(state == "OHIO" | state == "MONTANA"| state == "NORTH CAROLINA" |
           state == "CONNECTICUT" | state == "MINNESOTA" | state == "PENNSYLVANIA") %>% 
  arrange(by_group = state) %>% group_by(state) %>% 
  mutate(`Cumulative Tax Change` = cumsum(taxchange)) %>% 
  gather(key = "variable", value = "value", -c(state,year,tax_policy_type))

rate_increases <- data_graph %>% filter(variable == "Income Tax Rate" & tax_policy_type == "Tax Increasers") %>% 
  ggplot(mapping = aes(x = year, y = value, color = state, fill = state, shape = state)) +
  geom_line(size = 1) + geom_point(size = 2) + 
  labs(x = "", y = "", title = "Tax Increases", subtitle = "Top PIT Rate, Percent")


rate_decreases <- data_graph %>% filter(variable == "Income Tax Rate" & tax_policy_type == "Tax Decreasers") %>% 
  ggplot(mapping = aes(x = year, y = value, color = state, fill = state, shape = state)) +
  geom_line(size = 1) + geom_point(size = 2) + 
  labs(x = "", y = "", title = "Tax Decreases", subtitle = "Top PIT Rate, Percent")

cumul_rate_increases <- data_graph %>% filter(variable == "Cumulative Tax Change" & tax_policy_type == "Tax Increasers") %>% 
  ggplot(mapping = aes(x = year, y = value, color = state, fill = state, shape = state)) +
  geom_line(size = 1) + geom_point(size = 2) + 
  labs(x = "", y = "", title = "Tax Increases", subtitle = "Cumulative Change, Percentage Points") 

cumul_rate_decreases <- data_graph %>% filter(variable == "Cumulative Tax Change" & tax_policy_type == "Tax Decreasers") %>% 
  ggplot(mapping = aes(x = year, y = value, color = state, fill = state, shape = state)) +
  geom_line(size = 1) + geom_point(size = 2) + 
  labs(x = "", y = "", title = "Tax Decreases", subtitle = "Cumulative Change, Percentage Points")


rate_increases <- format_plot(rate_increases)  + scale_color_wsj() +
  scale_y_continuous(n.breaks = 7) + scale_x_continuous(breaks = pretty(data_graph$year, n = 21))

rate_decreases <- format_plot(rate_decreases)  + scale_color_wsj() +
  scale_y_continuous(n.breaks = 7) + scale_x_continuous(breaks = pretty(data_graph$year, n = 21)) 

cumul_rate_increases <- format_plot(cumul_rate_increases) + scale_color_wsj() +
  scale_y_continuous(n.breaks = 7) + scale_x_continuous(breaks = pretty(data_graph$year, n = 21))

cumul_rate_decreases <- format_plot(cumul_rate_decreases) + scale_color_wsj() +
  scale_y_continuous(n.breaks = 7) + scale_x_continuous(breaks = pretty(data_graph$year, n = 21))

legend1 <- get_legend(rate_increases)
legend2 <- get_legend(rate_decreases)

prow1 <- plot_grid(
  rate_increases + theme(legend.position = "none"), 
  cumul_rate_increases + theme(legend.position = "none"),
  align = 'vh',
  hjust = -1,
  nrow = 1)

prow2 <- plot_grid(
  rate_decreases + theme(legend.position = "none"),
  cumul_rate_decreases + theme(legend.position = "none"),
  align = 'vh',
  hjust = -1,
  nrow = 1)

increases_plot <- plot_grid(prow1, legend1, rel_widths = c(1,0.3))
decreases_plot <- plot_grid(prow2, legend2, rel_widths = c(1,0.3))
#```

#```{r effectivetaxrate, echo=FALSE, eval=TRUE}
### Effective tax Rate 
#taxchanges_groups <- data_controls %>% group_by(tax_policy_type, policygroup, year) %>% 
#  summarize(taxrate = mean(taxrate, na.rm = TRUE), 
#            fedtaxrate = mean(fedtaxrate, na.rm = TRUE), 
#            taxrate_eff = mean(taxrate_eff, na.rm = TRUE),
#            own_int_exempt = mean(own_int_exempt, na.rm = TRUE), 
#            cumul_treat = mean(cumul_treat, na.rm = TRUE)) %>% drop_na(policygroup) 
#
#graph_taxrate_eff <- taxchanges_groups %>% ggplot(mapping = aes(x = year, y = taxrate_eff, 
#                                                                color = tax_policy_type, fill = tax_policy_type, 
#                                                                shape = tax_policy_type)) + 
#  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
#            alpha = 0.3, fill = "aliceblue", inherit.aes = FALSE)+
#  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
#            alpha = 0.3, fill = "aliceblue", inherit.aes = FALSE)+
#  geom_line() + geom_point() + scale_x_continuous(breaks = pretty(data_controls$year, n = 21)) + scale_y_continuous(n.breaks = 10)+
#  labs(x = "", y = "Percentage Points", title = "Effective Tax Rate by PIT Policy Type") 
#
#graph_changes_group <- taxchanges_groups %>% filter(policygroup >= 3) %>% 
#  ggplot(mapping = aes(x = year, y = cumul_treat, color = tax_policy_type, fill = tax_policy_type, shape = tax_policy_type)) +
#  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
#            alpha = 0.3, fill = "aliceblue", inherit.aes = FALSE)+
#  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
#            alpha = 0.3, fill = "aliceblue", inherit.aes = FALSE)+
#  geom_line() + geom_point() + scale_x_continuous(breaks = pretty(data_controls$year, n = 21))+  scale_y_continuous(n.breaks = 10)+
#  labs(x = "", y = "Percentage Points", title = "Cumulative Tax Changes by PIT Policy Type")
#graph_changes_group <- format_plot(graph_changes_group) + theme(legend.position = c(0.17,0.12))
#graph_taxrate_eff <- format_plot(graph_taxrate_eff) + theme(legend.position = c(0.17,0.84))
#```

#```{r cumulativechange1, echo=FALSE, eval=TRUE}
# Trends in cumulative tax change across arms of the study 
taxrates_states <- taxes_merge %>% filter(treat_group != "Never Adopters" & treat_group != "No Tax Change") %>% 
  filter(year >= 2002) %>% 
  ggplot(mapping = aes(x = year, y = taxrate, color = state, 
                       linetype = treat_group, shape = state)) + 
  geom_line(size = 0.4) + geom_point(size = 0.5) +
  facet_wrap(~treat_group, nrow = 2, scales = "free") + 
  labs(x = "", y = "", title = "Personal Income Tax Rate by States", subtitle = "(Percentage Points)") +
  theme_few()  + 
  theme(axis.line=element_line()) +
  theme(plot.title = element_text(size = 10))+
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.77,0.2)) + 
  theme(legend.text = element_text(size = 5)) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.spacing.y = unit(-0.3, "cm")) + 
  guides(color=guide_legend(ncol=3, byrow=TRUE), shape = FALSE, linetype = FALSE) + 
  scale_y_continuous(n.breaks = 6) + scale_x_continuous(breaks = pretty(taxes_merge$year, n = 18))


cumultreat_states <- taxes_merge %>% filter(treat_group != "Never Adopters" & treat_group != "No Tax Change") %>% 
  filter(year >= 2002) %>% 
  ggplot(mapping = aes(x = year, y = cumul_treat, color = state, 
                       linetype = treat_group, shape = state)) + 
  geom_line(size = 0.4) + geom_point(size = 0.5) +
  facet_wrap(~treat_group, nrow = 2, scales = "free") + 
  labs(x = "", y = "", title = "Personal Income Tax Rate by States", subtitle = "Cumulative Percentage Point Changes") +
  theme_few()  + 
  theme(axis.line=element_line()) +
  theme(plot.title = element_text(size = 10))+
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.77,0.2)) + 
  theme(legend.text = element_text(size = 5)) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.spacing.y = unit(-0.3, "cm")) + 
  guides(color=guide_legend(ncol=3, byrow=TRUE), linetype = FALSE, shape = FALSE) + 
  scale_y_continuous(n.breaks = 6) + scale_x_continuous(breaks = pretty(taxes_merge$year, n = 18))

# Define Function To Graph all States 
states_ne <- state_graph(data = filter(taxes_merge, treat_group == "Never Adopters"), title = "PIT Rate Cumulative Change: Never Adopters")
states_nt <- state_graph(data = filter(taxes_merge, treat_group == "No Tax Change"), title = "PIT Rate Cumulative Change: No Tax Change")
states_ti <- state_graph(data = filter(taxes_merge, treat_group == "Tax Increasers"), title = "PIT Rate Cumulative Change: Tax Increase")
states_td <- state_graph(data = filter(taxes_merge, treat_group == "Tax Decreasers"), title = "PIT Rate Cumulative Change: Tax Decrease")
states_ts <- state_graph(data = filter(taxes_merge, treat_group == "Tax Switchers"), title = "PIT Rate Cumulative Change: Tax Switchers")
#```

#```{r maps1, echo=FALSE, eval=TRUE}
# Map States by Treatment Group 
map_data <- data_controls %>% group_by(state_fips, state_abbrev) %>% 
  summarize(policygroup = mean(policygroup), 
            tax_policy_type = first(tax_policy_type)) %>% 
  rename(fips = state_fips) %>% rename(abbr = state_abbrev)
# Map Tax Changes by State
map_data_changes <- taxes_merge %>% 
  filter(year >= 2002) %>% 
  filter(state_abbrev != "") %>% 
  mutate(taxchange = taxchange*100, 
         taxchangedum = ifelse(treat_group == "Never Adopters", NA, taxchangedum)) %>% 
  select(state_abbrev, year, taxchangedum, taxchange) %>% 
  arrange(by_group = state_abbrev, year) %>%
  group_by(state_abbrev) %>%
  summarize(taxchangedum = sum(taxchangedum, na.rm = FALSE), 
            taxchange = sum(taxchange, na.rm = TRUE)) %>% 
  rename(abbr = state_abbrev) %>% 
  mutate(taxchangedum = as.integer(taxchangedum))
# Data with Shape file 
us_states <- tigris::states(cb = TRUE, resolution = "20m") %>%
  filter(NAME != "Puerto Rico") %>% mutate(fips = as.integer(STATEFP)) %>% 
  shift_geometry()
# data for maps 
datamaps <- left_join(us_states, 
                      left_join(map_data, map_data_changes, by = "abbr"),  
                      by = "fips") %>% rename(state = NAME) %>% 
            mutate(taxchangedum = ifelse(taxchangedum == 0, NA, taxchangedum))

breaks_map <- datamaps$taxchangedum %>% unique() %>% sort() %>% as.vector()
# Draw the Maps 
#mapgroups <- datamaps %>% 
#  ggplot(mapping = aes(fill = tax_policy_type)) + 
#  geom_sf(color = "black", lwd = 0.2, alpha = 0.4) +
#  geom_sf_text(mapping = aes(label = STUSPS), size = 2.5) + 
#  labs(title = "States by Type of Income Tax Policy") + 
#  theme_map() + scale_fill_colorblind() +
#  theme(legend.position = "right")  +  
#  theme(legend.title = element_blank()) + 
#  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0)) + 
#  theme(legend.text = element_text(size = 13, face = "plain", hjust = 0))
#
#mapchanges <- datamaps %>% 
#  ggplot(mapping = aes(fill = taxchangedum)) + 
#  geom_sf(color = "black", lwd = 0.2, alpha = 0.4) +
#  geom_sf_text(mapping = aes(label = STUSPS), size = 2.5) + 
#  labs(title = "States by Type of Income Tax Policy") + 
#  theme_map() +
#  scale_fill_gradient2_tableau(breaks = breaks_map,
#                               palette = "Sunset-Sunrise Diverging", na.value = "grey80",
#                               guide = guide_legend(label.position = "right"))+
#  theme(legend.position = "right")  +  
#  theme(legend.title = element_blank()) + 
#  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0)) + 
#  theme(legend.text = element_text(size = 13, face = "plain", hjust = 0))                   

# map for slide 
mapgroupsjoin <- tm_shape(datamaps) + 
  tm_polygons("tax_policy_type", alpha = 0.5, palette = c("gray75", "steelblue1", "forestgreen", "maroon3", "orange2"), 
              legend.is.portrait = TRUE, title = "Tax Policy Type", legend.hist = TRUE) + 
  tm_borders(col = "black") +
  tm_text(text = "STUSPS", size = 1.4, col = "black") + 
  tm_layout(frame = FALSE,
            main.title = "States by PIT Policy and Number of Tax Changes (2002-2023)", 
            main.title.position = "center", 
            main.title.fontface = "bold",
            main.title.size = 2,
            legend.show = TRUE, 
            legend.outside = TRUE, 
            legend.text.size = 1.8, 
            legend.hist.width = 1, 
            legend.hist.height = 0.5,
            legend.hist.size = 1.2,
            legend.title.size = 2, 
            legend.title.fontface = "bold") + 
  tm_shape(datamaps)  + 
  tm_text(text = "taxchangedum", size = 1.2, col = "navy", auto.placement = 0.12)
tmap_save(filename = paste(ao, "statesmap.pdf", sep = ""), 
          tm = mapgroupsjoin, 
          width = 40, height = 20, units = "cm")

#```

#```{r salestax, echo=FALSE, eval=TRUE}
# Graph Income and Sales Tax Rate
taxrates_groups <- data_controls %>% filter(year >= 2002) %>% 
  mutate(taxrate = 100*taxrate) %>% 
  mutate(cumul_treat = 100*cumul_treat) %>% 
  group_by(tax_policy_type, year) %>% 
  summarize(`Income Tax Rate`         = stats::weighted.mean(taxrate, na.rm = TRUE),
            `Sales Tax Rate`          = stats::weighted.mean(sales_tax_rate, na.rm = TRUE),
            `Sales Tax Rate (Cumul)`  = stats::weighted.mean(salestax_cumultreat, na.rm = TRUE), 
            `Income Tax Rate (Cumul)` = stats::weighted.mean(cumul_treat, na.rm = TRUE)) %>%  drop_na(tax_policy_type)%>% 
  select(year, tax_policy_type, `Income Tax Rate`, `Sales Tax Rate`, `Sales Tax Rate (Cumul)`, `Income Tax Rate (Cumul)`) %>% 
  arrange(by_group = year) %>% arrange(by_group = tax_policy_type) %>% 
  fill(`Sales Tax Rate`, .direction = "down") %>% fill(`Sales Tax Rate (Cumul)`, .direction = "down") %>% ## Carryforwards 
  gather(key = "variable", value = "value", -c(year,tax_policy_type)) %>% 
  filter(tax_policy_type != "")


taxrates_graph <- taxrates_groups %>% filter(variable == "Income Tax Rate (Cumul)" | variable == "Sales Tax Rate (Cumul)") %>% 
  ggplot(mapping = aes(x = year, y = value, shape = variable, 
                       linetype = variable, fill = variable, color = variable )) +
  geom_line(size = 1.2) + geom_point(size = 1.3) + facet_wrap(~ factor(tax_policy_type,c("Tax Increasers", "Tax Decreasers", "Tax Switchers", 
                                                                             "No Tax Change", "Never Adopters")), scales = "free") +
  scale_x_continuous(breaks = pretty(taxrates_groups$year, n = 18)) + scale_y_continuous(n.breaks = 6) +
  labs(x = "", y = "Percentage Points", title = "State Sales and Personal Income Tax", subtitle = "Cumulative Tax Rate Changes (Percentage Points)")+
  guides(linetype = FALSE) + geom_hline(yintercept = 0, color = "gray") 


taxrates_graph <- format_plot(taxrates_graph) + 
                  theme(legend.position = c(0.85,0.20)) + 
                  scale_color_wsj() 
#```

#```{r fiscalstructure, echo=FALSE, eval=TRUE}
# Fiscal Structure Graphs 
fistr_data <- data_controls %>% mutate(`Sales Tax` = tot_sales___gr_rec_tax/total_revenue) %>% 
  mutate(`Income Tax` = total_income_taxes/total_revenue) %>% 
  mutate(`Personal Income Tax` = individual_income_tax/total_revenue) %>% 
  mutate(`IG Revenues` = total_fed_ig_revenue/total_revenue) %>% 
  mutate(Others = 1 - `Sales Tax` - `Income Tax` - `IG Revenues`) %>% 
  group_by(tax_policy_type,yr5) %>% 
  summarize(`Sales Tax` = mean(`Sales Tax`, na.rm = TRUE),
            `Income Tax` = mean(`Income Tax`, na.rm = TRUE),
            `Personal Income Tax` = mean(`Personal Income Tax`, na.rm = TRUE),
            `IG Revenues` = mean(`IG Revenues`, na.rm = TRUE), 
            Others = mean(Others, na.rm = TRUE)) %>% 
  filter(tax_policy_type != "") %>% 
  gather(key = "Variable", value = "value", -c(tax_policy_type,yr5)) 

fistr_mean <- data_controls %>% mutate(`Sales Tax` = tot_sales___gr_rec_tax/total_revenue) %>% 
  mutate(`Income Tax` = total_income_taxes/total_revenue) %>% 
  mutate(`Personal Income Tax` = individual_income_tax/total_revenue) %>% 
  mutate(`IG Revenues` = total_fed_ig_revenue/total_revenue) %>% 
  mutate(Others = 1 - `Sales Tax` - `Income Tax` - `IG Revenues`) %>% 
  group_by(tax_policy_type) %>% 
  summarize(`Sales Tax` = mean(`Sales Tax`, na.rm = TRUE),
            `Income Tax` = mean(`Income Tax`, na.rm = TRUE),
            `Personal Income Tax` = mean(`Personal Income Tax`, na.rm = TRUE),
            `IG Revenues` = mean(`IG Revenues`, na.rm = TRUE), 
            Others = mean(Others, na.rm = TRUE)) %>% 
  filter(tax_policy_type != "") %>% 
  gather(key = "Variable", value = "value", -c(tax_policy_type)) 


# Bar Graph Time by Income Tax Policy Type 
bargraph <- fistr_mean %>% mutate(value = round(100*value, 2)) %>% 
  ggplot(mapping = aes(x = factor(Variable, levels = c("Others","IG Revenues", "Sales Tax","Personal Income Tax","Income Tax")), 
                       y = value,
                       fill = factor(tax_policy_type, 
                                     levels = c("Tax Increasers", "Tax Decreasers", 
                                                "Tax Switchers", "No Tax Change", "Never Adopters")),
                       label = value)) +
  geom_bar(stat = "identity", alpha = 0.40, na.rm = TRUE) + 
  geom_text(aes(label=value), vjust=0, hjust =1.2, size=5.5)+
  facet_wrap(~factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", 
                                                 "Tax Switchers", "No Tax Change", "Never Adopters")), scales = "free_x") + 
  coord_flip() + 
  labs(x ="", y ="% Total Revenues", title = "Revenue Structure by PIT Policy Type", subtitle = "Average 2003-2021") + theme(legend.title = element_blank()) + 
  scale_y_continuous(n.breaks = 6) + theme(plot.subtitle = element_text(face = "italic")) 




bargraphyr <- fistr_data %>% filter(Variable != "Personal Income Tax") %>% 
  ggplot(mapping = aes(x= yr5, 
                       y = value, fill = factor(Variable, levels = c("Others","IG Revenues", "Sales Tax","Income Tax")))) +
  geom_bar(stat = "identity", alpha = 0.40) +
  facet_wrap(~factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters"))) + 
  labs(x ="", y ="% Total Revenues", title = "Revenue Structure", subtitle = "Average 2003-2021") + theme(legend.title = element_blank()) + 
  scale_y_continuous(n.breaks = 10)  


bargraph <- format_plot(bargraph) + theme(axis.text.x = element_text(angle = 0)) +
            theme(legend.position = c(0.85,0.20)) + 
            scale_color_wsj() 

bargraphyr <- format_plot(bargraphyr) + 
              theme(legend.position = c(0.8,0.1)) +
              scale_color_wsj() 
#```

#```{r republican, echo = FALSE, eval = TRUE}
# Extra Graphs Republican Status 
fistr_datarep <- data_controls %>% mutate(`Sales Tax` = tot_sales___gr_rec_tax/total_revenue) %>% 
  mutate(`Income Tax` = total_income_taxes/total_revenue) %>% 
  mutate(`Personal Income Tax` = individual_income_tax/total_revenue) %>% 
  mutate(`IG Revenues` = total_fed_ig_revenue/total_revenue) %>% 
  mutate(Others = 1 - `Sales Tax` - `Income Tax` - `IG Revenues`) %>% 
  group_by(tax_policy_type) %>% 
  summarize(`Real GDP` = mean(real_gdp, na.rm = TRUE),
            `Republican Governor` = mean(rep_gov, na.rm = TRUE),
            `Republican House` = mean(rep_house, na.rm = TRUE),
            `Republican Senate` = mean(rep_senate, na.rm = TRUE)) %>%  
  filter(tax_policy_type != "") %>% 
  gather(key = "Variable", value = "value", -c(tax_policy_type)) 


rep_graph <- fistr_datarep %>% filter(Variable != "Real GDP") %>% 
  mutate(value = round(100*value, 2)) %>% 
  ggplot(mapping = aes(x= Variable, 
                       y = value, 
                       fill = factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")), 
                       label = value)) +
  geom_bar(stat = "identity", alpha = 0.40) +
  geom_text(aes(label=value), vjust=0, hjust =1.2, size=5.5)+
  facet_wrap(~ factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")), scales = "free_x") +
  coord_flip() + 
  labs(x ="", y ="Percentage of Republican", title = "Political Affiliation by PIT Policy Type", subtitle = "Percentage Republican Government, 2003-2021") + 
  scale_y_continuous(n.breaks = 6)

rep_graph <- format_plot(rep_graph) + theme(axis.text.x = element_text(angle = 0)) +
             theme(legend.position = c(0.85,0.20)) + 
             scale_color_wsj() 
#```

#```{r creditrating, echo = FALSE, eval = TRUE}
# Credit Rating Distribution  
rating_dist <- data_controls %>% filter(state != "DISTRICT OF COLUMBIA") %>% 
  filter(sp != "NR") %>% 
  select(sp, state, year, tax_policy_type) %>% 
  group_by(tax_policy_type, sp) %>% 
  summarize(states = n())

statesgroup <- data_controls %>% filter(state != "DISTRICT OF COLUMBIA") %>% 
  filter(year == 2020) %>% 
  group_by(tax_policy_type) %>% 
  summarize(n = n()) %>% 
  mutate(n = n*(data_controls$year %>% unique() %>% length()))

ratings <- data_controls %>% filter(sp != "NR" | sp == "") %>% select(sp) %>% unique() %>% 
  mutate(indx = case_when(sp == "AAA" ~ 1, 
                          sp == "AA+" ~ 2,
                          sp == "AA" ~ 3,
                          sp == "AA-" ~ 4,
                          sp == "A+" ~ 5,
                          sp == "A" ~ 6,
                          sp == "A-" ~ 7,
                          sp == "BBB+" ~ 8,
                          sp == "BBB" ~ 9,
                          sp == "BBB-" ~ 10)) %>% arrange(indx)

policies <- cross_join(tibble(indx = seq(1:10)), data_controls %>% select(tax_policy_type) %>% unique()) %>% filter(tax_policy_type != "") %>% arrange(tax_policy_type)

template <- left_join(policies, ratings, by = "indx") 

data_rating_plot <- left_join(template, rating_dist, by = c("tax_policy_type", "sp")) %>% 
  right_join(statesgroup, by = "tax_policy_type") %>% 
  mutate(states = ifelse(is.na(states) == TRUE, 0, states)) %>% 
  mutate(percent = 100*states/n)

rating_graph <- data_rating_plot %>% ggplot(mapping = aes(x= reorder(sp, -indx), y = percent, 
                                                          fill = factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")))) +
  geom_bar(stat = "identity", alpha = 0.40) +
  facet_wrap(~ factor(tax_policy_type, 
                      levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")), scales = "free") +
  coord_flip() + 
  labs(x ="", y ="Percent of State-Year Sample", title = "Credit Rating by PIT Policy Type", subtitle = "Percentage of State-Year Sample (2003 -2021)") +
  scale_y_continuous(n.breaks = 6)


rating_graph <- format_plot(rating_graph) + theme(axis.text.x = element_text(angle = 0)) +
                theme(legend.position = c(0.85,0.20)) + 
                scale_color_wsj() 
  
#```

#```{r allrates1, echo = FALSE, eval = TRUE}
# taxsim
marginalrate <- taxes_merge %>% group_by(tax_policy_type, year) %>% 
  filter(year >= 2003 & year <= 2021) %>% 
  filter(state != "DISTRICT OF COLUMBIA") %>% 
  filter(tax_policy_type != "Never Adopters") %>% 
  summarize(srate = weighted.mean(srate, employment_level)) %>% 
  ggplot(mapping = aes(x = year, y = srate, color = tax_policy_type, 
                       shape = tax_policy_type, linetype = tax_policy_type)) + 
  geom_line(size = 1.2) + geom_point(size = 1.3) +
  labs(x ="", y ="Marginal Income Tax Rate", title = "Marginal Income Tax Rate (Weighted Average by Employment Level)") 

marginalrate <- format_plot(marginalrate) + theme_few() + scale_color_wsj() + 
  theme(legend.position = c(0.8, 0.93)) + theme(axis.text.x = element_text(angle=0)) + 
  theme(legend.title = element_blank()) + scale_x_continuous(n.breaks = 20) + scale_y_continuous(n.breaks = 10)+
  guides(color = guide_legend(nrow = 2)) + theme(legend.background = element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.7))


allratesdata <- taxes_merge %>% filter(year > 2003 & year <= 2021) %>% 
  filter(state != "DISTRICT OF COLUMBIA") %>% 
  filter(tax_policy_type != "Never Adopters") %>%
  mutate(taxrate = taxrate*100) %>% 
  group_by(tax_policy_type, year) %>% 
  summarize(`Top PIT Rate`  = mean(taxrate, na.rm = TRUE), 
            `Marginal PIT Rate`  = weighted.mean(srate, employment_level, na.rm = TRUE), 
            `Effective PIT Rate`  = weighted.mean(effrate, employment_level, na.rm = TRUE)) %>% 
  gather(key = "variable", value = "value", - c(year, tax_policy_type)) 


allrates2 <-  allratesdata %>% ggplot(mapping = aes(x = year, y = value, color = variable, shape = variable, linetype = variable)) + 
  geom_rect(xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_rect(xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
            alpha = 0.5, fill = "lightblue1", inherit.aes = FALSE)+
  geom_line(size = 1.2) + geom_point(size = 1.3) +
  labs(x ="", y ="Percentage Points", title = "State Income Tax Rates") +
  facet_wrap(~factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")), scales = "free") +
  scale_x_continuous(n.breaks = 15) + scale_y_continuous(n.breaks = 6)

allrates2 <- format_plot(allrates2) + 
              scale_colour_wsj() +             
              theme(legend.position = "top", legend.justification = "left", legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), legend.title = element_blank()) 


# marginal rates are weighted by employment level 
cowplot::ggsave2(filename = paste(ao, "allrates.pdf", sep =""), plot = allrates2, width = 40, height = 20, units = "cm")
#```

#```{r allrates2, echo = FALSE, eval = TRUE}
marginalrate_comp <- taxes_merge %>% filter(year > 2003 & year <= 2021) %>% 
  filter(state != "DISTRICT OF COLUMBIA") %>% 
  #filter(tax_policy_type != "Never Adopters") %>%
  mutate(taxrate = taxrate*100) %>% 
  group_by(tax_policy_type, year) %>% 
  summarize(`Top Income Tax Rate`  = mean(taxrate, na.rm = TRUE), 
            `Marginal Income Tax Rate (Observed MHI)`  = weighted.mean(srate, employment_level, na.rm = TRUE)) %>% 
  gather(key = "variable", value = "value", - c(year, tax_policy_type)) %>% 
  ggplot(mapping = aes(x = year, y = value, color = variable, shape = variable)) + 
  geom_line(size = 1.2) + geom_point(size = 1.3) +
  labs(x ="", y ="Marginal Income Tax Rate", title = "Marginal Income Tax Rate (Weighted Average by Employment Level)") + 
  facet_wrap(~factor(tax_policy_type, levels = c("Tax Increasers", "Tax Decreasers", "Tax Switchers", "No Tax Change", "Never Adopters")), scales = "free") 

marginalrate_comp <- format_plot(marginalrate_comp) +
  scale_color_wsj()  + 
  guides(color = guide_legend(nrow = 2)) + theme(legend.background = element_blank()) 
#```

scatterdata <- taxes_merge %>% filter(year > 2003 & year <= 2021) %>% 
  filter(state != "DISTRICT OF COLUMBIA") %>% 
  filter(tax_policy_type != "Never Adopters" & tax_policy_type != "No Tax Change") %>%
  mutate(taxrate = 100*taxrate) %>% 
  group_by(tax_policy_type, year) %>% 
  summarize(`Top Income Tax Rate`  = mean(taxrate, na.rm = TRUE), 
            `Marginal Income Tax Rate`  = weighted.mean(srate, employment_level, na.rm = TRUE), 
            `Effective Income Tax Rate`  = weighted.mean(effrate, employment_level, na.rm = TRUE)) 


scatter_all <- scatterdata %>% 
               gather(key = "variable", value = "value", -c(year, tax_policy_type, `Top Income Tax Rate`)) %>% 
               ggplot(mapping = aes(x = `Top Income Tax Rate`, 
                                    y = value, 
                                    color = tax_policy_type, shape = tax_policy_type, fill = tax_policy_type)) + 
               geom_point(size = 2.5) + geom_smooth(method = "lm", se = TRUE, size = 1) + 
               labs(x ="Top PIT Rate", y ="Marginal/Effective PIT Rate", title = "Marginal and Effective Tax Rate by PIT Reform") +
               facet_wrap(~variable, scales = "free") + 
               scale_x_continuous(n.breaks = 10) + scale_y_continuous(n.breaks = 10)
  
  
scatter_all <- format_plot(scatter_all) + scale_color_wsj() + scale_fill_wsj() + theme(legend.position = "top", legend.justification = "left")
  

scatter_taxchanges1 <- scatterdata %>%   
  ggplot(mapping = aes(x = `Top Income Tax Rate`, 
                       y = `Marginal Income Tax Rate`, 
                       color = tax_policy_type, shape = tax_policy_type)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x ="Top PIT Rate", y ="Marginal PIT Rate", title = "PIT Reforms and Marignal PIT Rates") +
  facet_wrap(~tax_policy_type, scales = "free") + scale_color_wsj() 


scatter_taxchanges2 <- scatterdata %>%   
  ggplot(mapping = aes(x = `Top Income Tax Rate`, 
                       y = `Effective Income Tax Rate`, 
                       color = tax_policy_type, shape = tax_policy_type)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x ="Top PIT Rate", y ="Effective PIT Rate", title = "PIT Reforms and Effective PIT Rates") +
  facet_wrap(~tax_policy_type, scales = "free") + scale_color_wsj() 


scatter_legend <- get_legend(scatter_taxchanges1)
scatter_taxchangescomb <- plot_grid(plot_grid(format_plot(scatter_taxchanges1) + theme(legend.position = "none"), 
                                              format_plot(scatter_taxchanges2) + theme(legend.position = "none"),
                                              nrow = 2))


# Export all graphs 

# 1 Pit Reforms Number of tax changes 
ggsave(filename = paste(ao, "taxchanges.pdf", sep = ""), 
       plot = plot_grid(taxchangesall, cumulgraph), 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

# Cumulative Tax Changes 
ggsave(filename = paste(ao, "cumultaxchanges.pdf", sep = ""), 
       plot = plot_grid(taxchanges, cumultaxchanges), 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 


# Increases and Decreases 
ggsave(filename = paste(ao, "taxincreasesdecreases.pdf", sep = ""), 
       plot = plot_grid(increases_plot, decreases_plot, nrow = 2), 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

# Maps 
tmap_save(filename = paste(ao, "statesmap.pdf", sep = ""), 
       tm = mapgroupsjoin, 
       width = 40, height = 20, units = "cm")

# Tax rates graph
ggsave(filename = paste(ao, "taxrates_graph.pdf", sep = ""), 
       plot = taxrates_graph, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

# Tax rates graph
ggsave(filename = paste(ao, "rep_graph.pdf", sep = ""), 
       plot = rep_graph, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

# Fiscal Revenes 
ggsave(filename = paste(ao, "revenue_structure.pdf", sep = ""), 
       plot = bargraph, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 


# Credit Rating 
ggsave(filename = paste(ao, "rating_graph.pdf", sep = ""), 
       plot = rating_graph, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

# All Rates 
ggsave(filename = paste(ao, "allrates.pdf", sep = ""), 
       plot = allrates2, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 

ggsave(filename = paste(ao, "scatterPITrates.pdf", sep = ""), 
       plot = scatter_all, 
       width = 40, height = 20, units = "cm", dpi = 500, 
       device = "pdf") 
##############################################################################
#```{r regression1, echo = FALSE, eval = TRUE}
# Define the Bond Sample To Include Only Bonds from 2005 to 2022 
data_bonds_all <- data_bonds %>% filter(year >= 2002 & year <= 2022) %>% 
  drop_na(taxrate) %>% 
  drop_na(treat_group) %>% 
  filter(treat_group != "1") %>% 
  select(-c(experiment1, experiment2)) %>% 
  ## Manual Replacement of Experiments: 
  mutate(experiment1 = ifelse(treat_group == "Never Adopters" |treat_group == "Tax Increasers", 1, 0)) %>% 
  mutate(experiment2 = ifelse(treat_group == "Never Adopters" |treat_group == "Tax Decreasers", 1, 0)) %>% 
  ## Remove TENNESSEE and NEW HAMPSHIRE from the analysis 
  mutate(experiment1 = ifelse(state == "TENNESSEE" | state == "NEW HAMPSHIRE", 0, experiment1)) %>% 
  mutate(experiment2 = ifelse(state == "TENNESSEE" | state == "NEW HAMPSHIRE", 0, experiment2)) %>% 
  mutate(experiment0 = 1) %>% 
  mutate(mat_yr = mat_yr %>% as.numeric()) %>%
  mutate(salestax = salestax %>% as.numeric()) %>% 
  mutate(igrev = igrev %>% as.numeric()) %>%
  mutate(currexp = currexp %>% as.numeric()) %>%
  mutate(salestax = salestax/100) %>% 
  mutate(igrev = igrev/100) %>% 
  mutate(currexp = currexp/100) %>% 
  mutate(go_series = ifelse(SeriesSecurity == "G.O." | 
                              SeriesSecurity == "Lt G.O" | 
                              SeriesSecurity == "Ult G.O.", 1, 0)) %>% 
  mutate(gobond = gobond %>% as.numeric()) %>% 
  mutate(revbond = ifelse(SeriesSecurity == "Revenue", 1, 0)) %>% 
  mutate(date = lubridate::parse_date_time(date, "dmy")) %>% 
  mutate(qofd = lubridate::quarter(date, with_year = TRUE)) 

# Split the sample in GO and Rev Bonds (Both Long Term)
data_bonds_golt <- data_bonds_all %>% filter(gobond == 1 & short_term == 0)
data_bonds_relt <- data_bonds_all %>% filter(revbond == 1 & short_term == 0)

# Note: Own Interest Exemption get subsummed by the state fixed effects 
models_full <- ols_bond_models(data_bonds_all)
models_golt <- ols_bond_models(data_bonds_golt)
models_relt <- ols_bond_models(data_bonds_relt)

coefplot_full <- modelplot(modellist(models_full), coef_omit = "^(?!taxchangedum)", coef_rename = "Income Tax Rate") + 
  geom_vline(xintercept = 0) + theme_few() + theme(legend.title = element_blank()) + labs(title = "Full Bond Sample", x = "", y = "") + 
  theme(plot.title = element_text(size = 8)) + scale_x_continuous(n.breaks = 8) + theme(axis.text.x = element_text(size = 8, angle = 0)) + 
  theme(legend.text = element_text(size = 10))

coefplot_golt <- modelplot(modellist(models_golt), coef_omit = "^(?!taxchangedum)", coef_rename = "Income Tax Rate") + 
  geom_vline(xintercept = 0) + theme_few() + theme(legend.title = element_blank()) + labs(title = "GO Bonds (Long Term)", x = "", y = "") + 
  theme(plot.title = element_text(size = 8)) + scale_x_continuous(n.breaks = 8) + theme(axis.text.x = element_text(size = 8, angle = 0)) + 
  theme(legend.text = element_text(size = 10))

coefplot_relt <- modelplot(modellist(models_relt), coef_omit = "^(?!taxchangedum)", coef_rename = "Income Tax Rate") + 
  geom_vline(xintercept = 0) + theme_few() + theme(legend.title = element_blank()) + labs(title = "Revenue Bonds (Long Term)", x = "", y = "") + 
  theme(plot.title = element_text(size = 8)) + scale_x_continuous(n.breaks = 8) + theme(axis.text.x = element_text(size = 8, angle = 0)) + 
  theme(legend.text = element_text(size = 10))

legendcoef <- get_legend(coefplot_full)

ols_coefplot <- plot_grid(coefplot_full + theme(legend.position = "none"), 
                          coefplot_golt + theme(legend.position = "none"), 
                          coefplot_relt + theme(legend.position = "none"), 
                          legendcoef, nrow = 2)

modsum1 <- modelsummary(list("m1" = modellist(models_full), 
                  "m2" = modellist(models_golt), 
                  "m3" = modellist(models_relt)), coef_omit = "^(?!taxchangedum)", coef_rename = "Income Tax Rate", output = "kableExtra", gof_omit = "^(?!.*Obs)", stars = TRUE, shape = "rbind") %>% kable_styling()  


### All Models 
all_models <- ols_bond_models_all(data_bonds_all)
all_models_golt <- ols_bond_models_all(data_bonds_golt)
all_models_relt <- ols_bond_models_all(data_bonds_relt)

modsumall <- modellistall(all_models) %>% modelsummary(coef_omit = "^(?!(taxrate|srate|effrate))", 
                                                             gof_omit = "^(?!.*Obs)", 
                                                             stars = TRUE, shape = "rbind") %>% kable_styling() 

modsumgolt <- modellistall(all_models_golt) %>% modelsummary(coef_omit = "^(?!(taxrate|srate|effrate))", 
                                                             gof_omit = "^(?!.*Obs)", 
                                                             stars = TRUE, shape = "rbind") %>% kable_styling() 

modsumrelt <- modellistall(all_models_relt) %>% modelsummary(coef_omit = "^(?!(taxrate|srate|effrate))", 
                                               gof_omit = "^(?!.*Obs)", 
                                               stars = TRUE, shape = "rbind") %>% kable_styling() 
# Main Table 
panela<- regtable(data = data_bonds_all, title =  "Panel A: Full Sample")
panelb<- regtable(data = data_bonds_golt, title = "Panel B: GO Bonds LT")
panelc<- regtable(data = data_bonds_relt, title = "Panel C: Rev Bonds LT")
allpanels <- rbind(panela, panelb, panelc)
names(allpanels) <- c("Variable", "All", "Tax Increases", "Tax Decreases")
ols_panel_coeftable_tex <- allpanels %>% latex_table() %>% row_spec(c(1,5,6,10,11), hline_after = T)

#Format the tables
panelac<- regtable_covs(data = data_bonds_all, title =  "Panel A: Full Sample") %>% latex_table() %>% row_spec(1, hline_after = T)
panelbc<- regtable_covs(data = data_bonds_golt, title = "Panel B: GO Bonds LT") %>% latex_table() %>% row_spec(1, hline_after = T)
panelcc<- regtable_covs(data = data_bonds_relt, title = "Panel C: Rev Bonds LT") %>% latex_table() %>% row_spec(1, hline_after = T)
#```