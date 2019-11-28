#### Install packages ####
# install.packages(c("devtools", "readxl", "ape", "rgdal", "GISTools", "ggalt", "broom", "tidyverse", "FSA",
#                     "cowplot", "impute", "preprocessCore", "Go.db", "AnnotationDbi", "fingertipsR"),
#                  dependencies = TRUE)

BiocManager::install("pkimes/sigclust2")
devtools::install_github("nolanlab/Rclusterpp")
devtools::install_version("rgdal", 
                          version = "1.3-9",
                          repos = "http://cran.us.r-project.org")

#### load packages ####
library(sigclust2)        # For clustering
library(devtools)         # For installing packages
library(readxl)           # For reading excel files
library(ape)              # For plotting dendrograms
library(rgdal)            # For mapping
library(GISTools)         # For mapping
library(ggalt)            # For mapping
library(broom)            # For tidying map data
library(tidyverse)        # For data manipulation
library(FSA)              # For Dunn's test
library(knitr)            # For tables in rmarkdown
library(cowplot)          # For doing good plots
library(fingertipsR)      # For getting data from Fingertips


#### Download files ####

if(!file.exists("imd_la_2015.xlsx")){
  download.file(url = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/464465/File_11_ID_2015_Upper-tier_Local_Authority_Summaries.xlsx",
                destfile = "imd_la_2015.xlsx")
}

#### Load data ####

imd_la <- read_xlsx(path = "imd_la_2015.xlsx",
                    sheet = 2)

for(i in 3:11){
  t <- read_xlsx(path = "imd_la_2015.xlsx",
                 sheet = i)
  imd_la <- merge(imd_la, t,
                  by = 1:2)
}

rm(t)

#### Select only domains ####

# Use average scores
imd_la_subdoms <- dplyr::select(imd_la,
                                1:2,
                                contains("Average score"),
                                -contains("Rank")) 

# rename variables
names(imd_la_subdoms) <- c("utla_code",
                           "utla_name",
                           "imd",
                           "income",
                           "employment",
                           "educ_skill",
                           "health",
                           "crime",
                           "housing",
                           "living_env",
                           "IDACI",
                           "IDAOPI")

# Create IMD quintiles variable
imd_la_subdoms <- mutate(imd_la_subdoms,
                         imd_quintile = ntile(imd, n = 5))

imd_la_subdoms <- imd_la_subdoms[ , c(1:2, 13, 3:12)]

#### Centre and scale variables ####

# Because we're using squared Euclidean distances would over/underweight some variables unless scaled

for(i in 5:ncol(imd_la_subdoms)){
  imd_la_subdoms[,i] <- (imd_la_subdoms[,i] - mean(imd_la_subdoms[,i])) / sd(imd_la_subdoms[,i])
}

# Drop IDACI and IDAOPI variables
imd_la_subdoms <- select(imd_la_subdoms,
                         -IDACI,
                         -IDAOPI)

#### Heirarchical clustering with sigclust2 ####

# Perform clustering with shc()
shc_result <- shc(as.matrix(imd_la_subdoms[,5:11]),
                  linkage = "complete",
                  alpha = 0.001,
                  n_sim = 500)

shc_result$hc_dat$labels <- imd_la_subdoms$utla_name

# Add cluster membership to data
imd_la_subdoms <- mutate(imd_la_subdoms,
                         cluster = factor(shcutree(shc_result)))

#### Figure 1a: Dendrogram ####
fig1a <- plot(shc_result,
              use_labs = FALSE,
              groups = imd_la_subdoms$cluster) + 
       theme_minimal() +
       labs(title = NULL) +
       scale_fill_brewer(type = "qualitative",
                         palette = "Set1",
                         name = "Cluster") +
       theme(legend.position = "none")

#### Figure 1b: Mapping clusters ####

# Get map data
if(!file.exists("la_map.Rdata")){
  la_map <- geojson_read("https://opendata.arcgis.com/datasets/d3d7b7538c934cf29db791a705631e24_4.geojson",
                         what = "sp") 
  save(la_map, file = "la_map.Rdata")
}else{
  load("la_map.Rdata")  
}

# Merge imd domain and cluster data with map data
map <- tidy(la_map, region = "ctyua17cd") 
map <- filter(map, !grepl("W", id))
map <- left_join(map, imd_la_subdoms,
                 by = c("id" = "utla_code"))

fig1b <- ggplot(data = map,
                aes(y = lat, 
                    x = long,
                    group = group,
                    fill = cluster)) +
  geom_polygon(col = "black") +
  theme_minimal() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.2),
        #plot.title = element_text(size = 20),
        #legend.text = element_text(size = 12),
        #legend.title = element_text(size = 12),
        legend.position = "right")

# Make an insert of London boroughs
# Get list of London Boroughs

if(!file.exists("london_boroughs.csv")){
  download.file(url = "https://data.london.gov.uk/download/london-borough-profiles/c1693b82-68b1-44ee-beb2-3decf17dc1f8/london-borough-profiles.csv",
              destfile = "london_boroughs.csv")
}

london <- read.csv("london_boroughs.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding="latin1")

# Create map object for only London boroughs
map_london <- filter(map, id %in% london$Code)

# Map London boroughs only
fig1b_insert <- ggplot(data = map_london,
                aes(y = lat, 
                    x = long,
                    group = group,
                    fill = cluster)) +
  geom_polygon(col = "black") +
  theme_minimal() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  guides(fill = FALSE) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  panel_border(colour = "black")

fig1b2 <- ggdraw() +
          draw_plot(fig1b) +
          draw_plot(fig1b_insert,
                    height = 0.25,
                    width = 0.25,
                    x = 0.55,
                    y = 0.65)

# Plot figures 1a and 1b together.
fig1_main <- plot_grid(fig1a, fig1b2, 
                       labels = "auto",
                       ncol = 1)

# Add a shared caption

cap_gg <- ggplot() +
  labs(caption = str_wrap("Fig. 1 (a) Clustering dendrogram showing the clustering structure. Nodes at which clustering was statistically significant are indicated by red branches. \nP-values for significant nodes are presented. Five statistically significant clusters are identified, indicated by the coloured bars at the bottom.\nFWER - family-wise error rate.\n
(b) Map showing the geographic distribution of the significant clusters identified.",
                          width = 60)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))   

fig1 <- plot_grid(fig1_main, 
                  cap_gg,
                  ncol = 1, 
                  rel_heights = c(1, 0.25))

ggsave(filename = "figure1.tiff",
       plot = fig1,
       width = 9,
       height = 16,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Table 2: Comparing clusters and IMD quintiles ####

# Create table of clusters vs IMD quintile
table2 <- ftable(imd_la_subdoms$cluster,
                 imd_la_subdoms$imd_quintile) %>%
          addmargins()

colnames(table2) <- c(as.character(1:5), "total")
rownames(table2) <- c(as.character(1:5), "total")

# Save table
write.csv(table2,
          file = "table2.csv",
          row.names = TRUE)

#### Table 1: Describe cluster characteristics ####

cluster_table <- imd_la_subdoms %>%
  group_by(cluster) %>%
  summarise(n = n(),
            IMD_score_med = median(imd),
            IMD_score_iqr = IQR(imd),
            income_med = median(income),
            income_iqr = IQR(income),
            health_med = median(health),
            health_iqr = IQR(health),
            crime_med = median(crime),
            crime_iqr = IQR(crime),
            employment_med = median(employment),
            employment_iqr = IQR(employment),
            education_med = median(educ_skill),
            education_iqr = IQR(educ_skill),
            housing_med = median(housing),
            housing_iqr = IQR(housing),
            environment_med = median(living_env),
            environment_iqr = IQR(living_env)) %>%
            mutate_if(is.double,
                      round,
                      digits = 2)

# table for paper
table1 <- cluster_table %>%
          mutate(IMD_score = paste0(IMD_score_med, " (", IMD_score_iqr, ")"),
                 income = paste0(income_med, " (", income_iqr, ")"),
                 employment = paste0(employment_med, " (", employment_iqr, ")"),
                 education = paste0(education_med, " (", education_iqr, ")"),
                 health = paste0(health_med, " (", health_iqr, ")"),
                 crime = paste0(crime_med, " (", crime_iqr, ")"),
                 housing = paste0(housing_med, " (", housing_iqr, ")"),
                 environment = paste0(environment_med, " (", environment_iqr, ")")
                 ) %>%
          select(cluster,
                 n,
                 IMD_score,
                 income,
                 employment,
                 education,
                 health,
                 crime,
                 housing,
                 environment)

write.csv(table1,
          file = "table1.csv",
          row.names = FALSE)

#### Figure 2: Plotting domain scores ####
# Use boxplots
fig2 <- ggplot(data = imd_la_subdoms %>%
               select(-imd, -imd_quintile) %>%
               gather(key = "variable",
                      value = "value",
                      -cluster,
                      -utla_code,
                      -utla_name,
                      factor_key = TRUE) %>%
               mutate(cluster = paste0("Cluster ", cluster),
                      variable = factor(variable,
                                        levels = c("income",
                                                   "employment",
                                                   "educ_skill",
                                                   "health",
                                                   "crime",
                                                   "housing",
                                                   "living_env"),
                                        ordered = TRUE)) %>%
               mutate(variable = fct_recode(variable,
                                            `living env.` = "living_env",
                                            education = "educ_skill")),
             aes(x = variable,
                 y = value,
                 fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~factor(cluster), ncol = 2) +
  stat_identity(yintercept=0, 
                lty = "dashed",
                geom='hline', 
                inherit.aes=TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 20),
        #strip.text.x = element_text(size = 14),
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 14),
        #axis.text.x = element_text(size = 14),
        #axis.title.y = element_text(face = "bold")
  ) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90),
        plot.caption = element_text(hjust = 0)) +
  guides(fill = FALSE) +
  labs(x = NULL,
       y = "sub-domain z-scores",
       caption = "Fig. 2: Boxplots of domain z-scores for the five clusters.")

ggsave(filename = "figure2.tiff",
       plot = fig2,
       width = 9,
       height = 16,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Get urban-rural data and merge with main dataset ####

# Get urban-rural data
if(!file.exists("urban_rural.xls")){
  download.file(url = "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/ruralurbanclassifications/2001ruralurbanclassification/ruralurbanlocalauthoritylaclassificationengland/laclassificationdatasetpost0409tcm77188156.xls",
                destfile = "urban_rural.xls")
}

urban_rural <- read_xls("urban_rural.xls")

# Get lookup from lower-tier to upper-tier
if(!file.exists("lower_tier_to_upper_tier_lookup.csv")){
  url <- "https://ons.maps.arcgis.com/sharing/rest/content/items/f3af497a0bf34930adc61272ddfa698b/data"
  download.file(url = url,
                destfile = "lower_tier_to_upper_tier_lookup.csv",
                method = "curl")
}

lookup <- read.csv(file = "lower_tier_to_upper_tier_lookup.csv",
                   stringsAsFactors = FALSE,
                   header = TRUE) 

# Fix lookup table so that unitaries aren't merged into regions
lookup_fixer <- function(region_names){
  lu <- lookup
  for(i in region_names){
    lu$UTLA16CD[lu$UTLA16NM == i] <- lu$LTLA16CD[lu$UTLA16NM == i]
    lu$UTLA16NM[lu$UTLA16NM == i] <- lu$LTLA16NM[lu$UTLA16NM == i]
  }
  return(lu)
}

regions <- c("Inner London",
             "Outer London",
             "Greater Manchester",
             "West Midlands",
             "West Yorkshire",
             "South Yorkshire",
             "Merseyside",
             "Tyne and Wear")

lookup <- lookup_fixer(regions)

# Need to match on names - fix names that don't match
urban_rural <- urban_rural %>%
               mutate(Name = recode(Name,
                                    "Bristol City of" = "Bristol, City of",
                                    "Durham" = "County Durham",
                                    "Herefordshire County of" = "Herefordshire, County of",
                                    "King`s Lynn and West Norfolk" = "King's Lynn and West Norfolk",
                                    "Kingston upon Hull City of" = "Kingston upon Hull, City of",
                                    "St. Edmundsbury" = "St Edmundsbury"))

# Merge lower to upper tier lookup with urban-rural data
urban_rural <- merge(urban_rural, lookup,
                     by.x = "Name",
                     by.y = "LTLA16NM",
                     all.x = FALSE)

# Summarise urban-rural data for upper tier local authorities
urban_rural <- urban_rural %>%
               group_by(UTLA16CD, UTLA16NM) %>%
               summarise(total_pop = sum(`Total Population1`),
                         rural_pop = sum(`Total Population1` * `Rural% (including Large Market Town population)2` / 100)) %>%
               mutate(percent_rural = rural_pop * 100 / total_pop) %>%
               select(utla_code = UTLA16CD,
                      utla_name = UTLA16NM,
                      percent_rural)

imd_la_subdoms <- merge(imd_la_subdoms, urban_rural)

# Remove temporary objects
rm(urban_rural)

#### Get demographic data and merge with main dataset ####

# Get demographic data from fingertips
inds <- indicators() %>%
        filter(grepl("Supporting information", IndicatorName))

# Percentages of under 18s, over 65s, and ethnic minorities
demog <- fingertips_data(IndicatorID = unique(inds$IndicatorID)) %>%
         filter(AreaType == "County & UA (pre 4/19)",
                Timeperiod == 2016,
                Sex == "Persons") %>%
         select(utla_code = AreaCode,
                ind = IndicatorName,
                value = Value) %>%
         spread(key = ind,
                value = value) %>%
         rename(percent_65plus = 2,
                percent_under18 = 3,
                percent_ethnic = 4)

# Total population numbers
pop <- fingertips_data(IndicatorID = inds$IndicatorID[1]) %>%
       filter(AreaType == "County & UA",
              Timeperiod == 2016,
              Sex == "Persons") %>%
       select(utla_code = AreaCode,
              total_pop = Denominator) %>%
       mutate(total_pop = total_pop / 100000)

# Merge percentages and total population data together
demog <- merge(demog, pop)

# Merge demographic data with main dataset
imd_la_subdoms <- merge(imd_la_subdoms, demog)

# Remove temporary objects
rm(demog, pop, inds)

#### Get geographic size data from ONS ####

# Get geographic size of upper tier local authorities
if(!file.exists("la_sizes.xls")){
  url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationestimatesbyfiveyearagebandsandhouseholdestimatesforlocalauthoritiesintheunitedkingdom/r12ukrttablep04ukv2_tcm77-304141.xls"
  download.file(url = url,
                destfile = "la_sizes.xls",
                method = "curl")
}

utla_sizes <- read_excel("la_sizes.xls",
                         sheet = 2,
                         skip = 12,
                         trim_ws = TRUE)

# Fix codes for Gateshead and Northumberland
utla_sizes$`Area code 2`[utla_sizes$...4 == "Gateshead"] <- "E08000037"
utla_sizes$`Area code 2`[utla_sizes$...3 == "Northumberland UA 5"] <- "E06000057"

# Select upper tier local authorities and scale numeric to 10,000s of HA
utla_sizes <- utla_sizes %>%
              select(utla_code = 1,
                     area = Area) %>%
              filter(!is.na(utla_code),
                     utla_code %in% imd_la_subdoms$utla_code) %>%
              mutate(area = as.numeric(area)/10000)

# Merge with main data set
imd_la_subdoms <- merge(imd_la_subdoms, utla_sizes)

# Remove temporary objects
rm(utla_sizes)

#### Testing for significant differences in cluster characteristics ####

# write a function to do testing and plotting of comparisons
subdom_test <- function(v, m = "bonferroni"){
  d <- imd_la_subdoms %>% 
       mutate(cluster = factor(cluster)) %>%
       select(cluster, var = contains(v))
  
  kt <- kruskal.test(var ~ cluster,
                     data = d)
  
  dt <- dunnTest(var ~ cluster,
                 data = d,
                 method = m)
  
  g <- ggplot(data = d,
              aes(x = cluster,
                  y = var,
                  fill = cluster)) +
       geom_boxplot() +
       labs(title = "",
            x = NULL,
            y = v) +
       theme_minimal() +
       scale_fill_brewer(type = "qualitative",
                           palette = "Set1") +
       guides(fill = FALSE)

  return(list(kt, dt, g))
  
}

# List of variables to compare
vars <- c("area",
          "total_pop",
          "percent_under18",
          "percent_65plus",
          "percent_ethnic",
          "percent_rural")

# Empty vectors for stats and p-values from Kruskal-Wallis tests
kt_stat <- numeric()
kt_pval <- numeric()

for(i in 1: length(vars)){
  t <- subdom_test(vars[i])
  kt_stat[i] <- t[[1]]$statistic
  kt_pval[i] <- t[[1]]$p.value
  g <- t[[3]]
  assign(paste0("g_", vars[i]), g)
}

df <- rep(4, times = length(kt_stat))
varnames <- c("Area",
              "Total population",
              "% aged under 18",
              "% aged 65 and over",
              "% ethnic minority",
              "% living in rural areas")

# Table of comparisons
kt_table <- tibble(varnames, kt_stat, df, kt_pval)

#### Table 3: Cluster characteristics ####
table3 <- kt_table %>%
          mutate(kt_stat = round(kt_stat, digits = 2)) %>%
          mutate(kt_pval = ifelse(kt_pval < 0.001, 0, kt_pval)) %>%
          mutate(kt_pval = round(kt_pval, digits = 3)) %>%
          mutate(kt_pval = ifelse(kt_pval == 0, "< 0.001", kt_pval)) %>%
          rename(Variable = varnames,
                 `H` = kt_stat,
                 `p-value` = kt_pval)  

cluster_chars <- imd_la_subdoms %>%
                 group_by(cluster) %>%
                 summarise(area_median = median(area),
                           area_iqr = IQR(area),
                           total_pop_median = median(total_pop, na.rm = TRUE),
                           total_pop_iqr = IQR(total_pop, na.rm = TRUE),
                           percent_under18_median = median(percent_under18, na.rm = TRUE),
                           percent_under18_iqr = IQR(percent_under18, na.rm = TRUE),
                           percent_over65_median = median(percent_65plus, na.rm = TRUE),
                           percent_over65_iqr = IQR(percent_65plus, na.rm = TRUE),
                           percent_ethnic_median = median(percent_ethnic, na.rm = TRUE),
                           percent_ethnic_iqr = IQR(percent_ethnic, na.rm = TRUE),
                           percent_rural_median = median(percent_rural, na.rm = TRUE),
                           percent_rural_iqr = IQR(percent_rural, na.rm = TRUE)) %>%
                mutate_if(is.numeric,
                          round,
                          digits = 2) %>%
                mutate(Area = paste0(area_median, " (", area_iqr, ")"),
                       `Total population` = paste0(total_pop_median, " (", total_pop_iqr, ")"),
                       `% aged under 18` = paste0(percent_under18_median, " (", percent_under18_iqr, ")"),
                       `% aged 65 and over` = paste0(percent_over65_median, " (", percent_over65_iqr, ")"),
                       `% ethnic minority`= paste0(percent_ethnic_median, " (", percent_ethnic_iqr, ")"),
                       `% living in rural areas`= paste0(percent_rural_median, " (", percent_rural_iqr, ")")) %>%
                select(Area, 
                       `Total population`, 
                       `% aged under 18`,
                       `% aged 65 and over`,
                       `% ethnic minority`,
                       `% living in rural areas`) %>%
                t() %>%
                as.data.frame() %>%
                rownames_to_column() 

colnames(cluster_chars) <- c("Variable", paste0("cluster ", 1:(ncol(cluster_chars)-1)))

table3 <- merge(table3, cluster_chars,
                by = "Variable") %>%
          select(Variable,
                 5:9,
                 2:4) %>%
          mutate(Variable = factor(Variable,
                                   levels = c("Area",
                                              "Total population",
                                              "% aged under 18",
                                              "% aged 65 and over",
                                              "% ethnic minority",
                                              "% living in rural areas"))) %>%
          arrange(Variable)

write.csv(table3,
          file = "table3.csv",
          row.names = FALSE)

#### Figure 3: Plotting cluster characteristics ####

# Add appropriate axis labels and titles
g_area <- g_area + 
          labs(title = "Geographic size",
               y = "area (10,000 ha)") +
          theme(plot.title = element_text(size = 10),
                axis.title = element_text(size = 9))

g_total_pop <- g_total_pop + 
               labs(title = "Population size",
                    y = "population (100,000s)")+
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9))

g_percent_under18 <- g_percent_under18 + 
  labs(title = "Aged under 18",
       y = "%")+
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9))

g_percent_65plus <- g_percent_65plus + 
  labs(title = "Aged 65+",
       y = "%")     +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9))  

g_percent_ethnic <- g_percent_ethnic + 
  labs(title = "Ethnic diversity",
       y = "%")+
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9))

g_percent_rural <- g_percent_rural + 
  labs(title = "Rural population",
       y = "%")+
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9))

# Plot comparisons
fig3_main <- plot_grid(g_area, 
                       g_total_pop, 
                       g_percent_under18,
                       g_percent_65plus,
                       g_percent_ethnic,
                       g_percent_rural,
                       ncol = 2,
                       labels = "auto")

# Add caption

cap_gg3 <- ggplot() +
  labs(caption = str_wrap("Fig. 3: Characterisation of clusters according to: (a) Geographic size in hectares (source: ONS), (b) Population size (100,000s) (source: PHE; 2016 data), (c) Percentage of population aged under 18 years (source: PHE; 2016 data). (d) Percentage of population aged 65 years and older (source: PHE; 2016 data), (e) Percentage of population from black and minority ethnic backgrounds (source: PHE; 2016 data), and (f) Percentage of population living in rural areas (including large market towns; source: ONS; 2011 data)",
                          width = 60)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0,
                                    size = 9)) 

fig3 <- plot_grid(fig3_main, 
                  cap_gg3,
                  ncol = 1, 
                  rel_heights = c(1, 0.25))

ggsave(filename = "figure3.tiff",
       plot = fig3,
       width = 9,
       height = 18,
       units = "cm",
       device = "tiff",
       dpi = "print")
