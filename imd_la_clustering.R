#### Install packages ####
# install.packages(c("devtools", "readxl", "ape", "geojsonio",
#                   "rgdal", "GISTools", "ggalt", "broom", "tidyverse", "FSA",
#                     cowplot),
#                  dependencies = TRUE)

devtools::install_github("pkimes/sigclust2")
devtools::install_version("rgdal", 
                          version = "1.3-9",
                          repos = "http://cran.us.r-project.org")

#### load packages ####
library(sigclust2)        # For clustering
library(devtools)         # For installing packages
library(readxl)           # For reading excel files
library(ape)              # For plotting dendrograms
library(geojsonio)        # For managing geojson
library(rgdal)            # For mapping
library(GISTools)         # For mapping
library(ggalt)            # For mapping
library(broom)            # For tidying map data
library(tidyverse)        # For data manipulation
library(FSA)              # For Dunn's test
library(knitr)            # For tables in rmarkdown
library(cowplot)          # For doing good plots


#### Download files ####

if(!file.exists("imd_la.xlsx")){
  download.file(url = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/464465/File_11_ID_2015_Upper-tier_Local_Authority_Summaries.xlsx",
                destfile = "imd_la.xlsx")
}

#### Load data ####

imd_la <- read_xlsx(path = "imd_la.xlsx",
                    sheet = 2)

for(i in 3:11){
  t <- read_xlsx(path = "imd_la.xlsx",
                 sheet = i)
  imd_la <- merge(imd_la, t,
                  by = 1:2)
}

rm(t)

#### Select only subdomains ####

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

# Make figure 1 using plot.shc()
fig1a <- plot(shc_result,
              use_labs = FALSE,
              groups = imd_la_subdoms$cluster) + 
       theme_minimal() +
       scale_fill_brewer(type = "qualitative",
                         palette = "Set1",
                         name = "Cluster") +
       labs(title = "A. Clustering dendrogram") +
       theme(plot.title = element_text(size = 20),
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 14))

fig1a

#### Mapping clusters ####

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
  labs(title = "B. Geographic distribution of clusters",
       x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

fig1b

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
  labs(title = "London",
       x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  guides(fill = FALSE) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12,
                                  hjust = 0.5))

fig1b_insert

ggdraw() +
  draw_plot(fig1b) +
  draw_plot(fig1b_insert,
            height = 0.2,
            width = 0.2,
            x = 0.70,
            y = 0.55)


# Do a hex-map instead
if(!file.exists("la_hex_map.Rdata")){
  la_hex_map <- geojson_read("https://olihawkins.com/files/media/2018/02/1/hexmap-lad-ew.geojson",
                         what = "sp") 
  save(la_hex_map, file = "la_hex_map.Rdata")
}else{
  load("la_hex_map.Rdata")  
}

# Tidy hexmap data
hexmap <- tidy(la_hex_map, region = "c")

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

# Keep only English local authorities & fix variable names
lookup <- filter(lookup,
                 grepl("E", UTLA16CD)) %>%
          select(id = LTLA16CD,
                 utla_code = UTLA16CD)

# Merge lookup with hexmap
hexmap <- left_join(hexmap, lookup)
  
# Merge IMD data with hexmap
hexmap <- left_join(hexmap, imd_la_subdoms)

# Keep only English local authorities
hexmap <- filter(hexmap, grepl("E", id))

# Plot the hexmap
fig1c <- ggplot(data = hexmap,
                aes(y = lat, 
                    x = long,
                    group = group,
                    fill = cluster)) +
  geom_polygon(col = "black") +
  theme_minimal() +
  labs(title = "B. Geographic distribution of clusters",
       x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

fig1c

# Plot figures 1a and 1b together.
fig1 <- multiplot(fig1a, fig1b, cols = 2)

png(file = "figure1.png",
    width = 960,
    height = 480)
multiplot(fig1a, fig1b, cols = 2)
dev.off()

# Lets see how those clusters compare with the earlier plot of IMD quintiles & principle components
# Extract principle components
imd_pcs <- prcomp(imd_la_subdoms[, 5:11])

imd_la_prcomps <- data.frame(imd_pcs$x)


g1 <- ggplot(data = imd_la_prcomps,
             aes(x = PC1,
                 y = PC2,
                 colour = factor(imd_la_subdoms$imd_quintile))) +
  geom_point()  +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  guides(colour=guide_legend(title="IMD quintile",
                             nrow = 2,
                             byrow = TRUE))

g2 <- ggplot(data = imd_la_prcomps,
             aes(x = PC1,
                 y = PC2,
                 colour = imd_la_subdoms$cluster)) + 
  geom_point() +
  scale_colour_brewer(type = "qualitative",
                    palette = "Set1") +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  guides(colour=guide_legend(title="Cluster",
                             nrow = 2,
                             byrow = TRUE))


plot_grid(g1, g2, ncol = 2, labels = "AUTO")

#### Describe cluster characteristics ####

cluster_table <- imd_la_subdoms %>%
  group_by(cluster) %>%
  summarise(num_las = n(),
            income = median(income),
            health = median(health),
            crime = median(crime),
            employ = median(employment),
            education = median(education_skill),
            housing = median(housing),
            environment = median(living_env),
            IMD_score = median(imd))

#### UNFINISHED - Get urban-rural and demographic data ####

# Get urban-rural data

if(!file.exists("urban_rural.xls")){
  download.file(url = "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/ruralurbanclassifications/2001ruralurbanclassification/ruralurbanlocalauthoritylaclassificationengland/laclassificationdatasetpost0409tcm77188156.xls",
                destfile = "urban_rural.xls")
}

urban_rural <- read_xls("urban_rural.xls")

#### Statistical testing for differences in subdomains ####

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
            x = "cluster",
            y = paste0(v, " (z-score)")) +
       theme_minimal() +
       scale_fill_brewer(type = "qualitative",
                           palette = "Set1") +
       guides(fill = FALSE)
  
  print(kt)
  print(dt)
  print(g)
  
  return(list(kt, dt, g))
  
}

# test comparisons

subdom_test("imd")
subdom_test("income")
subdom_test("employ")
subdom_test("educ")
subdom_test("health")
subdom_test("crime")
subdom_test("housing")
subdom_test("living")

#### Plotting subdomain scores ####

g4 <- ggplot(data = cluster_table %>%
               gather(key = "variable",
                      value = "value",
                      -cluster,
                      -num_las,
                      -IMD_score,
                      factor_key = TRUE) %>%
               mutate(cluster = paste0("Cluster ", cluster)),
             aes(x = variable,
                 y = value,
                 fill = factor(cluster))) +
       geom_bar(stat = "identity") +
       facet_wrap(~factor(cluster)) +
       theme_minimal() +
       scale_fill_brewer(type = "qualitative",
                          palette = "Set1") +
       theme(axis.text.x = element_text(angle = 90)) +
       guides(fill = FALSE) +
       labs(x = NULL,
            y = "median sub-domain z-scores") +
       ggtitle("Figure 2: Cluster median sub-domain scores")

g4
  
# Use boxplots
g5 <- ggplot(data = imd_la_subdoms %>%
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
                                        ordered = TRUE)),
             aes(x = variable,
                 y = value,
                 fill = factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~factor(cluster)) +
  stat_identity(yintercept=0, 
                lty = "dashed",
                geom='hline', 
                inherit.aes=TRUE) +
  theme_minimal() +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(fill = FALSE) +
  labs(x = NULL,
       y = "sub-domain z-scores") +
  ggtitle("Figure 2: Cluster median sub-domain scores")

g5

#### Images for PHE e-poster
phe1 <- plot(shc_result,
              use_labs = FALSE,
              groups = shcutree(shc_result)) + 
  theme_minimal() +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  labs(title = "Figure 1: Clustering Dendrogram",
       subtitle = "Clusters of English upper-tier local authorities \nby 2015 sub-domains of deprivation",
       caption = "Data source: Ministry of Housing, Communities and Local Government") +
  theme(plot.title = element_text(size = 20))

ggsave(filename = "phe_image_1.png",
       plot = phe1,
       device = "png",
       height = 13.69,
       width = 14.2,
       units = "cm")

phe2 <- ggplot(data = map,
                aes(y = lat, 
                    x = long,
                    group = group,
                    fill = cluster)) +
  geom_polygon(col = "black") +
  theme_minimal() +
  labs(title = "Figure 2: Geographic distribution of clusters",
       subtitle = "Clusters of English upper-tier local authorities \nby 2015 sub-domains of deprivation",
       caption = "Data sources: 
       \n1. Ministry of Housing, Communities and Local Government
       \n2. Office for National Statistics Open Geography Portal",
       x = NULL,
       y = NULL) +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1",
                    name = "Cluster") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

ggsave(filename = "phe_image_2.png",
       plot = phe2,
       device = "png",
       height = 13.69,
       width = 14.2,
       units = "cm")

phe3 <- ggplot(data = cluster_table %>%
               gather(key = "variable",
                      value = "value",
                      -cluster,
                      -num_las,
                      -IMD_score,
                      factor_key = TRUE),
             aes(x = variable,
                 y = value,
                 fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  facet_wrap(~factor(cluster)) +
  theme_linedraw() +
  scale_fill_brewer(type = "qualitative",
                    palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.83, 0.1),
        legend.direction = "vertical",
        legend.title.align = 0.0,
        legend.text = element_text(size = 12)) +
  guides(fill = FALSE) +
  labs(title = "Figure 3: Cluster Profiles",
       subtitle = "Median sub-domain scores for significant clusters",
       caption = "Data source: Ministry of Housing, Communities and Local Government",
       x = NULL,
       y = "median sub-domain z-scores",
       fill = "Cluster")

  ggsave(filename = "phe_image_3.png",
         plot = phe3,
         device = "png",
         height = 13.69,
         width = 14.2,
         units = "cm")
