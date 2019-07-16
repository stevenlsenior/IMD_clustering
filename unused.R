#### Extra code not used in main analyses ####

#### Principle components analysis ####
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

#### Making a hexmap ####

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

#### Unused bar plots of subdomain scores ####
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

#### Images for PHE e-poster ####
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

#### Code to test for sig differences in subdomains ####
# test comparisons

subdom_test("imd")
subdom_test("income")
subdom_test("employ")
subdom_test("educ")
subdom_test("health")
subdom_test("crime")
subdom_test("housing")
subdom_test("living")