library(RSQLite)
library(dplyr)
library(ggplot2)
library(summarytools)
library(GGally)
library(readr)
library(rlang)
library(stringr)
library(gridExtra)

install.packages(dplyr)
install.packages("ggplot2")
install.packages(summarytools)
install.packages(GGally)
install.packages(readr)

# Establish connection
setwd("D:/Dílna/Studium/DP - Bio/Diplomka - Analyses")
db_path <- "Data/Dataset.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Set query
query <- "SELECT * FROM traits_unique_per_network WHERE species IS NOT NULL"  # Adjust your SELECT query as needed
df <- dbGetQuery(con, query)
dbDisconnect()

# Preliminary summary
df_summary <- summarytools::dfSummary(df)
summarytools::view(df_summary)

# Boxplots for season and elevation:

# Boxplots

# Define a vector of strings
list_of_strings <- tail(names(unique_species_across_df), 8)

# Store each plot in a list
plot_list <- list()

# Loop over each string in the list
for (i in 1:length(list_of_strings)) {
  
  # Prepare strings
  metric <- list_of_strings[[i]]
  label_name <- str_to_title(gsub("_", " ", metric))
  elev <- as.factor(unique_species_across_df$elevation)
  
  # Prepare plots
  plot_list[[i]] <- ggplot(df, aes_string(x="elev", y=metric, color="season")) +
    geom_boxplot() +
    labs(x="Elevation", y=label_name, title = paste(label_name, "Distribution (All)")) +
    theme_classic()
  print(plot_list[[i]])
  
} 

#Barplots of categorials across elevation: -----------------------------------------------------------------------

# Prepare lists of colors and season labels
color_list <- list("orange", "steelblue")
season_list <- list("DRY", "WET")
elevation_list_strings <- as.list(c("650", "1100", "1450", "2250"))

# Define a list of strings
list_of_traits <- names(df)[5:13]

for (i in seq_along(list_of_traits)) {
  
  plot_list <- list()
  
  # Prepare strings
  metric <- list_of_traits[[i]]
  label_name <- str_to_title(gsub("_", " ", metric))
  
  #For each elevation
  for (j in seq_along(elevation_list_strings)) {

    plot_list[[j]] <- ggplot(filter(df, elevation == as.numeric(elevation_list_strings[[j]])), aes_string(x=metric, fill="season")) +
      geom_bar(position="dodge") +
      labs(x=paste(label_name, elevation_list_strings[[j]]), y="Count of Species") +
      theme_minimal()
  }
  
  # Arrange the plots in two rows and four columns
  grid.arrange(grobs=rev(plot_list), nrow=4, ncol=1)
  
}
--------------------------------------


# Number of species in each network (elevation*season): -----------------------------------------------------

# Species count per elevation and season
species_count_per_network <- df %>% 
  count(elevation, season)

# Define custom colors for seasons
season_colors <- c("DRY" = "orange", "WET" = "steelblue")

# Custom labels for elevation axis
elevation_labels <- c("650" = "650m asl", "1100" = "1100m asl", "1450" = "1450m asl", "2250" = "2250m asl")

# Create the plot with specified customizations
ggplot(species_count_per_network, aes(x = factor(elevation), y = n, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = season_colors) +
  scale_x_discrete(labels = elevation_labels) +  # Use custom labels for elevation axis
  labs(x = "Elevation", y = "Number of Unique Species", fill = "Season") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal()

# Basic histograms for continuous traits across entire data sets: ---------------------------------------------

# Removed duplicates based on sp_code.
unique_species_across_df <- df %>% distinct(sp_code, .keep_all = TRUE)


# Histograms

# Define a vector of strings
list_of_strings <- tail(names(unique_species_across_df), 8)

# Store each plot in a list
plot_list <- list()

# Loop over each string in the list
for (i in 1:length(list_of_strings)) {
  
  # Prepare strings
  metric <- list_of_strings[[i]]
  label_name <- str_to_title(gsub("_", " ", metric))
  
  # Prepare plots
  plot_list[[i]] <- ggplot(unique_species_across_df, aes_string(x=metric)) +
    geom_histogram(bins=20, fill="forestgreen", color="white") +
    labs(x=label_name, y="Frequency") +
    theme_minimal() + labs(title = paste(label_name, "Distribution (All)")) + theme(
      panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
      plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
    )

} 

# Arrange the plots in two rows and four columns
grid.arrange(grobs=plot_list, nrow=2, ncol=4)

# Basic histograms for continuous traits for every season: ------------------------------------------------

# Prepare data frames for each season
unique_species_dry <- filter(df, season == "DRY") %>% distinct(sp_code, .keep_all = TRUE)
unique_species_wet <- filter(df, season == "WET") %>% distinct(sp_code, .keep_all = TRUE)

# Histograms
list_of_dataframes <- list(unique_species_dry, unique_species_wet)
color_list <- list("orange", "steelblue")
season_list <- list("DRY", "WET")

for (i in seq_along(list_of_dataframes)) {
  
  # Current dataframe
  current_df <- list_of_dataframes[[i]]
  
  # Define a vector of strings
  list_of_strings <- tail(names(current_df), 8)
  
  # Store each plot in a list
  plot_list <- list()
  
  # Loop over each string in the list
  for (j in 1:length(list_of_strings)) {
    
    # Prepare strings
    metric <- list_of_strings[[j]]
    label_name <- str_to_title(gsub("_", " ", metric))
    
    # Prepare plots
    plot_list[[j]] <- ggplot(current_df, aes_string(x=metric)) +
      geom_histogram(bins=20, fill=color_list[[i]], color="white") +
      labs(x=label_name, y="Frequency") +
      theme_minimal() + labs(title = paste(label_name, "Distribution", season_list[[i]])) + theme(
        panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
        plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
      )
    
  } 
  
  # Arrange the plots in two rows and four columns
  grid.arrange(grobs=plot_list, nrow=2, ncol=4)
  
}

# Basic histograms for individual traits, per elevation, per season: -------------------------------------------

# Prepare data frames for each season
unique_species_dry <- filter(df, season == "DRY") %>% distinct(sp_code, .keep_all = TRUE)
unique_species_wet <- filter(df, season == "WET") %>% distinct(sp_code, .keep_all = TRUE)

# Prepare a list of dataframes pee season and elevation
elevation_list <- list(filter(df, elevation == 650), filter(df, elevation == 1100), filter(df, elevation == 1450), filter(df, elevation == 2250))
elevation_list_dry <- list(filter(df, elevation == 650, season == "DRY"), filter(df, elevation == 1100, season == "DRY"), filter(df, elevation == 1450, season == "DRY"), filter(df, elevation == 2250, season == "DRY"))
elevation_list_wet <- list(filter(df, elevation == 650, season == "WET"), filter(df, elevation == 1100, season == "WET"), filter(df, elevation == 1450, season == "WET"), filter(df, elevation == 2250, season == "WET"))
list_of_dataframes <- elevation_list


# Prepare lists of colors and season labels
color_list <- list("orange", "steelblue")
season_list <- list("DRY", "WET")
elevation_list_strings <- as.list(as.character(unique(df$elevation)))

# Define a list of strings
list_of_traits <- as.list(tail(names(df), 8))

#For each trait
for (i in seq_along(list_of_traits)) {
  
  # Store each plot in a list
  plot_list <- list()
  counter <- 1
  
  #For each elevation
  for (j in seq_along(elevation_list)) {
    
    # Current dataframe
    current_df <- elevation_list[[j]]
    
    # Prepare strings
    metric <- list_of_strings[[i]]
    label_name <- str_to_title(gsub("_", " ", metric))
    
    for (k in seq_along(season_list)) {
      
      filtered_df <- filter(current_df, season == season_list[[k]])
        
      # Prepare plots
      plot_list[[counter]] <- ggplot(filtered_df, aes_string(x=metric)) +
        geom_histogram(bins=20, fill=color_list[[k]], color="white") +
        labs(x=label_name, y="Frequency") +
        theme_minimal() + labs(title = paste("(", elevation_list_strings[[j]], ")", label_name, "Distribution", season_list[[k]])) + theme(
          panel.background = element_rect(fill = "white", colour = NA), # Set the background color here
          plot.background = element_rect(fill = "white", colour = NA) # Optional: Set the plot margin color he
        )
      
      counter <- counter + 1
    }
    
  }
  
  # Arrange the plots in two rows and four columns
  grid.arrange(grobs=plot_list, nrow=4, ncol=2)
  
}


# Elaborate Size and Tube length plot ---------------------------------------------------------------------


library(ggplot2)
library(grid)
library(png)

# Prepare data frames for each season
unique_species_dry <- filter(unique_species_across_df, season == "DRY")
unique_species_wet <- filter(unique_species_across_df, season == "WET")


# Assuming you have a PNG image named "background.png" in your working directory
background <- readPNG("D:/Dílna/Studium/DP - Bio/Diplomka - Analyses/img/tube_bg.png")
g_background <- rasterGrob(background, interpolate=TRUE)

# Create two histograms
# Determine common scale for the size variable
common_bins <- 10
common_limit <- range(c(unique_species_wet$size, unique_species_dry$size), na.rm = TRUE)
common_breaks <- seq(from = common_limit[1], to = common_limit[2], length.out = common_bins + 1)

integer_breaks_x <- seq(floor(min(unique_species_wet$size)), 
                        ceiling(common_limit[2]-2), 
                        by = 1)

# Histogram for wet conditions with common scale
hist1a <- ggplot(unique_species_wet, aes(x=size)) +
  geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
  theme_void() +
  scale_x_continuous(limits = common_limit, breaks = integer_breaks_x, labels=integer_breaks_x) +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 3, vjust = 0.5),
    panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
    #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
    panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
    #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
  ) + 
  labs(subtitle = "Wet Season Flower Size")

integer_breaks_x <- seq(floor(1), 
                        ceiling(common_limit[2]-2), 
                        by = 1)

# Histogram for dry conditions with common scale
hist1b <- ggplot(unique_species_dry, aes(x=size)) +
  geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
  theme_void() +
  #scale_x_continuous(limits = rev(common_limit), breaks = rev(common_breaks)) +  # Reverse the limits and breaks for mirroring
  scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels=rev(integer_breaks_x)) +  # This will flip the x-axis
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 3, vjust = 0.5),
    panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
    #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
    panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
    #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
  ) + 
  labs(subtitle = "Dry Season Flower Size")

# Ensure both plots are displayed with the same y-axis limits if they are side by side
common_y_limit <- range(
  c(
    ggplot_build(hist1a)$layout$panel_params[[1]]$y.range,
    ggplot_build(hist1b)$layout$panel_params[[1]]$y.range
  )
)

hist1a <- hist1a + scale_y_continuous(limits = common_y_limit)
hist1b <- hist1b + scale_y_continuous(limits = common_y_limit)

# Determine common scale for the size variable
common_bins <- 10
common_limit <- range(c(unique_species_wet$tube_length, unique_species_dry$tube_length), na.rm = TRUE)
common_breaks <- seq(from = common_limit[1], to = common_limit[2], length.out = common_bins + 1)
common_limit_y <- range(0,10)
common_breaks_y <- seq(from = common_limit_y[1], to = common_limit_y[2], length.out = common_bins + 1)

integer_breaks_x <- seq(floor(min(unique_species_wet$tube_length)), 
                        ceiling(common_limit[2]), 
                        by = 1)

hist2a <- ggplot(unique_species_wet, aes(x=tube_length)) +
  geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
  coord_flip() + # Flip the axes to make histogram horizontal
  theme_void()  +
  scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels = rev(integer_breaks_x)) + # Invert the x-axis values, which are now vertical due to coord_flip()
  scale_y_continuous(limits = common_limit_y, breaks = common_breaks_y) +
  # Keep the void theme
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(hjust = 3, vjust = 0.5),
    panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
    #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
    panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
    #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
  ) + 
  labs(subtitle = "Wet Season Tube Length")

integer_breaks_x <- seq(floor(min(unique_species_dry$tube_length)), 
                        ceiling(common_limit[2]), 
                        by = 1)

hist2b <- ggplot(unique_species_dry, aes(x=tube_length)) +
  geom_histogram(fill="#2f2421", color="#fafcf4", bins=common_bins) +
  coord_flip() + # Flip the axes to make histogram horizontal
  theme_void()  +
  scale_x_reverse(limits = rev(common_limit), breaks = rev(integer_breaks_x), labels = rev(integer_breaks_x), position="top") + # Invert the x-axis values, which are now vertical due to coord_flip()
  scale_y_reverse(limits = rev(common_limit_y), breaks = rev(common_breaks_y)) +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(hjust = 3, vjust = 0.5),  # Add this line to bring back x-axis text (which is actually y-axis due to coord_flip)
    panel.grid.major.y = element_line(color = "#2f2421", size = 0.5), # Customize major Y grid lines (horizontal)
    #panel.grid.minor.y = element_line(color = "#2f2421", size = 0.25), # Customize minor Y grid lines (horizontal)
    panel.grid.major.x = element_line(color = "#2f2421", size = 0.5), # Customize major X grid lines (vertical)
    #panel.grid.minor.x = element_line(color = "#2f2421", size = 0.25) # Customize minor X grid lines (vertical)
  ) + 
  labs(subtitle = "Dry Season Tube Length")

# Ensure both plots are displayed with the same y-axis limits if they are side by side
#

# Create a base plot with the background image
base_plot <- ggplot() +
  annotation_custom(g_background, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_void()  # Keep the void theme

# Horizontal line grob
y_pos = 0.775
hline_grob <- linesGrob(
  x = unit(c(0.2, 0.8), "npc"),  # Span from the left to the right of the plot
  y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
  gp = gpar(
    col = "#2f2421",      # Line color
    lwd = unit(0.5, "mm")   # Line width in millimeters
  )
)

# Horizontal line grob
y_pos = 0.375
hline_grob_2 <- linesGrob(
  x = unit(c(0.2, 0.45), "npc"),  # Span from the left to the right of the plot
  y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
  gp = gpar(
    col = "#2f2421",      # Line color
    lwd = unit(0.5, "mm")   # Line width in millimeters
  )
)

# Horizontal line grob
y_pos = 0.375
hline_grob_3 <- linesGrob(
  x = unit(c(0.55, 0.8), "npc"),  # Span from the left to the right of the plot
  y = unit(c(y_pos, y_pos), "npc"),  # Positioned at the middle, change this value as needed
  gp = gpar(
    col = "#2f2421",      # Line color
    lwd = unit(0.5, "mm")   # Line width in millimeters
  )
)



# Vertical line grob
x_pos = 0.485
vline_grob <- linesGrob(
  x = unit(c(x_pos, x_pos), "npc"),  # Positioned at the middle, change this value as needed
  y = unit(c(0.75, 0.9), "npc"),  # Span from the bottom to the top of the plot
  gp = gpar(
    col = "#2f2421",     # Line color
    lwd = unit(0.5, "mm")   # Line width in millimeters
  )
)


  
# Add the histograms onto the base plot
# Here you need to determine the placement coordinates for the histograms
final_plot <- base_plot +
  annotation_custom(ggplotGrob(hist1a), xmin=0.5, xmax=0.8, ymin=0.76, ymax=1) +
  annotation_custom(ggplotGrob(hist1b), xmin=0.2233, xmax=0.5233, ymin=0.76, ymax=1) +
  annotation_custom(ggplotGrob(hist2a), xmin=0.5, xmax=0.77, ymin=0.0, ymax=0.425) +
  annotation_custom(ggplotGrob(hist2b), xmin=0.195, xmax=0.465, ymin=0.0, ymax=0.425) +
  annotation_custom(hline_grob) +
  annotation_custom(hline_grob_2) +
  annotation_custom(hline_grob_3) +
  annotation_custom(vline_grob)


print(final_plot)





# Turnover of species across elevations: ------------------------------------------------

library(tidyr)
library(dplyr)
library(reshape2)

# Filter df by season - do not get rid of duplicates - they will be used
dry_df <- filter(df, season == "DRY")
wet_df <- filter(df, season == "WET")

# Turn the elevation column into a wide format, ready for melting
plant_species_wide_dry <- dry_df %>%
  mutate(Presence = 1) %>%
  spread(key = elevation, value = Presence, fill = 0)

plant_species_wide_wet <- wet_df %>%
  mutate(Presence = 1) %>%
  spread(key = elevation, value = Presence, fill = 0)

# Create a unique identifier that combines species code and season
df_1 <- df %>%
  unite(col = "sp_season_code", c("sp_code", "season"), sep = "_")

plant_species_wide <- df_1 %>%
  mutate(Presence = 1) %>%
  spread(key = elevation, value = Presence, fill = 0)

# Melt the data frame for ggplot2 heatmap
species_melted_dry <- melt(select(plant_species_wide_dry, "sp_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_code"))
species_melted_wet <- melt(select(plant_species_wide_wet, "sp_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_code"))
species_melted <- melt(select(plant_species_wide, "sp_season_code", "650", "1100", "1450", "2250"), value.name = "elevation", id.vars = c("sp_season_code"))

# Separate the combined species and season code back into two columns
species_melted <- species_melted %>%
  separate(col = sp_season_code, into = c("sp_code", "season"), sep = "_")

# Plotting the heatmap for species turnover
ggplot(species_melted, aes(x = variable, y = sp_code, fill = elevation)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~season, ncol = 1) + # Separate by Season
  theme_minimal() +
  labs(x = "Elevation", y = "Species", fill = "Presence") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create UPSET plot to visualize turnover: ------------------------------------------------------------------------------

install.packages("UpSetR")
library(UpSetR)
detach("package:ComplexUpset", unload=TRUE)


df_2 <- df %>%
  mutate(Presence = 1)

# Now, create a unique identifier for each combination of season and elevation
df_2 <- df_2 %>%
  unite("SeasonElevation", season, elevation, sep = "_")

# Next, pivot the data to wide format to get binary indicators
species_binary_matrix <- df_2 %>%
  pivot_wider(
    id_cols = sp_code,
    names_from = SeasonElevation,
    values_from = Presence,
    values_fill = list(Presence = 0) # fill absent combinations with 0
  )

species_binary_matrix <- data.frame(lapply(species_binary_matrix[,-1], function(x) as.numeric(as.character(x))))
# Assuming df is your data frame and 'column_name' is the name of the column you want to turn into row names
row.names(species_binary_matrix) <- species_binary_matrix$sp_code

# After setting the row names, you might want to remove that column from the dataframe
species_binary_matrix$sp_code <- NULL


upset(species_binary_matrix, sets = c("DRY_650","DRY_1100", "DRY_1450", "DRY_2250"), keep.order=TRUE, set_size.show = TRUE)
upset(species_binary_matrix, sets = c("WET_650","WET_1100", "WET_1450", "WET_2250"), keep.order=TRUE, set_size.show = TRUE)

# Turnover via jaccard and correlation amtrix --------------------------------------------------------------------------------

library(vegan)

# Assuming 'species_data' is a matrix with species as rows and sites (elevations) as columns
# and the values are presence (1) or absence (0) data.

# Calculate Jaccard distance (a measure of beta diversity)
jaccard_dist <- vegdist(select(plant_species_wide, "650", "1100", "1450", "2250"), method = "jaccard")

# Perform cluster analysis
species_cluster <- hclust(jaccard_dist)

# Plot the dendrogram
plot(species_cluster)

# Specialization indices: -------------------------------------------------------------------------------------------

library(bipartite)

db_path <- "Data/Dataset.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Set query
query <- "SELECT * FROM combined"  # Adjust your SELECT query as needed
visits <- dbGetQuery(con, query)
dbDisconnect()

# FIlter visists
visits <- visits[,4:46]
visits <- filter(visits, include_in_network == 1)

# Prepare data frames for each season
visits_dry <- filter(visits, season == "DRY")
visits_wet <- filter(visits, season == "WET")

# Lists fro itterations
list_of_dataframes <- list(visits_dry, visits_wet)
color_list <- list("orange", "steelblue")
season_list <- list("DRY", "WET")
elevation_list_strings <- as.list(as.character(unique(visits$elevation)))

# Lists of webs
webs_order <- list()
webs_group <- list()

#For every season
for (i in seq_along(list_of_dataframes)) {
  
  # Current dataframe
  current_df <- list_of_dataframes[[i]]
  
  webs_order[[i]]<-frame2webs(current_df, varnames = c("sp_code", "insect_order", "elevation", "freq_fm_spp"), type.out = "list", emptylist = TRUE)
  webs_group[[i]]<-frame2webs(current_df, varnames = c("sp_code", "functional_group", "elevation", "freq_fm_spp"), type.out = "list", emptylist = TRUE)
  
}

#Visualize web
for (i in 1:2) {
  for(j in 1:4) {
    
    web <- webs_order[[i]][[j]]
    
    plotweb(web, method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
            y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
            col.interaction=color_list[[i]], col.high = "gray0", col.low="forestgreen", bor.col.interaction ="black", 
            bor.col.high="white", bor.col.low="forestgreen", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
            low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
            bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
            adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
            high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
  
  }
}

joined_dfs_list <- list()

# Each season
for (i in 1:2) {
  
  # Current dataframe
  current_df <- list_of_dataframes[[i]]
    
  # Each elevation
  for(j in 1:4) {
    
    filtered_df <- filter(current_df, elevation == as.numeric(rev(elevation_list_strings)[[j]]))
    
    web <- webs_group[[i]][[j]]
    
    plotweb(web, method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
            y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
            col.interaction=color_list[[i]], col.high = "gray0", col.low="forestgreen", bor.col.interaction ="black", 
            bor.col.high="white", bor.col.low="forestgreen", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
            low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
            bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
            adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
            high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
    
    # Add a title after creating the plot
    title(main = elevation_list_strings[[j]], col.main = "black", font.main = 2)
    
    indices <- specieslevel(web, index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                             PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
    # Convert row names of df2 to a column
    indices$sp_code <- rownames(indices)
    joined_dfs_list[[j]] <- merge(filtered_df %>% distinct(sp_code, .keep_all = TRUE), select(indices, sp_code, d), by = "sp_code")
    
    
  }
  
  list_of_dataframes[[i]] <- do.call(rbind, joined_dfs_list)
  
}

plants_with_indices <- do.call(rbind, list_of_dataframes)[,-c(5:42)]

for (i in 1:2) {
  for(j in 1:4) {
    p <- ggplot(filter(plants_with_indices, season == season_list[[i]], elevation == elevation_list_strings[[j]]), aes(x=d)) +
      geom_histogram(bins=10, fill="blue", color="black") +
      labs(x="Specialization", y="Frequency", title = paste(season_list[[i]], " ", elevation_list_strings[[j]])) +
      theme_minimal()
    print(p)
  }
}

