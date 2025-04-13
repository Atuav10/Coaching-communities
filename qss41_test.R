library(dplyr)
library(tidyr)
library(igraph)
df <- read.csv("qss41_final_data.csv", stringsAsFactors = FALSE)
table(df$position)
# Create a function that turns our data into connections between coaches based on team and year
create_coach_connections <- function(data) {
  connections <- data %>%
    group_by(year, team) %>% # Group data by year and team
    summarise(pairs = list(as.data.frame(t(combn(name, 2)))), .groups = "drop") %>% 
    unnest(pairs) %>% 
    rename(Coach_1 = V1, Coach_2 = V2) 
  
  connections <- connections %>%
    left_join(df, by = c("year", "team", "Coach_1" = "name")) %>% 
    left_join(df, by = c("year", "team", "Coach_2" = "name"), suffix = c("_1", "_2")) 
  
  return(connections)
}

coach_connections <- create_coach_connections(df)

# Convert our variables to binary
coach_connections$is_fired_1 <- ifelse(coach_connections$is_fired_1 != 1, 0, coach_connections$is_fired_1)
coach_connections$is_fired_2 <- ifelse(coach_connections$is_fired_2 != 1, 0, coach_connections$is_fired_2)

coach_connections$is_promoted_1 <- ifelse(is.na(coach_connections$is_promoted_1), 0, coach_connections$is_promoted_1)
coach_connections$is_promoted_2 <- ifelse(is.na(coach_connections$is_promoted_2), 0, coach_connections$is_promoted_2)
coach_connections$is_assistant_hc_1 <- ifelse(is.na(coach_connections$is_assistant_hc_1), 0, coach_connections$is_assistant_hc_1)
coach_connections$is_assistant_hc_2 <- ifelse(is.na(coach_connections$is_assistant_hc_2), 0, coach_connections$is_assistant_hc_2)
coach_connections$is_interim_coordinator_1 <- ifelse(is.na(coach_connections$is_interim_coordinator_1), 0, coach_connections$is_interim_coordinator_1)
coach_connections$is_interim_coordinator_2 <- ifelse(is.na(coach_connections$is_interim_coordinator_2), 0, coach_connections$is_interim_coordinator_2)
coach_connections$is_interim_hc_1 <- ifelse(is.na(coach_connections$is_interim_hc_1), 0, coach_connections$is_interim_hc_1)
coach_connections$is_interim_hc_2 <- ifelse(is.na(coach_connections$is_interim_hc_2), 0, coach_connections$is_interim_hc_2)

coach_connections$rating_1 <- as.numeric(coach_connections$rating_1)
coach_connections$rating_2 <- as.numeric(coach_connections$rating_2)
coach_connections$is_fired_1 <- as.numeric(coach_connections$is_fired_1)
coach_connections$is_fired_1 <- as.numeric(coach_connections$is_fired_1)
coach_connections$is_fired_2 <- as.numeric(coach_connections$is_fired_2)
coach_connections$is_fired_2 <- as.numeric(coach_connections$is_fired_2)
coach_connections$is_assistant_hc_1 <- as.numeric(coach_connections$is_assistant_hc_1)
coach_connections$is_assistant_hc_2 <- as.numeric(coach_connections$is_assistant_hc_2)
coach_connections$is_interim_hc_1 <- as.numeric(coach_connections$is_interim_hc_1)
coach_connections$is_interim_hc_2 <- as.numeric(coach_connections$is_interim_hc_2)
coach_connections$is_interim_coordinator_1 <- as.numeric(coach_connections$is_interim_coordinator_1)
coach_connections$is_interim_coordinator_2 <- as.numeric(coach_connections$is_interim_coordinator_2)
coach_connections$strength <- coach_connections$rating_1 + coach_connections$rating_2


write.csv(coach_connections, "coaches_connections.csv", row.names = FALSE)


#Create pairs of coaches that represent the connections and aggregate it to show the strength of the connection
coach_connections$pair <- apply(coach_connections[, c('Coach_1', 'Coach_2')], 1, function(x) paste(sort(x), collapse = '-'))
coach_connections_aggregated <- coach_connections %>%
  group_by(pair) %>%
  arrange(year, .by_group = TRUE) %>%  # ensure correct seasonal order
  summarise(total_strength = cumsum(strength),
    years = n(),
    season = year,
    team = team
  )

final_coach_connections <- coach_connections_aggregated %>%
  group_by(pair) %>%
  filter(row_number() == which.max(total_strength)) %>%
  ungroup()

final_coach_connections <- final_coach_connections %>%
  mutate(pair = gsub("Andrew Hayes-Stoker", "Andrew Hayes Stoker", pair),
         pair = gsub("Stephen Bravo-Brown", "Stephen Bravo Brown", pair))
df_final <- final_coach_connections %>%
  separate(pair, into = c("Coach1", "Coach2"), sep = "-") 

g_df <- graph.data.frame(df_final, directed = F)

threshold <- 50

g_backbone <- delete_edges(g_df, E(g_df)[total_strength < threshold])
g_backbone <- delete_vertices(g_backbone, V(g_backbone)[degree(g_backbone) == 0])

node_color <- rgb(0, 0, 1, alpha = 0.2)  # Blue with 50% opacity
edge_color <- rgb(0.5, 0.5, 0.5, alpha = 0.5)  # Gray edges with 50% opacity

plot(g_backbone, 
     vertex.size = 8,
     vertex.label = NA,
     vertex.label.cex = 0.8,        
     vertex.label.dist = 0,         
     vertex.label.color = "black", 
     vertex.frame.color = "white",
     vertex.color = node_color,      # Set node color with opacity
     edge.color = edge_color         # Set edge color with opacity
)
coach_mod = igraph::cluster_fast_greedy(g_backbone)
sizes(coach_mod)

coach_membership <- (membership(coach_mod))
coach_df <- data.frame(Coach = names(coach_membership), Community = as.numeric(coach_membership))
coach_membership <- (membership(coach_mod))
coach_df <- data.frame(Coach = names(coach_membership), Community = as.numeric(coach_membership))
shanahan_tree <- filter(coach_df, Community == 5)
reid_tree <- filter(coach_df, Community == 2)
harbaugh_tree <- filter(coach_df, Community == 1)
belichick_tree <- filter(coach_df, Community == 7)
fangio_tree <- filter(coach_df, Community == 3)
mccarthy_tree <- filter(coach_df, Community == 4)
payton_tree <- filter(coach_df, Community == 6)
rivera_tree <- filter(coach_df, Community == 8)
tomlin_tree <- filter(coach_df, Community == 9)
carroll_tree <- filter(coach_df, Community == 10)
garrett_gruden_tree <- filter(coach_df, Community == 13)
lewis_zimmer_tree <- filter(coach_df, Community == 15)
arians_tree <- filter(coach_df, Community == 16) #Joins Mike Tomlin

shanahan_nodes <- shanahan_tree$Coach
g_shanahan <- induced_subgraph(g_backbone, vids = shanahan_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Kyle Shanahan", "Sean McVay", "Mike McDaniel", "Raheem Morris", "Matt LaFleur", "Dan Quinn", "DeMeco Ryans")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_shanahan)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_shanahan)$name %in% bold_names, V(g_shanahan)$name, NA)
par(cex.main = 2.25)  # Scale title and subtitle if used
plot(g_shanahan, 
     layout = layout.norm(layout_with_fr(g_shanahan)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 1.25,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#AA0000", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#B3995D", alpha.f = 0.5),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Shanahan Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nKyle Shanahan\nSean McVay\nMatt LaFleur\nMike McDaniel\nDeMeco Ryans\nDan Quinn\nRaheem Morris", 
     family = "Helvetica", cex = 1, col = "black")


payton_nodes <- payton_tree$Coach
g_payton <- induced_subgraph(g_backbone, vids = payton_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Sean Payton", "Brian Callahan", "Dan Campbell", "Ben Johnson", "Aaron Glenn", "Zac Taylor")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_payton)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_payton)$name %in% bold_names, V(g_payton)$name, NA)
plot(g_payton, 
     layout = layout.norm(layout_with_fr(g_payton)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 1,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#fb4f14", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#002244", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Payton Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nSean Payton\nZac Taylor\nDan Campbell\nBrian Callahan\nBen Johnson\nAaron Glenn", 
     family = "Helvetica", cex = 1, col = "black")


reid_nodes <- reid_tree$Coach
g_reid <- induced_subgraph(g_backbone, vids = reid_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Andy Reid", "Nick Sirianni", "Jonathan Gannon")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_reid)$name %in% bold_names, 2, 1)

plot(g_reid, 
     layout = layout.norm(layout_with_fr(g_reid)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = V(g_reid)$name,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#E31837", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#FFB81C", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Reid Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nAndy Reid\nNick Sirianni\nJonathan Gannon", 
     family = "Helvetica", cex = 1, col = "black")


belichick_nodes <- belichick_tree$Coach
g_belichick <- induced_subgraph(g_backbone, vids = belichick_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Mike Vrabel", "Bill Belichick")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_belichick)$name %in% bold_names, 2, 1)

plot(g_belichick, 
     layout = layout.norm(layout_with_fr(g_belichick)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = V(g_belichick)$name,   
     vertex.label.cex = 0.7,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#002244", alpha.f = 0.2),  # 60% opacity
     edge.color = adjustcolor("#C60C30", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Belichick Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nMike Vrabel", 
     family = "Helvetica", cex = 1, col = "black")


rivera_nodes <- rivera_tree$Coach
g_rivera <- induced_subgraph(g_backbone, vids = rivera_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Ron Rivera", "Sean McDermott", "Brian Daboll")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_rivera)$name %in% bold_names, 2, 1)

plot(g_rivera, 
     layout = layout.norm(layout_with_fr(g_rivera)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = V(g_rivera)$name,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#0085CA", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#000", alpha.f = 0.2),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Rivera Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nSean McDermott\nBrian Daboll", 
     family = "Helvetica", cex = 1, col = "black")


carroll_nodes <- carroll_tree$Coach
g_carroll <- induced_subgraph(g_backbone, vids = carroll_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Pete Carroll", "Dave Canales", "Brian Schottenheimer")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_carroll)$name %in% bold_names, 2, 1)

plot(g_carroll, 
     layout = layout.norm(layout_with_fr(g_carroll)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = V(g_carroll)$name,   
     vertex.label.cex = 1,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#A5ACAF", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#000", alpha.f = 0.2),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Carroll Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nPete Carroll\nDave Canales\nBrian Schottenheimer", 
     family = "Helvetica", cex = 1, col = "black")


lewis_zimmer_nodes <- lewis_zimmer_tree$Coach
g_lewis_zimmer <- induced_subgraph(g_backbone, vids = lewis_zimmer_nodes)

# Define default and enlarged font sizes

# Names to bold
bold_names <- c("Marvin Lewis", "Mike Zimmer", "Kevin O'Connell", "Kevin Stefanski")

# Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_lewis_zimmer)$name %in% bold_names, 2, 1)

plot(g_lewis_zimmer, 
     layout = layout.norm(layout_with_fr(g_lewis_zimmer)),  # Normalize layout for better spacing
     vertex.size = 7,                      # Reduce node size slightly
     vertex.label = V(g_lewis_zimmer)$name,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 0.7,               # Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#4F2683", alpha.f = 0.3),  # 60% opacity
     edge.color = adjustcolor("#FFC62F", alpha.f = 0.5),
     asp = 0, rescale = TRUE,                # Ensure full space is used
     main = "The Lewis-Zimmer Coaching Community",  # Title
     sub = NULL                             # No subtitle
)

# Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nKevin Stefanski\nKevin O'Connell", 
     family = "Helvetica", cex = 1, col = "black")