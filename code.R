library(dplyr)
library(tidyr)
library(igraph)
library(nflfastR)
df <- read.csv("qss41_final_data.csv", stringsAsFactors = FALSE)
table(df$position)
#Create a function that turns our data into connections between coaches based on team and year
create_coach_connections <- function(data) {
  connections <- data %>%
    group_by(year, team) %>% #Group data by year and team
    summarise(pairs = list(as.data.frame(t(combn(name, 2)))), .groups = "drop") %>% 
    unnest(pairs) %>% 
    rename(Coach_1 = V1, Coach_2 = V2) 
  
  connections <- connections %>%
    left_join(df, by = c("year", "team", "Coach_1" = "name")) %>% 
    left_join(df, by = c("year", "team", "Coach_2" = "name"), suffix = c("_1", "_2")) 
  
  return(connections)
}

coach_connections <- create_coach_connections(df)

#Convert our variables to binary
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
  arrange(year, .by_group = TRUE) %>%  #ensure correct seasonal order
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

node_color <- rgb(0, 0, 1, alpha = 0.2)  #Blue with 50% opacity
edge_color <- rgb(0.5, 0.5, 0.5, alpha = 0.5)  #Gray edges with 50% opacity

plot(g_backbone, 
     vertex.size = 8,
     vertex.label = NA,
     vertex.label.cex = 0.8,        
     vertex.label.dist = 0,         
     vertex.label.color = "black", 
     vertex.frame.color = "white",
     vertex.color = node_color,      #Set node color with opacity
     edge.color = edge_color         #Set edge color with opacity
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

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Kyle Shanahan", "Sean McVay", "Mike McDaniel", "Raheem Morris", "Matt LaFleur", "Dan Quinn", "DeMeco Ryans")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_shanahan)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_shanahan)$name %in% bold_names, V(g_shanahan)$name, NA)
par(cex.main = 2.25)  #Scale title and subtitle if used
plot(g_shanahan, 
     layout = layout.norm(layout_with_fr(g_shanahan)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 1.25,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#AA0000", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#B3995D", alpha.f = 0.5),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Shanahan Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nKyle Shanahan\nSean McVay\nMatt LaFleur\nMike McDaniel\nDeMeco Ryans\nDan Quinn\nRaheem Morris", 
     family = "Helvetica", cex = 1, col = "black")


payton_nodes <- payton_tree$Coach
g_payton <- induced_subgraph(g_backbone, vids = payton_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Sean Payton", "Brian Callahan", "Dan Campbell", "Ben Johnson", "Aaron Glenn", "Zac Taylor")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_payton)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_payton)$name %in% bold_names, V(g_payton)$name, NA)
par(cex.main = 2.25)  #Scale title and subtitle if used

plot(g_payton, 
     layout = layout.norm(layout_with_fr(g_payton)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 1.25,  
     vertex.label.dist = 1,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#fb4f14", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#002244", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Payton Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nSean Payton\nZac Taylor\nDan Campbell\nBrian Callahan\nBen Johnson\nAaron Glenn", 
     family = "Helvetica", cex = 1, col = "black")


reid_nodes <- reid_tree$Coach
g_reid <- induced_subgraph(g_backbone, vids = reid_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Andy Reid", "Nick Sirianni", "Jonathan Gannon")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_reid)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_reid)$name %in% bold_names, V(g_reid)$name, NA)
par(cex.main = 2.25)  #Scale title and subtitle if used

plot(g_reid, 
     layout = layout.norm(layout_with_fr(g_reid)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 1.25,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#E31837", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#FFB81C", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Reid Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nAndy Reid\nNick Sirianni\nJonathan Gannon", 
     family = "Helvetica", cex = 1, col = "black")


belichick_nodes <- belichick_tree$Coach
g_belichick <- induced_subgraph(g_backbone, vids = belichick_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Mike Vrabel", "Bill Belichick")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_belichick)$name %in% bold_names, 2, 1)

plot(g_belichick, 
     layout = layout.norm(layout_with_fr(g_belichick)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = V(g_belichick)$name,   
     vertex.label.cex = 0.7,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#002244", alpha.f = 0.2),  #60% opacity
     edge.color = adjustcolor("#C60C30", alpha.f = 0.3),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Belichick Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nMike Vrabel", 
     family = "Helvetica", cex = 1, col = "black")


rivera_nodes <- rivera_tree$Coach
g_rivera <- induced_subgraph(g_backbone, vids = rivera_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Ron Rivera", "Sean McDermott", "Brian Daboll")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_rivera)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_rivera)$name %in% bold_names, V(g_rivera)$name, NA)
par(cex.main = 2.25)  #Scale title and subtitle if used

plot(g_rivera, 
     layout = layout.norm(layout_with_fr(g_rivera)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#0085CA", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#000", alpha.f = 0.2),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Rivera Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nSean McDermott\nBrian Daboll", 
     family = "Helvetica", cex = 1, col = "black")


carroll_nodes <- carroll_tree$Coach
g_carroll <- induced_subgraph(g_backbone, vids = carroll_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Pete Carroll", "Dave Canales", "Brian Schottenheimer")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_carroll)$name %in% bold_names, 2, 1)
vertex_label <- ifelse(V(g_carroll)$name %in% bold_names, V(g_carroll)$name, NA)
par(cex.main = 2.25)  #Scale title and subtitle if used

plot(g_carroll, 
     layout = layout.norm(layout_with_fr(g_carroll)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = vertex_label,   
     vertex.label.cex = 1,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#A5ACAF", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#000", alpha.f = 0.2),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Carroll Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nPete Carroll\nDave Canales\nBrian Schottenheimer", 
     family = "Helvetica", cex = 1, col = "black")


lewis_zimmer_nodes <- lewis_zimmer_tree$Coach
g_lewis_zimmer <- induced_subgraph(g_backbone, vids = lewis_zimmer_nodes)

#Define default and enlarged font sizes

#Names to bold
bold_names <- c("Marvin Lewis", "Mike Zimmer", "Kevin O'Connell", "Kevin Stefanski")

#Create label font vector: 2 for bold names, 1 for others
vertex_label_font <- ifelse(V(g_lewis_zimmer)$name %in% bold_names, 2, 1)

plot(g_lewis_zimmer, 
     layout = layout.norm(layout_with_fr(g_lewis_zimmer)),  #Normalize layout for better spacing
     vertex.size = 7,                      #Reduce node size slightly
     vertex.label = V(g_lewis_zimmer)$name,   
     vertex.label.cex = 0.9,  
     vertex.label.dist = 0.7,               #Further increase label distance
     vertex.label.color = "black", 
     vertex.label.family = "Helvetica",     
     vertex.label.font = vertex_label_font, 
     vertex.frame.color = "white",
     vertex.color = adjustcolor("#4F2683", alpha.f = 0.3),  #60% opacity
     edge.color = adjustcolor("#FFC62F", alpha.f = 0.5),
     asp = 0, rescale = TRUE,                #Ensure full space is used
     main = "The Lewis-Zimmer Coaching Community",  #Title
     sub = NULL                             #No subtitle
)

#Add caption to bottom-left
text(x = 0.9, y = -0.9, labels = "Current Head Coaches:\nKevin Stefanski\nKevin O'Connell", 
     family = "Helvetica", cex = 1, col = "black")

g_df <- as.undirected(g_df, mode = "collapse")

E(g_df)$weight <- 1/E(g_df)$years + 1/E(g_df)$total_strength


shanahan_node <- which(V(g_df)$name == "Kyle Shanahan")

names(shanahan_numbers) <- V(g_df)$name

paths <- get.shortest.paths(g_df, from = shanahan_node, to = V(g_df), weights = E(g_df)$weight)

shanahan_df <- data.frame(
  Coach = V(g_df)$name,
  Shanahan_Number = sapply(paths$vpath, function(x) sum(E(g_df, path = x)$weight)),
  Path = sapply(paths$vpath, function(x) paste(V(g_df)[x]$name, collapse = " - "))  #Joining coach names in the path
)

shanahan_df <- shanahan_df[shanahan_df$Coach != "Kyle Shanahan", ]
shanahan_df <- shanahan_df[order(shanahan_df$Shanahan_Number, na.last = TRUE), ]

head(shanahan_df)


dist_matrix <- dist(shanahan_df$Shanahan_Number, method = "euclidean")

cluster_result <- hclust(dist_matrix, method = "single")  


k <- 10 
shanahan_df$Cluster <- cutree(cluster_result, k = k)

#View clusters
table(shanahan_df$Cluster)
head(shanahan_df)

test <- left_join(shanahan_df, head_coaches, by = c("Coach" = "Coach"))
test <- na.omit(test)

#Record data ----
pbp <- load_pbp(2010:2024)
standings_data <- nflfastR::calculate_standings(pbp)
standings_data$made_playoffs <- ifelse(is.na(standings_data$seed), 0, 1)
standings_data$won_division <- ifelse(standings_data$seed < 5, 1, 0)
standings_data$won_division <- ifelse(is.na(standings_data$seed), 0, standings_data$won_division)
colnames(standings_data)
final_standings <- select(standings_data, season, team, win_pct, sov, sos, made_playoffs, won_division)

#Getting proper abbreviated names:
teams <- nflfastR::teams_colors_logos
teams_abbr <- select(teams, team_abbr, team_nick)
teams_abbr$team_nick <- ifelse(teams_abbr$team_nick == "Commanders", "Washington", teams_abbr$team_nick)

coach_connections_aggregated$team <- ifelse(coach_connections_aggregated$team == "Bucs", "Buccaneers", coach_connections_aggregated$team )
coach_connections_aggregated$team <- ifelse(coach_connections_aggregated$team == "Commanders", "Washington", coach_connections_aggregated$team)
coach_connections_aggregated$team <- ifelse(coach_connections_aggregated$team == "Ravens ", "Ravens", coach_connections_aggregated$team)
coach_connections_aggregated$team <- ifelse(coach_connections_aggregated$team == "Cardinals ", "Cardinals", coach_connections_aggregated$team)

network_teams <- left_join(coach_connections_aggregated, teams_abbr, by = c("team" = "team_nick"))
sum(is.na(network_teams$team_abbr))

network_standings <- left_join(network_teams, final_standings, by = c("season", "team_abbr" = "team"))
network_standings$weighted_connections <- network_standings$total_strength + network_standings$years

team_agg <- network_standings %>%
  group_by(team_abbr, season, win_pct, sov, sos, made_playoffs, won_division) %>%
  summarize(closeness_score = mean(weighted_connections))

team_agg <- team_agg %>%
  group_by(team_abbr) %>%
  mutate(lag_win_pct = lag(win_pct)) %>%
  ungroup()

#Get the 2009 season standing data
pbp_2009 <- load_pbp(2009)
standings_2009 <- nflfastR::calculate_standings(pbp_2009)
standings_2009 <- select(standings_2009, season, team, win_pct_2009 = win_pct)
standings_2009$season <- 2010
team_agg <- team_agg %>%
  left_join(standings_2009, by = c("season", "team_abbr" = "team")) %>%
  mutate(lag_win_pct = ifelse(is.na(lag_win_pct), win_pct_2009, lag_win_pct)) %>%
  select(-win_pct_2009)  

team_agg <- filter(team_agg, !team_abbr %in% c("OAK", "SD", "LAR", "STL"))

head_coaches <- read.csv("coaches.csv")
head_coaches <- left_join(coach_df, head_coaches, by = c("Coach" = "coach"))
head_coaches <- na.omit(head_coaches)

team_agg <- left_join(team_agg, head_coaches, by = c("team_abbr" = "team", "season" = "year"))
team_agg$Community <- as.factor(team_agg$Community)

shanahans <- select(shanahan_df, Coach, Shanahan_Number)
team_agg <- left_join(team_agg, shanahans)
team_agg$Shanahan_Number <- ifelse(team_agg$Coach == "Kyle Shanahan", 0, team_agg$Shanahan_Number)

summary(lm(win_pct ~ closeness_score + lag_win_pct + sos, data = team_agg))

summary(glm(made_playoffs ~ closeness_score + lag_win_pct + sos, 
            data = team_agg, 
            family = binomial))

summary(glm(won_division ~ closeness_score + lag_win_pct + sos, 
            data = team_agg, 
            family = binomial))
# Visualizing Shanahan ----

#Define a color gradient from white to red
score_colors <- colorRampPalette(c("#ff6363", "#610000"))(length((shanahan_viz$Score)))

#Rank scores, ensuring the highest score gets the furthest ranking
shanahan_viz <- shanahan_viz %>%
  mutate(
    Score_Rank = rank(Score, ties.method = "first"),  #Ensure rank method handles ties
    Score_Color = score_colors[Score_Rank],  #Assign colors based on rank
    Score_Styled = paste0('<span style="background-color:', Score_Color, '; color:white; padding:5px;">', Score, '</span>'),
    Name_Styled = gsub("([^ ]+) ([^ ]+)", "\\1<br>\\2", Name)  #Insert <br> between first and last names
  )


#Create the HTML table
htmlTable(
  shanahan_viz[, c("Name_Styled", "Path", "Score_Styled")], #Ensure correct column selection
  header = c("Coach Name", "Path", "Score"),
  align = "c",
  css.cell = "padding: 8px; border: 1.5px solid black;",
  caption = '<div style="font-size: 20px; font-weight: bold; text-align: center;">How close are major head coaches to Kyle Shanahan?</div>',  #Increase caption size and center it
  escape.html = FALSE  #Ensures HTML styling is interpreted
)
