library(tibble)
library(dplyr)
library(igraph)

#-----------------------------
# 1. Define nodes
#-----------------------------
nodes <- tibble(
  name = c(
    # physical drivers
    "BOTTOM_TEMP", "BLOOM_SIZE",
    
    # species
    "POLLOCK", "SALMON", "CRAB", "COD", "HALIBUT",
    "SOLE", "HERRING", "SABLEFISH", "ATKA",
    
    # fisheries
    "POLLOCK_FISHERY", "SALMON_FISHERY", "CRAB_FISHERY", "COD_FISHERY",
    "HALIBUT_FISHERY", "SOLE_FISHERY", "HERRING_FISHERY",
    "SABLEFISH_FISHERY", "ATKA_FISHERY",
    
    # management
    "NPFMC", "STATE", "IPHC", "JOINT"
  ),
  layer = c(
    rep("physical", 2),
    rep("species", 9),
    rep("fishery", 9),
    rep("management", 4)
  )
)

#-----------------------------
# 2. Define edges
#-----------------------------
edges <- tribble(
  ~from,              ~to,                 ~sign, ~type,
  
  # physical -> species
  "BOTTOM_TEMP",      "CRAB",              "-",   "physical",
  "BOTTOM_TEMP",      "COD",               "+",   "physical",
  "BOTTOM_TEMP",      "SOLE",              "+",   "physical",
  "BOTTOM_TEMP",      "POLLOCK",           "0/?", "physical",
  "BOTTOM_TEMP",      "SALMON",            "0/?", "physical",
  "BOTTOM_TEMP",      "HALIBUT",           "0/?", "physical",
  "BOTTOM_TEMP",      "HERRING",           "0/?", "physical",
  "BOTTOM_TEMP",      "SABLEFISH",         "+",   "physical",
  "BOTTOM_TEMP",      "ATKA",              "0/?", "physical",
  
  "BLOOM_SIZE",       "POLLOCK",           "+",   "physical",
  "BLOOM_SIZE",       "SALMON",            "+",   "physical",
  "BLOOM_SIZE",       "CRAB",              "+",   "physical",
  "BLOOM_SIZE",       "COD",               "+",   "physical",
  "BLOOM_SIZE",       "HERRING",           "+",   "physical",
  "BLOOM_SIZE",       "ATKA",              "+",   "physical",
  
  # species -> species
  "POLLOCK",          "SALMON",            "-",   "ecological",
  "POLLOCK",          "CRAB",              "-",   "ecological",
  "POLLOCK",          "HERRING",           "-",   "ecological",
  "COD",              "CRAB",              "-",   "ecological",
  "COD",              "POLLOCK",           "-",   "ecological",
  "HALIBUT",          "CRAB",              "-",   "ecological",
  "SOLE",             "CRAB",              "-",   "ecological",
  "HERRING",          "SALMON",            "+",   "ecological",
  
  # fisheries -> species
  "POLLOCK_FISHERY",  "POLLOCK",           "-",   "harvest",
  "SALMON_FISHERY",   "SALMON",            "-",   "harvest",
  "CRAB_FISHERY",     "CRAB",              "-",   "harvest",
  "COD_FISHERY",      "COD",               "-",   "harvest",
  "HALIBUT_FISHERY",  "HALIBUT",           "-",   "harvest",
  "SOLE_FISHERY",     "SOLE",              "-",   "harvest",
  "HERRING_FISHERY",  "HERRING",           "-",   "harvest",
  "SABLEFISH_FISHERY","SABLEFISH",         "-",   "harvest",
  "ATKA_FISHERY",     "ATKA",              "-",   "harvest",
  
  # management -> fisheries
  "NPFMC",            "POLLOCK_FISHERY",   "+",   "management",
  "NPFMC",            "COD_FISHERY",       "+",   "management",
  "NPFMC",            "SOLE_FISHERY",      "+",   "management",
  "NPFMC",            "SABLEFISH_FISHERY", "+",   "management",
  "NPFMC",            "ATKA_FISHERY",      "+",   "management",
  
  "STATE",            "SALMON_FISHERY",    "+",   "management",
  "STATE",            "HERRING_FISHERY",   "+",   "management",
  
  "STATE",            "CRAB_FISHERY",      "+",   "management",
  "IPHC",             "HALIBUT_FISHERY",   "+",   "management"
)

#-----------------------------
# 3. Helper to place nodes on a ring
#-----------------------------
place_ring <- function(names_vec, radius, start_angle = 0) {
  n <- length(names_vec)
  angles <- seq(0, 2*pi, length.out = n + 1)[-(n + 1)] + start_angle
  tibble(
    name = names_vec,
    x = radius * cos(angles),
    y = radius * sin(angles)
  )
}

#-----------------------------
# 4. Build ring layout
#-----------------------------
physical_nodes   <- nodes %>% filter(layer == "physical")   %>% pull(name)
species_nodes    <- nodes %>% filter(layer == "species")    %>% pull(name)
fishery_nodes    <- nodes %>% filter(layer == "fishery")    %>% pull(name)
management_nodes <- nodes %>% filter(layer == "management") %>% pull(name)

layout_tbl <- bind_rows(
  place_ring(physical_nodes,   radius = 1.2, start_angle = pi/2),
  place_ring(species_nodes,    radius = 2.8, start_angle = pi/2),
  place_ring(fishery_nodes,    radius = 4.3, start_angle = pi/2),
  place_ring(management_nodes, radius = 5.6, start_angle = pi/4)
) %>%
  left_join(nodes, by = "name")

#-----------------------------
# 5. Build graph
#-----------------------------
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# reorder layout to match graph vertex order
layout_mat <- layout_tbl %>%
  slice(match(V(g)$name, name)) %>%
  select(x, y) %>%
  as.matrix()

#-----------------------------
# 6. Styling
#-----------------------------
# node colors by layer
layer_cols <- c(
  physical   = "gold",
  species    = "skyblue",
  fishery    = "tomato",
  management = "palegreen3"
)

V(g)$color <- layer_cols[layout_tbl$layer[match(V(g)$name, layout_tbl$name)]]
V(g)$size <- c(
  physical = 38,
  species = 28,
  fishery = 24,
  management = 32
)[layout_tbl$layer[match(V(g)$name, layout_tbl$name)]]

V(g)$frame.color <- "gray30"
V(g)$label.cex <- 0.8
V(g)$label.color <- "black"

# edge color by sign
E(g)$color <- case_when(
  edges$sign == "-"   ~ "firebrick",
  edges$sign == "+"   ~ "darkgreen",
  TRUE                ~ "gray50"
)

E(g)$lty <- ifelse(edges$sign == "0/?", 2, 1)
E(g)$width <- case_when(
  edges$type == "management" ~ 2.8,
  edges$type == "harvest"    ~ 2.2,
  edges$type == "ecological" ~ 1.8,
  edges$type == "physical"   ~ 1.5
)
E(g)$arrow.size <- 0.35
E(g)$curved <- 0.12

#-----------------------------
# 7. Plot
#-----------------------------
plot(
  g,
  layout = layout_mat,
  rescale = FALSE,
  xlim = c(-6.5, 6.5),
  ylim = c(-6.5, 6.5),
  margin = 0.05,
  edge.curved = E(g)$curved,
  asp = 0
)

legend(
  "topleft",
  legend = c("Negative", "Positive", "Uncertain"),
  col = c("firebrick", "darkgreen", "gray50"),
  lty = c(1, 1, 2),
  lwd = 2,
  bty = "n"
)

legend(
  "bottomleft",
  legend = c("Physical drivers", "Species groups", "Fisheries", "Management"),
  fill = c("gold", "skyblue", "tomato", "palegreen3"),
  bty = "n"
)

