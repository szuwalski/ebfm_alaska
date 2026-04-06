
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# Read the coverage/index file created from AFCA manifests
idx <- read_csv("data/alaska_salmon_coverage_1970_present.csv", show_col_types = FALSE)

# Helper to extract annual total from a single JSON file
get_annual_total <- function(url) {
  x <- tryCatch(fromJSON(url), error = function(e) NULL)
  if (is.null(x)) {
    return(NA_real_)
  }

  # Prefer metadata$totalCount when present
  if (!is.null(x$metadata) && "totalCount" %in% names(x$metadata)) {
    return(as.numeric(x$metadata$totalCount[[1]]))
  }

  # Otherwise sum the FISHCOUNT column
  if (!is.null(x$COLUMNS) && !is.null(x$DATA)) {
    cols <- unlist(x$COLUMNS)
    dat <- as.data.frame(x$DATA, stringsAsFactors = FALSE)
    names(dat) <- cols[seq_len(ncol(dat))]
    if ("FISHCOUNT" %in% names(dat)) {
      return(sum(as.numeric(dat$FISHCOUNT), na.rm = TRUE))
    }
  }

  NA_real_
}

# Pull all annual totals
out <- idx %>%
  mutate(
    annual_count = map_dbl(raw_url, get_annual_total)
  )

write_csv(out, "alaska_salmon_annual_counts_1970_present.csv")

# Optional summary by stock/species
summary_out <- out %>%
  group_by(dataset, location_id, stock, region, species, year) %>%
  summarise(annual_count = first(annual_count), .groups = "drop")

write_csv(summary_out, "alaska_salmon_annual_counts_summary_1970_present.csv")

library(ggplot2)
ggplot(summary_out)+
  geom_line(aes(x=year,y=log(annual_count),col=stock))+
  facet_wrap(~species)+
  theme_bw()+
  theme(legend.position='none')


summary_out_2<-summary_out%>%
  group_by(year,species)%>%
  summarize(tot_count=sum(annual_count,na.rm=T))

ggplot(filter(summary_out_2,year<2026))+
  geom_line(aes(x=year,y=log(tot_count),col=species))+
  theme_bw()

library(dplyr)
library(ggplot2)

year_min <- 1970
year_max <- max(dat$year, na.rm = TRUE)

target_years <- seq(year_min, year_max)
n_target_years <- length(target_years)
min_years_required <- ceiling(0.75 * n_target_years)


dat<-summary_out
# locations retained for each species
locations_75 <- dat %>%
  filter(
    year >= year_min,
    year <= year_max,
    !is.na(species),
    !is.na(location_id),
    !is.na(annual_count)
  ) %>%
  group_by(species, location_id, stock, region) %>%
  summarise(
    n_years_present = n_distinct(year),
    prop_years_present = n_years_present / n_target_years,
    .groups = "drop"
  ) %>%
  filter(n_years_present >= min_years_required)

# aggregate time series by species using only retained locations
species_ts_75 <- dat %>%
  filter(
    year >= year_min,
    year <= year_max,
    !is.na(species),
    !is.na(location_id),
    !is.na(annual_count)
  ) %>%
  semi_join(
    locations_75 %>% select(species, location_id),
    by = c("species", "location_id")
  ) %>%
  group_by(species, year) %>%
  summarise(
    annual_count = sum(annual_count, na.rm = TRUE),
    n_locations_included = n_distinct(location_id),
    .groups = "drop"
  ) %>%
  arrange(species, year)

ggplot(species_ts_75, aes(year, annual_count)) +
  geom_line() +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()


library(dplyr)
library(ggplot2)

year_min <- 1970
year_max <- max(dat$year, na.rm = TRUE)

target_years <- seq(year_min, year_max)
n_target_years <- length(target_years)
min_years_required <- ceiling(0.75 * n_target_years)

# retained locations
locations_75 <- dat %>%
  filter(
    year >= year_min,
    year <= year_max,
    !is.na(species),
    !is.na(location_id),
    !is.na(annual_count)
  ) %>%
  group_by(species, location_id, stock, region) %>%
  summarise(
    n_years_present = n_distinct(year),
    prop_years_present = n_years_present / n_target_years,
    .groups = "drop"
  ) %>%
  filter(n_years_present >= min_years_required)

# data for retained locations only
dat_retained <- dat %>%
  filter(
    year >= year_min,
    year <= year_max,
    !is.na(species),
    !is.na(location_id),
    !is.na(annual_count)
  ) %>%
  inner_join(
    locations_75 %>% select(species, location_id, stock, region),
    by = c("species", "location_id", "stock", "region")
  )

ggplot(dat_retained, aes(year, annual_count, group = location_id)) +
  geom_line(linewidth = 0.3, alpha = 0.8) +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()


ggplot(filter(dat_retained,species=="Sockeye"), aes(year, annual_count)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~ species + stock, scales = "free_y") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 7)
  )



library(mgcv)
weights_df <- dat %>%
  group_by(species, location_id) %>%
  summarise(
    mean_count = mean(annual_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    weight = sqrt(mean_count),
    weight = weight / mean(weight, na.rm = TRUE)
  )

dat_w <- dat %>%
  left_join(weights_df, by = c("species", "location_id"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

# choose species
sp <- "Sockeye"

# prepare data
dat_sp <- dat_w %>%
  filter(species == sp, !is.na(annual_count), annual_count >= 0) %>%
  mutate(
    location_id = factor(location_id),
    location_lab = ifelse(
      is.na(stock) | stock == "NA",
      paste0("ID ", location_id),
      paste0(stock, " (", location_id, ")")
    ),
    location_lab = factor(location_lab)
  )

# fit weighted GAM
mod <- gam(
  log(annual_count + 1) ~
    s(year, k = 10) +
    s(year, location_lab, bs = "fs", m = 1),
  data = dat_sp,
  weights = weight,
  method = "REML"
)

library(tidyr)
# prediction grid
pred_grid <- dat_sp %>%
  distinct(location_lab) %>%
  crossing(
    year = seq(min(dat_sp$year), max(dat_sp$year))
  )

# population predictions
pred <- predict(mod, newdata = pred_grid, se.fit = TRUE)

pred_grid <- pred_grid %>%
  mutate(
    fit = exp(pred$fit) - 1,
    lwr = pmax(0, exp(pred$fit - 1.96 * pred$se.fit) - 1),
    upr = pmax(0, exp(pred$fit + 1.96 * pred$se.fit) - 1)
  )

# shared trend
shared_grid <- data.frame(
  year = seq(min(dat_sp$year), max(dat_sp$year)),
  location_lab = levels(dat_sp$location_lab)[1]
)

shared_pred <- predict(
  mod,
  newdata = shared_grid,
  se.fit = TRUE,
  exclude = "s(year,location_lab)"
)

shared_grid <- shared_grid %>%
  mutate(
    fit = exp(shared_pred$fit) - 1,
    lwr = pmax(0, exp(shared_pred$fit - 1.96 * shared_pred$se.fit) - 1),
    upr = pmax(0, exp(shared_pred$fit + 1.96 * shared_pred$se.fit) - 1)
  )

# plot
ggplot() +
  geom_point(
    data = dat_sp,
    aes(year, annual_count),
    alpha = 0.5,
    size = 0.7
  ) +
  geom_ribbon(
    data = pred_grid,
    aes(year, ymin = lwr, ymax = upr),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_grid,
    aes(year, fit),
    linewidth = 0.4
  ) +
  geom_line(
    data = shared_grid,
    aes(year, fit),
    color = "red",
    linewidth = 0.6
  ) +
  facet_wrap(~ location_lab, scales = "free_y") +
  theme_bw() +
  labs(
    title = paste("Weighted GAM fits with uncertainty —", sp),
    x = "Year",
    y = "Annual count",
    subtitle = "Black = population fit, Red = shared trend"
  )
