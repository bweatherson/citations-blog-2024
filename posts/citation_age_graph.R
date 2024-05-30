require(tidyverse)


load("philo_cite_years.RData")
load("philo_bib.RData")

cite_count_by_year <- philo_cite_years |>
  group_by(citing_year) |>
  tally(name ="all_cites")

cite_count_by_pair_year <- philo_cite_years |>
  group_by(cited_year, citing_year) |>
  tally(name = "year_cites") |>
  ungroup() |>
  complete(cited_year, citing_year, fill = list(year_cites = 0)) |>
  group_by(citing_year) |>
  mutate(all_cites = sum(year_cites)) |>
  mutate(ratio = year_cites*100/all_cites) |>
  mutate(citation_age = citing_year - cited_year) |>
  filter(citation_age >= -2)

cite_count_by_age <- cite_count_by_pair_year |>
  group_by(citation_age) |>
  summarise(cites = sum(year_cites))

ggplot(cite_count_by_pair_year, aes(citation_age, ratio, group = citing_year, color = as.factor(citing_year))) +
  geom_line() +
  scale_x_continuous(trans = "reverse") +
  theme_minimal() +
  labs(x = "Age in years", y = "Percentage of citations with that age", color = "Citing year")

ggplot(cite_count_by_age, aes(citation_age, cites)) +
  geom_line() +
  scale_x_continuous(trans = "reverse") +
  theme_minimal() +
  labs(x = "Age in years", y = "Citations")

median_cite_age_by_year <- philo_cite_years |>
  mutate(citation_age = citing_year - cited_year) |>
  ungroup() |>
  group_by(citing_year) |>
  summarise(cite_age = median(citation_age))