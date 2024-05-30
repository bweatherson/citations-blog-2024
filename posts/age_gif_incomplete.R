require(tidyverse)
require(igraph)
require(gganimate)

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
  mutate(ratio = year_cites*1000/all_cites) |>
  mutate(citation_age = cited_year - citing_year) |>
  filter(citation_age <= 2)

for (jjj in 2001:2004){
  
  thecolor <- hcl(h = (jjj-1999)*(360/20)+15, l = 65, c = 100)
  
  animation_bg <- cite_count_by_pair_year %>%
    filter(citing_year < jjj) %>%
    mutate(revealer = -45)
  
  animation_foreground <- cite_count_by_pair_year%>%
    filter(citing_year == jjj) %>%
    mutate(revealer = as.numeric(citation_age))
  
  the_gif <- ggplot(animation_foreground |> drop_na(),
                    aes(x = citation_age, y = ratio, group = citing_year, color = thecolor, na.rm = TRUE)) +
    scale_x_continuous(limits = c(-45, 2),
                       expand = expansion(mult = c(0.01, 0.01)),
                       breaks = (-4:0) * 10) +
    scale_y_continuous(limits = c(0,90),
                       expand = expansion(mult = c(0.01, 0.01)),
                       breaks = 1:10 * 10) +
    geom_line(data = animation_bg, size = 0.2, alpha = 0.5, color = "grey70", na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE, color = thecolor) +
    geom_line(size = 1, na.rm = TRUE, color = thecolor) +
    theme_minimal() +
    theme(legend.position = "None",
          panel.grid.major = element_line(color = "grey85", size = 0.1),
          panel.grid.minor = element_line(color = "grey85", size = 0.05),
          plot.title = element_text(size = rel(2), 
                                    face = "bold",
                                    margin = margin(0, 0, 5, 0)),
          axis.text = element_text(size = rel(1.5))) +
    labs(title = jjj, x = NULL, y = NULL) +
    transition_reveal(citation_age)
  
  gifname <- paste0(jjj,".gif", sep = "")
  
  animate(the_gif, 
          nframes = 20, 
          start_pause = 0, 
          end_pause = 13, 
          duration = 3.6, 
          fps = 25, 
          detail = NULL, 
          width = 480,
          height = 480,
          gifski_renderer(gifname))
}

jjj <- 2000

thecolor <- hcl(h = (jjj-1999)*(360/20)+15, l = 65, c = 100)

#animation_bg <- cite_count_by_pair_year %>%
#  filter(citing_year < jjj) %>%
#  mutate(revealer = 2000)

animation_foreground <- cite_count_by_pair_year%>%
  filter(citing_year == jjj) %>%
  mutate(revealer = as.numeric(citation_age))

the_gif <- ggplot(animation_foreground |> drop_na(),
                  aes(x = citation_age, y = ratio, group = citing_year, color = thecolor, na.rm = TRUE)) +
  scale_x_continuous(limits = c(-45, 2),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = (-4:0) * 10) +
  scale_y_continuous(limits = c(0,90),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = 1:10 * 10) +
#  geom_line(data = animation_bg, size = 0.2, alpha = 0.5, color = "grey70", na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE, color = thecolor) +
  geom_line(size = 1, na.rm = TRUE, color = thecolor) +
  theme_minimal() +
  theme(legend.position = "None",
        panel.grid.major = element_line(color = "grey85", size = 0.1),
        panel.grid.minor = element_line(color = "grey85", size = 0.05),
        plot.title = element_text(size = rel(2), 
                                  face = "bold",
                                  margin = margin(0, 0, 5, 0)),
        axis.text = element_text(size = rel(1.5))) +
  labs(title = jjj, x = NULL, y = NULL) +
  transition_reveal(citation_age)

gifname <- paste0(jjj,".gif", sep = "")

animate(the_gif, 
        nframes = 20, 
        start_pause = 0, 
        end_pause = 13, 
        duration = 3.6, 
        fps = 25, 
        detail = NULL, 
        width = 480,
        height = 480,
        gifski_renderer(gifname))

commd <- "2000.gif"

for (jjj in 2001:2004){
  commd <- paste0(commd," ",jjj,".gif")
}

commd