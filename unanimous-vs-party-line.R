## Attach required packages
library(dplyr)
library(tidyr)
library(ggplot2)

## Get justices' votes from term 2000 to the present
scdb = read.csv("SCDB_2022_01_justiceCentered_Docket.csv") %>%
    mutate(justiceName = gsub("^[A-Z]+([A-Z][a-z]+)$", "\\1", justiceName)) %>%
    mutate(justiceName = gsub("Connor", "O'Connor", justiceName)) %>%
    mutate(majority = majority - 1) %>%
    filter(term >= 2000) %>%
    select(justiceName, docket, majority, term)
votes = read.csv("votes.csv") %>%
    pivot_longer(!docket, names_to = "justiceName", values_to = "majority") %>%
    mutate(term = 2022)
dat = bind_rows(scdb, votes)

## Determine how many unanimous and party-line votes occurred per term
partisanship = read.csv("justice-partisanship.csv")
dat = dat %>%
    left_join(partisanship, by = join_by(justiceName == justice)) %>%
    filter(!is.na(majority)) %>%
    group_by(docket) %>%
    summarise(
        unanimous = all(majority == 1),
        party_line = length(unique(ideology[majority == 1])) == 1,
        term = first(term)
    ) %>%
    group_by(term) %>%
    summarise(
        unanimous = mean(unanimous),
        party_line = mean(party_line)
    ) %>%
    pivot_longer(!term) %>%
    mutate(name = ifelse(name == "unanimous", "Unanimous", "Party Line"))

## Plot results
pal = c("Unanimous" = "#0072b2", "Party Line" = "#d55e00")
map = aes(x = term, y = value, color = name)
ttl = "What % of cases are unanimous or party-line votes?"
cap = "@SentientPotato6"
plt = ggplot(data = dat, mapping = map) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = pal, name = "") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = ttl, caption = cap) +
    theme_bw() +
    theme(
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0, color = "#5f5f5f", size = 6),
        plot.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)
    )
ggsave(
    plot = plt,
    filename = "unanimous-vs-party-line.png",
    height = 675 / 300,
    width = 1200 / 300
)
