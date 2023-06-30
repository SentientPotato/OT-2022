## Attach required packages
library(dplyr)
library(tidyr)
library(ggplot2)

## Read in data on opinions from this term & remove the per curiam opinions
opinions = read.csv("opinions.csv")
opinions = opinions[!is.na(opinions$opinion_author), ]

## Get the % of each justices' opinions that every other justice joined
joining_percent = opinions %>%
    group_by(opinion_author) %>%
    summarise(across(Roberts:Jackson, ~ 100 * mean(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = -opinion_author)

## Turn the joining justice names into abbreviations to fit on the x axis
joining_percent = joining_percent %>%
    mutate(name = case_match(
        name,
        "Alito" ~ "SA",
        "Thomas" ~ "CT",
        "Barrett" ~ "ACB",
        "Gorsuch" ~ "NG",
        "Kavanaugh" ~ "BK",
        "Roberts" ~ "JR",
        "Kagan" ~ "EK",
        "Sotomayor" ~ "SS",
        "Jackson" ~ "KBJ"
    ))

## Make the opinion author & joining justice variables ordered factors to
## ensure they will be on the axes in roughly conservative to liberal order
justices = c(
    "Alito", "Thomas", "Barrett", "Gorsuch", "Kavanaugh", "Roberts",
    "Kagan", "Sotomayor", "Jackson"
)
initials = c("SA", "CT", "ACB", "NG", "BK", "JR", "EK", "SS", "KBJ")
joining_percent = joining_percent %>%
    mutate(opinion_author = factor(opinion_author, levels = justices)) %>%
    mutate(name = factor(name, levels = initials))

## Plot results
ttl = "How often does each justice join other justices' opinions?"
cap = "@SentientPotato6"
map = aes(x = name, y = opinion_author, fill = value)
plt = ggplot(data = joining_percent, mapping = map) +
    geom_tile(color = "white", na.rm = TRUE) +
    scale_fill_gradient2(
        limits = c(0, 100),
        high = "#0072b2",
        low  = "#d55e00",
        mid  = "#ffffff",
        midpoint = 50,
        name = "% joined"
    ) +
    xlab("Joining Justice") +
    ylab("Opinion Author") +
    labs(title = ttl, caption = cap) +
    theme(
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0, color = "#5f5f5f", size = 6),
        plot.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)
    )
ggsave(
    plot = plt,
    filename = "opinion-joining-percents.png",
    width = 1200 / 300,
    height = 675 / 300
)
