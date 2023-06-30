##### Setup -----
## Attach required packages
library(ggplot2)
library(bggum)
library(coda)

##### Prepare data -----
## Read in data on outcomes votes, i.e. voting on who wins
outcome_votes = t(as.matrix(read.csv("votes.csv", row.names = 1)))
## Read in data on opinion votes, i.e. who joined the majority opinion
opinion_votes = t(as.matrix(read.csv("votes-alt.csv", row.names = 1)))
## Remove unanimous votes
unanimous = apply(outcome_votes, 2, function(x) length(unique(na.omit(x))) == 1)
outcome_votes = outcome_votes[ , !unanimous]
unanimous = apply(opinion_votes, 2, function(x) length(unique(na.omit(x))) == 1)
opinion_votes = opinion_votes[ , !unanimous]

##### Generate ideology estimates -----
## Set the seed for reproducibility
set.seed(138)
## Tune proposals
outcome_proposals = tune_proposals(outcome_votes, tune_iterations = 5000)
opinion_proposals = tune_proposals(opinion_votes, tune_iterations = 5000)
## Tune the temperature schedule
outcome_temps = tune_temperatures(
    data = outcome_votes,
    n_temps = 6,
    proposal_sds = outcome_proposals
)
opinion_temps = tune_temperatures(
    data = opinion_votes,
    n_temps = 6,
    proposal_sds = opinion_proposals
)
## Generate posterior samples
outcome_samples = lapply(1:2, function(x) {
    ggumMC3(
        data = outcome_votes,
        sample_iterations = 250000,
        burn_iterations = 50000,
        proposal_sds = outcome_proposals,
        temps = outcome_temps
    )
})
opinion_samples = lapply(1:2, function(x) {
    ggumMC3(
        data = opinion_votes,
        sample_iterations = 250000,
        burn_iterations = 50000,
        proposal_sds = opinion_proposals,
        temps = opinion_temps
    )
})
## Choose a reflective posterior mode (using Alito as the constraint)
outcome_samples = lapply(
    outcome_samples, post_process, constraint = 3, expected_sign = "+"
)
opinion_samples = lapply(
    opinion_samples, post_process, constraint = 3, expected_sign = "+"
)
## Convergence diagnostics
diagnostics = gelman.diag(mcmc.list(outcome_samples[[1]], outcome_samples[[2]]))
diagnostics$mpsrf
diagnostics = gelman.diag(mcmc.list(opinion_samples[[1]], opinion_samples[[2]]))
diagnostics$mpsrf
## Summarize the posterior
outcome_summary = summary(outcome_samples)
opinion_summary = summary(opinion_samples)

##### Plot results -----
lvl = c(
    "Alito", "Thomas", "Gorsuch",
    "Barrett", "Kavanaugh", "Roberts",
    "Kagan", "Sotomayor", "Jackson"
)
dat = data.frame(
    jnm = rownames(outcome_votes),
    est = outcome_summary$statistics[1:9, "Mean"],
    lwr = outcome_summary$statistics[1:9, "Quantile 0.025"],
    upr = outcome_summary$statistics[1:9, "Quantile 0.975"]
)
dat$jnm = factor(dat$jnm, levels = lvl)
write.csv(dat, file = "ideology-estimates.csv", row.names = FALSE)
ttl = "Justice ideology estimates & 95% CI, 2022 term"
cap = "@SentientPotato6"
plt = ggplot(data = dat, mapping = aes(x = est, y = jnm)) +
    geom_point(size = 2) +
    geom_segment(aes(x = lwr, xend = upr, yend = jnm), linewidth = 0.75) +
    labs(title = ttl, caption = cap) +
    theme_bw() +
    theme(
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0, color = "#5f5f5f"),
        plot.title = element_text(size = 10)
    )
ggsave(
    plot = plt,
    filename = "outcome-ideology.png",
    height = 675 / 300,
    width = 1200 / 300
)
dat = data.frame(
    jnm = rownames(opinion_votes),
    est = opinion_summary$statistics[1:9, "Mean"],
    lwr = opinion_summary$statistics[1:9, "Quantile 0.025"],
    upr = opinion_summary$statistics[1:9, "Quantile 0.975"]
)
dat$jnm = factor(dat$jnm, levels = lvl)
plt = ggplot(data = dat, mapping = aes(x = est, y = jnm)) +
    geom_point(size = 2) +
    geom_segment(aes(x = lwr, xend = upr, yend = jnm), linewidth = 0.75) +
    labs(title = ttl, caption = cap) +
    theme_bw() +
    theme(
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0, color = "#5f5f5f"),
        plot.title = element_text(size = 10)
    )
ggsave(
    plot = plt,
    filename = "opinion-ideology.png",
    height = 675 / 300,
    width = 1200 / 300
)
