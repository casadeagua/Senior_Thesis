library(tidyverse)
library(lubridate)
library(plm)
library(lmtest)
library(stargazer)
library(keyATM)
library(olsrr)
library(sandwich)
library(lmtest)
library(foreign)
library(pscl)


## Loading  ------------------------------------------------------------------------
datastorage_dir <- "/put_file_directory_here/"
setwd(datastorage_dir)
source(file = "./dataanalysisfunctions.R")

all_crp_codes <- read_delim(file = "all_crp_codes.txt", delim = "|")
most_important <- read_csv(file = "most_important.csv")
policy_moods <- read_csv(file = "policy_moods.csv")

pac_data <- vroom::vroom(file = "pacdata_quarter.txt", delim = "|")
pred_matched_data <- vroom::vroom(file = "subsampledata_NEW_1.txt", delim = "|")
toplist <- read_delim("topiclist_newmod.txt", "|")$alltops

# List of CAP Major Topic Codes
# 1 = Macroeconomics
# 2 = Civil Rights, Minority Issues, and Civil Liberties
# 3 = Health
# 4 = Agriculture
# 5 = Labor and Employment
# 6 = Education
# 7 = Environment
# 8 = Energy
# 9 = Immigration
# 10 = Transportation
# 12 = Law, Crime, and Family Issues
# 13 = Social Welfare
# 14 = Community Development and Housing Issues
# 15 = Banking, Finance, and Domestic Commerce
# 16 = Defense
# 17 = Space, Science, Technology, and Communications
# 18 = Foreign Trade
# 19 = International Affairs and Foreign Aid
# 20 = Government Operations
# 21 = Public Lands and Water Management

envi_codes <- c("E2000", "E3000", "E1500", "JE300") ## Codes that align with environmental topic

## Join together topicmodel with PAC data
## prep_dat follows the format: prep_dat(dataframe, c("realcode", "sector"), sector/realcode name(s), topic model topic)
proenvi_mod_match <- prep_dat(pred_matched_data, "realcode", envi_codes, "environment")
energydonos_mod_match <- prep_dat(pred_matched_data, "sector", "energy_nat_resource", "business")
health_mod_match <- prep_dat(pred_matched_data, "sector", "health", "health")
abortion_mod_match <- prep_dat(pred_matched_data, "realcode", "J7120", "religion")
finance_mod_match <- prep_dat(pred_matched_data, "sector", "finance_insur_realest", "tax")
defense_mod_match <- prep_dat(pred_matched_data, "sector", "defense", "defense")
labor_mod_match <- prep_dat(pred_matched_data, "sector", "labor", "labor")

## joining as list for further
datasets <- list(
    "Environmental" = proenvi_mod_match,
    "Energy" = energydonos_mod_match,
    "Health" = health_mod_match,
    "Finance and Real Estate" = finance_mod_match,
    "Defense" = defense_mod_match,
    "Labor" = labor_mod_match
)

majortopic_list <- c(
    7, # Environment
    8, # Energy
    3, # Health
    15, #  Banking, Finance, and Domestic Commerce
    16, #  Defense
    5 # Labor and Employment
)

### Environmental Topic ----------------------------------------------------------------------
### Outputs: 2 LATEX regression tables into Working Directory -- environment topic
i <- 1
ind_name <- names(datasets)[i]
full_tmdat <- most_important %>%
    filter(majortopic == majortopic_list[i]) %>%
    right_join(datasets[[i]], by = "year")
full_tmdat <- full_tmdat %>%
    filter(!is.na(.$prev_quarter_donations) &
        !is.na(.$n_speeches) &
        !is.na(.$proportion_speeches) &
        .$prev_quarter_donations >= 0 &
        .$n_speeches < 150) %>%
    mutate(
        party = ifelse(party == "R" | party == 1, 1, 0),
        logdono = log(prev_quarter_donations + 1)
    )
full_tmdat <- full_tmdat %>%
    mutate(
        term = (year %/% 2) * 2,
        log_lifetime = log(lifetime_donations),
        interact_term_1 = logdono * percent,
        log_speeches = log(n_speeches)
    )
data_2 <- full_tmdat %>%
    filter(large_distance == 1) %>%
    mutate(interact_term = log(largest_donation) * percent)

## Model 1: Donations on Legislator Speech ----------
lm_1 <- plm(
    proportion_speeches ~ logdono,
    data = full_tmdat, index = c("fullname", "term"),
    model = "within", effect = "twoway"
)
lm_2 <- plm(
    proportion_speeches ~
        percent,
    data = full_tmdat, index = c("fullname", "term"),
    model = "within", effect = "twoway"
)
lm_3 <- plm(
    proportion_speeches ~ logdono +
        percent + interact_term_1,
    data = full_tmdat, index = c("fullname", "term"),
    model = "within", effect = "twoway"
)
lm_4 <- plm(
    proportion_speeches ~ logdono +
        percent + interact_term_1 +
        chamber + gender + party + log_speeches + log_lifetime,
    data = full_tmdat, index = c("fullname", "term"),
    model = "within", effect = "twoway",
)
filename <- paste0(gsub(" ", "_", ind_name), "_prevdonos.html")
stargazer(lm_1, lm_2, lm_3, lm_4,
    type = "latex",
    title = paste(
        "Previous Quarter Donations on", ind_name, "Speech"
    ),
    dep.var.labels =
        paste("Proportion of Speeches on", ind_name, "Topic in Quarter"),
    dep.var.caption =
        "Within Member-and-Term Fixed Effects",
    out = filename,
    order = c(
        "logdono",
        "percent",
        "interact_term_1",
        "chamber",
        "log_speeches"
    ),
    covariate.labels = c(
        "Log+1 PAC Donations",
        "Issue Salience (MIP)",
        "Log+1 PAC Donations X MIP",
        "Chamber (Senate)",
        "Log Number of Speeches"
    )
)

## SECOND MODEL: Largest Donation on Legislator Speech ---------------------
largest_mod_1 <- plm(
    proportion_speeches ~ log(largest_donation),
    data = data_2,
    index = c("fullname", "term"), model = "within", effect = "time"
)
largest_mod_2 <- plm(
    proportion_speeches ~ percent,
    data = data_2,
    index = c("fullname", "term"), model = "within", effect = "time"
)
largest_mod_3 <- plm(
    proportion_speeches ~ log(largest_donation) +
        percent + interact_term,
    data = data_2,
    index = c("fullname", "term"), model = "within", effect = "time"
)
largest_mod <- plm(
    proportion_speeches ~ log(largest_donation) +
        percent + interact_term +
        chamber + gender + party + log_speeches + log(lifetime_donations),
    data = data_2,
    index = c("fullname", "term"), model = "within", effect = "time"
)
filename <- paste0(gsub(" ", "_", ind_name), "_largestdono.html")
stargazer(largest_mod_1, largest_mod_2, largest_mod_3, largest_mod,
    type = "latex",
    out = filename,
    title = paste(
        "Size of Largest Donation on ", ind_name, "Speeches"
    ),
    dep.var.labels =
        paste("Proportion of Speeches on", ind_name, "Topic"),
    dep.var.caption =
        "Within Term Fixed Effects",
    order = c(
        "largest_donation",
        "percent",
        "interact_term",
        "lifetime_donations",
        "chamber",
        "gender",
        "party",
        "log_speeches"
    ),
    covariate.labels = c(
        "Log Largest PAC Donation",
        "Issue Salience (MIP)",
        "Log Largest PAC Donation X MIP",
        "Log of Lifetime Donations",
        "Chamber (Senate)",
        "Gender (Male)",
        "Party (Republican)",
        "Log Number of Speeches"
    )
)


## Other Topics: Creating data -------------------------------------------------------
prevdonlist <- list()
largelist <- list()
# adding environment model to this table
prevdonlist[[1]] <- lm_4
largelist[[1]] <- largest_mod

for (i in 2:length(datasets)) {
    ind_name <- names(datasets)[i]
    ## Prep Data
    full_tmdat <- most_important %>%
        filter(majortopic == majortopic_list[i]) %>%
        right_join(datasets[[i]], by = "year")
    full_tmdat <- full_tmdat %>%
        filter(!is.na(.$prev_quarter_donations) &
            !is.na(.$n_speeches) &
            !is.na(.$proportion_speeches) &
            .$prev_quarter_donations >= 0 &
            .$n_speeches < 150) %>%
        mutate(
            party = ifelse(party == "R" | party == 1, 1, 0),
            logdono = log(prev_quarter_donations + 1)
        )
    full_tmdat <- full_tmdat %>%
        mutate(
            term = (year %/% 2) * 2,
            log_lifetime = log(lifetime_donations),
            interact_term_1 = logdono * percent,
            log_speeches = log(n_speeches)
        )
    data_2 <- full_tmdat %>%
        filter(large_distance == 1) %>%
        mutate(interact_term = log(largest_donation) * percent)
    ## Make LMs

    prevdon_lm <- plm(
        proportion_speeches ~ logdono +
            percent + interact_term_1 +
            chamber + gender + party + log_speeches + log_lifetime,
        data = full_tmdat, index = c("fullname", "term"),
        model = "within", effect = "twoway"
    )
    largest_mod <- plm(
        proportion_speeches ~ log(largest_donation) +
            percent + interact_term +
            chamber + gender + party + log_speeches + log(lifetime_donations),
        data = data_2, index = c("fullname", "term"),
        model = "within", effect = "time"
    )

    prevdonlist[[i]] <- prevdon_lm
    largelist[[i]] <- largest_mod
}

listnames <- names(datasets)

## Model 1 Regression Table, with all topics
filename <- "othermodels_prevdono.html"
stargazer(prevdonlist,
    type = "latex",
    title = "Previous Quarter Donations on Speech (Other Topics)",
    column.labels = listnames,
    dep.var.caption = "Within Member-and-Term Fixed Effects. Linear, Panel Model",
    dep.var.labels = "Proportion of Speeches in Quarter",
    out = filename,
    order = c(
        "logdono",
        "percent",
        "interact_term_1",
        "chamber", "log_speeches"
    ),
    covariate.labels = c(
        "Log+1 PAC Donations",
        "Issue Salience (MIP)",
        "Log+1 PAC Donations X MIP",
        "Chamber (Senate)",
        "Log Number of Speeches"
    )
)

## Model 2 Regression Table, with all topics
filename <- "othermodels_largestdono.html"
stargazer(largelist,
    type = "latex",
    title = "Largest Donation on Speech (Other Topics)",
    column.labels = listnames,
    dep.var.caption = "Term Fixed Effects",
    dep.var.labels = "Proportion of Speeches in Quarter",
    out = filename,
    order = c(
        "largest_donation",
        "percent",
        "interact_term",
        "lifetime_donations",
        "chamber",
        "gender",
        "party",
        "log_speeches"
    ),
    covariate.labels = c(
        "Log+1 Largest PAC Donation",
        "Issue Salience (MIP)",
        "Log+1 Largest PAC Donation X MIP",
        "Log of Lifetime Donations",
        "Chamber (Senate)", "Gender (Male)", "Party (Republican)", "Log Number of Speeches"
    )
)


### Other Analysis & Plots ------------------------------------------------------------------------

## Reassigning full_tmdat to evironmental policy
i <- 1
ind_name <- names(datasets)[i]
full_tmdat <- most_important %>%
    filter(majortopic == majortopic_list[i]) %>%
    right_join(datasets[[i]], by = "year")
full_tmdat <- full_tmdat %>%
    filter(!is.na(.$prev_quarter_donations) &
        !is.na(.$n_speeches) &
        !is.na(.$proportion_speeches) &
        .$prev_quarter_donations >= 0 &
        .$n_speeches < 150) %>%
    mutate(
        party = ifelse(party == "R" | party == 1, 1, 0),
        logdono = log(prev_quarter_donations + 1)
    )
full_tmdat <- full_tmdat %>%
    mutate(
        term = (year %/% 2) * 2,
        log_lifetime = log(lifetime_donations),
        interact_term_1 = logdono * percent,
        log_speeches = log(n_speeches)
    )


# Residuals Plots --------------------------
residuals_data <- data.frame(
    Residuals = residuals(lm_4),
    Log_Donations = full_tmdat$log_lifetime,
    MostImportant = full_tmdat$percent,
    Number_Speeches = full_tmdat$n_speeches,
    lifetime = full_tmdat$log_lifetime,
    lognspeeches = log(full_tmdat$n_speeches)
)
n_ind <- which(names(residuals_data) != "Residuals")
residuals_data <- na.omit(residuals_data)

resid1 <- ggplot(residuals_data, aes(x = Log_Donations, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = "Residuals", title = "Prev. Quart. Donations Residuals")
resid2 <- ggplot(residuals_data, aes(x = MostImportant, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = "Residuals", title = "Most Important Problem Residuals")
resid3 <- ggplot(residuals_data, aes(x = Number_Speeches, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = "Residuals", title = "Number Speeches Residuals")
resid4 <- ggplot(residuals_data, aes(x = lifetime, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = "Residuals", title = "Number Speeches Residuals")
resid5 <- ggplot(residuals_data, aes(x = lognspeeches, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = "Residuals", title = "Number Speeches Residuals")

## QQ Plot
ggplot(full_tmdat, aes(sample = log(proportion_speeches + 1))) +
    stat_qq() +
    stat_qq_line(col = "red")

## Display multiple plots at once
gridExtra::grid.arrange(resid1, resid2, resid3)

## Pair Plot ----------------------------
library(GGally)
pp_dat <- select(full_tmdat, c(
    "proportion_speeches", "logdono", "percent",
    "n_speeches", "lifetime_donations"
))
colnames(pp_dat) <- c(
    "Proportion_Speech", "Log_Donations_Prev_Quart",
    "Most_Important_Problem", "Number_Speeches", "Log_Lifetime Donations"
)
names(pp_dat) <- c(
    "Proportion_Speech", "Log_Donations_Prev_Quart",
    "Most_Important_Problem", "Number_Speeches", "Log_Lifetime Donations"
)

pairplots <- ggpairs(pp_dat)
GGally::print_if_interactive(pairplots)


## Various Commands to fix memory issues ------------
# gc()
# if using hpttpgd ----
# httpgd::hgd_close(all = TRUE) ## use this if graph gives "NULL" error
# httpgd::hgd_details()
# httpgd::hgd_view()
# httpgd::hgd_url()
