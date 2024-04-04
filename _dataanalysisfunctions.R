# Functions
### PREPPING DATA

prep_dat <- function(model_dat, what_group, what_keep, topic) {
    switch(what_group,
        realcode =
            pd_tmp <- filter(pac_data, pac_data$RealCode %in% what_keep),
        sector =
            pd_tmp <- filter(pac_data, pac_data$sector %in% what_keep),
        catorder =
            pd_tmp <- filter(pac_data, pac_data$catorder %in% what_keep),
    )
    pd_tmp <- pd_tmp %>%
        group_by(fullname, quarter) %>%
        summarise(
            median_donation = median(median_donation),
            sum_donations = sum(sum_donations),
            mean_donation = round(mean(mean_donation)),
            n_donations = sum(n_donations),
            cycle = first(cycle),
            party = first(party),
            office = first(office),
            sector = first(sector),
            industry = first(industry),
            catname = first(catname),
            catorder = first(catorder),
            sector = first(sector),
            .groups = "drop"
        ) %>%
        ungroup()
    donation_stats <- pd_tmp %>%
        group_by(fullname, quarter) %>%
        summarize(
            lifetime_donations = sum(sum_donations),
            largest_donation = max(sum_donations),
            date_largest_donate = quarter[which.max(sum_donations)],
            date_first_donation = min(quarter),
            .groups = "drop"
        ) %>%
        filter(!is.na(.$lifetime_donations))
    pd_tmp <-
        left_join(
            pd_tmp, donation_stats,
            join_by("fullname", "quarter"),
            relationship = "many-to-one"
        )


    print("part 1")
    df <- join_dat(model_dat, contribs = pd_tmp)
    print("part 2")
    df <- find_time_distances(df)
    print("part 3")
    df <- filter_no_donos(df)
    # extracting speeches / quarter
    toplist <- read_delim("topiclist_newmod.txt", "|")$alltops
    filt_df <- df[, toplist]
    df$n_speeches <- rowSums(filt_df)

    print("part 4")
    df <- create_dv(df, topic)

    return(df)
}


create_dv <- function(df, colname) {
    df <- df %>%
        group_by(fullname, quarter) %>%
        mutate(
            proportion_speeches = sum(.data[[colname]]) / sum(n_speeches)
        ) %>%
        ungroup()
    df <- df %>%
        group_by(fullname) %>%
        fill(party, .direction = "downup") %>%
        mutate(
            prev_quarter_donations = ifelse(
                get_distance(quarter) == get_distance(dplyr::lag(quarter)) + 1,
                dplyr::lag(sum_donations),
                0
            )
        ) %>%
        ungroup()
    return(df)
}

join_dat <- function(temp, contribs) {
    print("Joining Contribs..")
    temp <- left_join(
        temp,
        contribs,
        by = join_by(
            fullname,
            quarter, party
        ),
        relationship = "one-to-one",
        na_matches = "never"
    )
    temp <- ungroup(temp)
    temp <- temp %>%
        replace_na(
            replace = list(
                sum_donations = 0,
                median_donation = 0,
                mean_donation = 0,
                n_donations = 0,
                sector = "none",
                n_speeches = 0,
                realcode = "none"
            )
        ) %>%
        select(!c(
            "lifetime_donations",
            "largest_donation", "date_largest_donate",
            "date_first_donation"
        ))

    return(temp)
}

find_time_distances <- function(df) {
    ## grabs num of quarters from ref_date to date for math operations
    get_distance <- function(date, ref_date = 1995.1) {
        ref <- ref_date %>%
            as.character() %>%
            strsplit("\\.")
        ref_y <- as.integer((sapply(ref, "[", 1)))
        ref_q <- as.integer((sapply(ref, "[", 2)))
        splits <- date %>%
            as.character() %>%
            strsplit("\\.")
        y <- as.integer((sapply(splits, "[", 1))) # year
        q <- as.integer((sapply(splits, "[", 2))) # quarter
        num_quarters <-
            (((y - ref_y) * 4) + (q - ref_q)) ##
        return(num_quarters)
    }
    temp <- df
    ## fixing missing values / adjusting to new grouping
    splits <- temp$quarter %>%
        as.character() %>%
        strsplit("\\.")
    temp$year <- as.integer((sapply(splits, "[", 1)))
    temp$season <- as.integer((sapply(splits, "[", 2)))
    temp_stats <- temp %>%
        group_by(fullname) %>%
        summarize(
            lifetime_donations = sum(sum_donations),
            largest_donation = max(sum_donations),
            date_largest_donate = quarter[which.max(sum_donations)],
            date_first_donation = min(quarter),
            .groups = "drop"
        )
    temp <- left_join(temp, temp_stats,
        by = c("fullname"),
        relationship = "many-to-one"
    )
    temp$next_donation <- ifelse(temp$n_donations > 0, temp$quarter, NA)
    temp <- temp %>%
        mutate(
            last_donation = next_donation
        ) %>%
        arrange(fullname, quarter) %>%
        group_by(fullname) %>%
        fill(next_donation, .direction = "up") %>%
        fill(last_donation, .direction = "down") %>%
        ungroup()
    print("Finding distances: 3 ...")
    temp$quartersuntil_nextdonation <-
        get_distance(temp$quarter) -
        get_distance(temp$next_donation)
    temp$quartersuntil_lastdonation <-
        get_distance(temp$quarter) -
        get_distance(temp$last_donation)
    print("Finding distances: 4 ...")
    temp$first_distance <-
        get_distance(temp$quarter, temp$date_first_donation)
    temp$large_distance <-
        get_distance(temp$quarter, temp$date_largest_donate)
    temp <- temp %>%
        mutate(
            borafter_first = as.factor(ifelse(first_distance >= 0, 1, 0)),
            borafter_large = as.factor(ifelse(large_distance >= 0, 1, 0))
        )
    static_vars <- c(
        "largest_donation", "lifetime_donations",
        "cycle", "office", "lifetime_donations", "largest_donation",
        "date_largest_donate", "date_first_donation",
        "chamber", "state", "gender", "district"
    )
    temp <- as_tibble(temp) %>%
        group_by(fullname) %>%
        fill(all_of(static_vars), .direction = "downup") %>%
        ungroup()

    print("alldone!")

    return(temp)
}

filter_no_donos <- function(df, amount = 500) {
    non_donos <- df %>%
        select(fullname, lifetime_donations) %>%
        filter(lifetime_donations <= amount) %>%
        distinct() %>%
        {
            .$fullname
        }
    match_indicies <- (df$fullname %in% non_donos)
    df <- df[!match_indicies, ] %>%
        filter(.$quarter < 2017.1 &
            .$quarter > 1996 &
            .$quartersuntil_nextdonation > -9 &
            .$quartersuntil_lastdonation < 9)

    return(df)
}

### PLOTTING / ANALYIS
residual_plot <- function(model) {
    residuals_df <- data.frame(fit = fitted(model), residual = residuals(model))
    .title <- paste("Residual Plots For object: ", deparse(substitute(model)))
    print(
        ggplot(residuals_df, aes(x = fit)) +
            geom_histogram(bins = 60) +
            theme_minimal() +
            labs(title = paste(.title, " -- Fitted Values Histogram"))
    )
    print(
        ggplot(residuals_df, aes(x = residual)) +
            geom_histogram(bins = 60) +
            theme_minimal() +
            labs(title = paste(.title, " -- Residual Histogram"))
    )
    print(
        ggplot(residuals_df, aes(x = fit, y = residual)) +
            geom_point() +
            theme_minimal() +
            geom_smooth(
                method = "loess", color = "blue",
                linewidth = 3, fill = "red"
            ) +
            labs(title = paste(.title, " -- Fit:Residual Scatterplot"))
    )
}

get_distance <- function(date, ref_date = 1995.1) {
    ref <- ref_date %>%
        as.character() %>%
        strsplit("\\.")
    ref_y <- as.integer((sapply(ref, "[", 1)))
    ref_q <- as.integer((sapply(ref, "[", 2)))
    splits <- date %>%
        as.character() %>%
        strsplit("\\.")
    y <- as.integer((sapply(splits, "[", 1))) # year
    q <- as.integer((sapply(splits, "[", 2))) # quarter
    num_quarters <-
        (((y - ref_y) * 4) + (q - ref_q)) ##
    return(num_quarters)
}

getyear_fromdist <- function(quarter_int) { ## reference distance is 1995.1
    quarter <- quarter_int %% 4
    year <- 1995 + (quarter_int %/% 4) + (quarter %/% 4)
    quarter <- 1 + quarter
    full_date <- as.numeric((paste0(year, ".", quarter)))
    return(full_date)
}
