# All In One Doc
library(tidyverse)
library(quanteda)
library(readxl)
library(keyATM)
library(plm)
library(lubridate)
quanteda_options(verbose = TRUE)

## Directories ----------------------------------------------------------------------------------
datastorage_dir <- "D:/Data/ModifiedFiles/" ## Catch-All Directory
rawspeech_dir <- "D:/Data/DataStorage/SpeechData/heindaily/" ## format: "speeches_##.txt"
map_dir <- "D:/Data/DataStorage/SpeechData/Speakermap/" ## format: "###_SpeakerMap.txt"
descr_path <- "D:/Data/DataStorage/SpeechData/descr/" ## format:


### Speech Metadata (speakermaps) ----------------------------------------------------------------------------------
setwd(map_dir)
filelist <- list.files(pattern = ".txt")

cnames <- c("speakerid", "speech_id", "lastname", "firstname", "chamber", "state", "gender", "party", "district", "nonvoting")
data_0 <- data.frame(matrix(ncol = length(cnames), nrow = 0))
colnames(data_0) <- cnames

for (i in 1:length(filelist)) {
    print(c("Loading:...", filelist[i]))
    x <- vroom::vroom(filelist[i], col_select = all_of(cnames), delim = "|")
    print(c("Cleaning Data for... ", filelist[i]))
    data_0 <- rbind(data_0, x)
}

rm(i)
# Writing File
speech_maps <- data_0
setwd(datastorage_dir)
write_csv(data_0, file = "speakermaps.csv")
rm(data_0)

speech_maps <- vroom::vroom(file = "speakermaps.csv")


### Preprocessing Text Data ----------------------------------------------------------------------------------
# Reading Speech Data
setwd(rawspeech_dir)
filelist <- list.files(pattern = ".txt")
cnames <- c("speech_id", "speech")
data_0 <- data.frame(matrix(ncol = length(cnames), nrow = 0))
colnames(data_0) <- cnames

for (i in 1:length(filelist)) {
    print(c("Loading:...", filelist[i]))
    x <- vroom::vroom(filelist[i], col_select = all_of(cnames), delim = "|")
    data_0 <- rbind(data_0, x)
}
all_speeches <- data_0

# joining with metadata
meta_speeches <- left_join(speech_maps, all_speeches,
    relationship = "one-to-one"
)
# CONVERT TO UTF-8 CHARS -- IMPORTANT
# install.packages("upstartr")
meta_speeches <- meta_speeches %>%
    mutate(speech = upstartr::remove_non_utf8(speech))

## reading Descr_Data
filelist <- list.files(path = descr_path, pattern = ".txt")
cnames <- c(
    "speech_id", "chamber", "date", "number_within_file",
    "speaker", "first_name", "last_name", "state", "gender",
    "line_start", "line_end", "file", "char_count", "word_count"
)
data_0 <- data.frame(matrix(ncol = length(cnames), nrow = 0))
colnames(data_0) <- cnames

for (i in 1:length(filelist)) {
    print(c("Loading:...", filelist[i]))
    x <- vroom::vroom(paste0(descr_path, filelist[i]), col_select = all_of(cnames), delim = "|")
    data_0 <- rbind(data_0, x)
}
descr_data <- data_0

# creating metadata combined
descr_data <- descr_data %>%
    select(!c(
        "number_within_file", "line_start", "line_end", "file", "char_count",
        "state", "gender", "chamber"
    ))

meta_speeches <- left_join(meta_speeches, descr_data,
    relationship = "one-to-one", by = join_by(speech_id),
    suffix = c(".x", "")
)
keep <- c(
    "speakerid", "speech_id", "lastname", "firstname", "party",
    "chamber", "date", "speaker", "state", "district", "gender",
    "word_count", "speech"
)
meta_speeches <- select(meta_speeches, all_of(keep))

write_delim(meta_speeches, file = paste0(datastorage_dir, "allspeeches.txt"), delim = "|")
rm(list = c("i", "data_0"))

meta_speeches <- vroom::vroom(file = paste0(datastorage_dir, "allspeeches.txt"), delim = "|")

## tokenizing and cleaning --------------------
setwd(datastorage_dir)
meta_speeches <- vroom::vroom("allspeeches2.txt")
speech_corpus <- corpus(meta_speeches, text_field = "speech", docid_field = "speech_id")
master_list <- vroom::vroom("C:/Users/forre/Desktop/DATA/Thesis/STOPWORDS/master_list.txt")
valid_bigrams <- filter(master_list, master_list$"_classify" == "vocab")$phrase

speech_dfm <- tokens(speech_corpus,
    remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE,
    split_hyphens = TRUE, include_docvars = TRUE, verbose = TRUE
)
speech_dfm <- speech_dfm %>%
    tokens_tolower() %>%
    tokens_wordstem() %>%
    tokens_remove(stopwords("en"))
speech_dfm <- speech_dfm %>%
    tokens_ngrams(n = 2L, skip = 0, concatenator = " ")
speech_dfm <- tokens_select(speech_dfm, pattern = valid_bigrams)
write_rds(speech_dfm, file = "tokens.rds")

speech_dfm <- dfm(speech_dfm)
trimmed_dfm <- speech_dfm %>%
    dfm_trim(min_termfreq = 0.000007, termfreq_type = "prop") ## Frequency Selected to achieve ~ 15,000 terms per the keyATM recomendations # nolint
trimmed_dfm <- trimmed_dfm[rowSums(trimmed_dfm) > 0, ] ## remove empty rows

write_rds(speech_dfm, file = "speech_bigrams_new.rds")
setwd(rawspeech_dir)
speech_bigrams <- read_rds(file = "speech_bigrams_new.rds")


### Topic Modeling ----------------------------------------------------------------------------------
setwd(datastorage_dir)

topic_phrases <- read_delim("topic_phrases.txt", delim = "|")
topic_phrases <- split(topic_phrases$phrase, topic_phrases$topic)
topic_phrases <- c(
    topic_phrases,
    list(immigration = c(
        "immigr reform", "border patrol",
        "illeg immigr", "nation secur"
    )),
    list(law_enforcement = c(
        "law enforc", "justic system", "enforc offic",
        "rule law", "attorney general", "civil right"
    )),
    list(agriculture = c(
        "farm subsid", "crop insur", "soil health",
        "land us", "agricultur trade", "water manag",
        "food secur", "rural develop", "pesticid regul",
        "climate chang", "sustain agricultur", "price support",
        "crop yield", "livestock product", "agri environment"
    )),
    list(finance = c(
        "financ market", "invest portfoli", "financ institut",
        "econom growth", "monetari polici", "fiscal polici",
        "interest rate", "stock market", "bond market",
        "deriv market", "financ regul", "corpor govern",
        "asset manag", "risk manag", "financ technolog",
        "real estate market", "properti invest", "mortgag loan", "hous market"
    )),
    list(communication_electronics = c(
        "telecommun act", "broadband internet", "telecommun industri",
        "telecommun compani", "telecommun servic", "telecommun industri",
        "broadband access", "internet servic", "satellit communic",
        "data privaci", "cybersecur", "cloud comput", "wireless network",
        "smartphon technolog", "electron devic", "semiconductor industri",
        "comput network", "artifici intellig", "robot technolog",
        "digit transformat", "telephon servic"
    )),
    list(construction = c(
        "infrastructur invest", "transport committe", "transport infrastructur",
        "transport depart", "transport project", "construct industri",
        "build material", "infrastructur develop", "urban plann", "civil engin",
        "architectur design", "construct project", "hous constr", "road constr",
        "bridg constr", "build code", "construct manag", "sustain construct",
        "construct technolog"
    )),
    list(energy = c(
        "fossil fuel", "energi market", "oil ga", "coal industri",
        "power plant", "carbon emiss", "fuel consumpt", "energi produc",
        "reserv estim", "explor dril", "refin process", "energi polic",
        "environment impact", "petroleum product", "climate regul"
    )),
    list(transportation = c(
        "transport industri", "public transport", "road network",
        "air transport", "marit transport", "logist manag", "vehicl manufactur",
        "automobil industri", "urban mobiliti", "freight transport",
        "passeng transport", "infrastructur invest", "transport policy",
        "railway system", "fuel effici"
    ))
)

speech_dfm <- read_rds(file = "speech_bigrams_new.rds")
trimmed_dfm <- speech_dfm %>%
    dfm_trim(min_termfreq = 0.000007, termfreq_type = "prop")
trimmed_dfm <- trimmed_dfm[rowSums(trimmed_dfm) > 0, ]

all_bigrams <- dfm_subset(trimmed_dfm, trimmed_dfm@docvars$date > 19950102)
rm(list = c("trimmed_dfm", "master_list", "speech_dfm", "valid_bigrams", "speech_corpus"))
all_bigrams <- dfm_sort(all_bigrams, decreasing = TRUE, "both")
write_rds(all_bigrams, file = "all_bigrams.rds")

## Create Subsample & convert to keyatm
## ------------------------------------
# sample_indexes <- sample(1:nrow(all_bigrams), 0.7*nrow(all_bigrams))
# write_csv(as.data.frame(sample_indexes), file = "sampled_indexes.csv")
sample_indexes <- read_csv(file = "sampled_indexes.csv")$sample_indexes ##  --training indexes--
training_bigrams <- all_bigrams[sample_indexes, ]

key_bigrams <- keyATM_read(texts = training_bigrams, keep_docnames = TRUE)
write_rds(key_bigrams, file = "keyatm_TRAINING_dfm.rds")

setwd(datastorage_dir)
key_bigrams <- read_rds(file = "keyatm_TRAINING_dfm.rds")

topic_phrases <- lapply(topic_phrases, function(x) gsub(" ", "_", x))
key_bigrams$W_raw <- lapply(key_bigrams$W_raw, function(x) gsub(" ", "_", x))
key_bigrams$wd_names <- as.character(sapply(key_bigrams$wd_names, function(x) gsub(" ", "_", x)))

key_viz <- visualize_keywords(
    docs = key_bigrams,
    keywords = topic_phrases
)
keyval <- values_fig(key_viz) # occurances of each keyword
keyval
# write.csv(keyval, file = "topbigrams.csv")

# keyval <- read_csv("topbigrams.csv")
print("all done!")
filt_values <- keyval %>%
    ungroup() %>%
    filter(.$"Proportion(%)" > 0.005) %>%
    filter(.$Ranking < 11)

filt_values$Topic <- gsub("*[^a-zA-Z]", "", filt_values$Topic)
topic_keywords <- split(filt_values$Word, filt_values$Topic)
topic_keywords <- subset(topic_keywords, sapply(topic_keywords, length) > 1)

rm(list = c(
    "sub_set", "filt_values", "key_viz", "keyval", "topic_phrases",
    "upon_tabl", "reconsid_laid", "offic_build", "tem"
))

write_rds(topic_keywords, file = "topic_keywords.rds")
print("all done pt.2!")

### RUNNING THE MODEL ------------------------------------------------------
small_together_model <- function(save_location,
                                 seed_m,
                                 nloops,
                                 keyword_v = topic_phrases,
                                 n_iteration = 250,
                                 nokey_topics = 2) {
    start_time <- Sys.time()
    print(paste("Model Started at: ", start_time))
    setwd(save_location)
    for (i in 1:nloops) {
        looptime <- Sys.time()
        print(paste("Loop #", i, "Computing!"))

        model <- keyATM( # Runs model with iteration session
            docs = key_bigrams,
            no_keyword_topics = nokey_topics,
            keywords = keyword_v,
            model = "base",
            options = list(
                seed = seed_m,
                iterations = n_iteration,
                verbose = TRUE,
                resume = "keyATM_resume.rds"
            )
        )
        end_time <- Sys.time()
        loop_time <- paste("Loop Time: ", end_time - start_time)
        timelog <- paste(
            "start time: ", start_time, "\n",
            "end time: ", end_time, "\n",
            "total loop time: ", loop_time, "\n",
            "Loop #: ", i, "\n",
            "iterations: ", n_iteration, "\n",
            "nokey_topics: ", nokey_topics, "\n",
            "seed(s): ", seed_m, "\n"
        )
        savefile <- paste0(save_location, "model.rds")
        save.keyATM_output(model, file = savefile)
        files <- list.files(pattern = ".txt")
        le <- length(files)
        write(timelog, file = paste0("time_log_", (le + 1), ".txt"))
    }
    print(paste("Model Finished at: ", end_time))
    print(end_time - start_time)
    return(model)
}

seed <- floor(runif(1, min = 0, max = 1000001))
print(paste("Running with Seed: ", seed))

## model 6 seed: 150296
model_1 <- small_together_model(
    save_location = "D:/Data/ModifiedFiles/keyATM_model_maxtops_1/",
    nloops = 13,
    keyword_v = topic_keywords,
    seed_m = 150296,
    n_iteration = 75, 
)


### After Model 
model_1 <- read_rds(file = "D:/Data/ModifiedFiles/keyATM_model_maxtops_1/model.rds")

## Validating Results ----------------------------------------------------------------------------------
## extract random sample of documents documents into file "speeches_i.txt"
setwd("D:/Data/DataStorage")
setwd(datastorage_dir)

meta_speeches <- vroom::vroom(file = "allspeeches.txt")
topics <- unique(subdat$topic_mode)

for (top in topics) {
    subdat_samp <- subdat %>%
        filter(topic_mode == top)

    random_sample <- sample(subdat_samp$doc_id, 20)
    sample_metdata <- meta_speeches %>%
        filter(speech_id %in% random_sample)

    speeches <- sapply(sample_metdata$speech, function(x) {
        paste("Document: \n", paste(strwrap(x, width = 80), collapse = "\n"))
    }) %>%
        paste0("\n\n")

    writeLines(speeches, paste0("./topic_documents/speeches_", top, ".txt"))
}

### Loading Campaign Finance Data  ----------------------------------------------------------------------------------
parent_directory_pac <- "./Campaign_Finance" ## where the 1994-2014 cycle tables have been placed

search_files <- function(directory, pattern) {
    files <- list.files(directory, recursive = TRUE, full.names = TRUE)
    files <- files[grepl(pattern, files)]
    return(files)
}

file_pattern <- "pacs[0-9]{2}\\.txt"
file_paths <- search_files(parent_directory_pac, file_pattern)
data_list <- list()
header_2 <- c("Cycle", "FECRecNo", "PACID", "CID", "Amount", "Date", "RealCode", "Type", "DI", "FECCandID")

for (file_path in file_paths) {
    data <- read_delim(file_path, delim = ",", col_names = header_2, quote = "|")
    data_list[[file_path]] <- data
}
combined_data <- do.call(rbind, data_list)

## Adding Names ----
setwd("C:/Users/forre/Desktop/DATA/Thesis/CampaignFinance/")
col_names <- c("CID", "CRPName", "Party", "Office", "FECCandID")
all_donations <- combined_data

allnames <- data.frame()
for (i in 106:117) {
    sheetname <- paste0("Members ", i, "th")
    dat <- read_excel("CRP_IDs.xls", sheet = sheetname)
    dat <- dat[-c(1:5), -c(1)]
    colnames(dat) <- col_names
    allnames <- rbind(allnames, dat)
}
allnames <- filter(allnames, !duplicated(CID))
industrycodes <- read_excel("CRP_IDs.xls", sheet = "CRP Industry Codes")[-c(1:5), ]
colnames(industrycodes) <- c("Catcode", "Catname", "Catorder", "Industry", "Sector", "Sector Long")

full_data <- left_join(all_donations, allnames, by = "CID", relationship = "many-to-one")
full_data <- full_data %>%
    filter(DI == "D") %>%
    left_join(industrycodes, by = join_by("RealCode" == "Catcode"))
full_data <- full_data %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    select(!c("FECCandID.x", "FECCandID.y", "FECRecNo"))

## Save to disk
setwd(datastorage_dir)
write_delim(full_data, file = "all_donations.txt", delim = "|")
pac_data <- read_delim(file = "all_donations.txt", delim = "|")

## Prepping DATA for Data_Analysis ----------------------------------------------------------------------------------
# library(lmtest)
setwd(datastorage_dir)
speaker_maps <- vroom::vroom(file = "allspeeches.txt", delim = "|") ## actually metadata
speaker_maps <- select(speaker_maps, !speech)

## pacdata by quarter ------------------------------------------------------------------
pac_data <- vroom::vroom(file = "all_donations.txt", delim = "|")
pacdata_quarter <- pac_data %>%
    mutate(
        quarter = quarter(Date, type = "year.quarter"),
        year = year(Date),
        season = quarter(Date)
    ) %>%
    group_by(CRPName, RealCode, quarter) %>%
    summarise(
        median_donation = median(Amount),
        sum_donations = sum(Amount),
        mean_donation = round(mean(Amount)),
        n_donations = n(),
        cycle = first(Cycle),
        party = first(Party),
        office = first(Office),
        sector = first(Sector),
        industry = first(Industry),
        catname = first(Catname),
        catorder = first(Catorder),
        sector = first(Sector),
        .groups = "drop"
    ) %>%
    mutate(CRPName = toupper(CRPName)) %>%
    mutate(
        last_name = str_extract(CRPName, "^([^,]*)"),
        first_name = str_extract(CRPName, "(?<= )([a-zA-Z]+)")
    ) %>%
    mutate(fullname = paste(last_name, first_name, sep = " ")) %>%
    ungroup()
print("doneloading")

remove_list <- c("unknown", "NA", "other", "non-contribution")

pacdata_temp <-
    pacdata_quarter %>%
    filter(.$fullname %in% preds_withmeta_byquart$fullname) %>%
    filter(.$quarter > 1995) %>%
    filter(!(.$sector %in% remove_list))
pacdata_temp$sector <- pacdata_temp$sector %>%
    tolower() %>%
    {
        gsub(" ", "_", .)
    } %>% # nolint
    {
        gsub("/", "_", .)
    } # nolint
rm(remove_list)

donation_stats <- pacdata_temp %>%
    group_by(fullname, RealCode) %>%
    summarize(
        lifetime_donations = sum(sum_donations),
        largest_donation = max(sum_donations),
        date_largest_donate = quarter[which.max(sum_donations)],
        date_first_donation = min(quarter),
        .groups = "drop"
    ) %>%
    filter(!is.na(.$lifetime_donations))
pacdata_temp <-
    left_join(
        pacdata_temp, donation_stats,
        join_by("fullname", "RealCode"),
        relationship = "many-to-one"
    )
pacdata_temp <- pacdata_temp %>%
    select(!c("first_name", "last_name", "CRPName"))

setwd(datastorage_dir)

rm(list = c(
    "donation_stats_catorder", "pac_data"
))

write_delim(pacdata_temp, file = "pacdata_quarter.txt", delim = "|")


## FITTING NEW MODEL -----------------------------------
## loading
setwd(datastorage_dir)
speech_dfm <- read_rds(file = "speech_bigrams_new.rds")
trimmed_dfm <- speech_dfm %>%
    dfm_trim(min_termfreq = 0.000007, termfreq_type = "prop")
trimmed_dfm <- trimmed_dfm[rowSums(trimmed_dfm) > 0, ]
sample_indexes <- read_csv(file = "sampled_indexes.csv")$sample_indexes
sampled_model <- read_rds(file = "D:/Data/ModifiedFiles/final_model/final_model_150_iteration.rds")

test_indexes <- !(1:nrow(trimmed_dfm) %in% sample_indexes)
test_dfm <- trimmed_dfm[test_indexes, ]
phi_mod <- sampled_model$phi

phi_mod_matrix <- as(phi_mod, "sparseMatrix")
test_matrix <- as(test_dfm, "sparseMatrix")

topic_weights <- test_dfm %*% t(phi_mod)

max_values <- apply(topic_weights, 1, which.max)
max_values <- as_tibble(max_values)
tops <- rownames(sampled_model$phi)
tops <- tibble(tops, ind = 1:length(tops))
new_match <- left_join(max_values, tops, join_by(value == ind), relationship = "many-to-one")
new_match$speech_id <- as.integer(rownames(test_dfm))
test_preds <- left_join(new_match, speaker_maps)


### return here after new model fitting ---------
newmd <- read_rds("newestmod.rds")
subdat <- newmd %>%
    mutate(
        doc_id = testdocs,
        topic_mode = mod_ss
    ) %>%
    select(c("doc_id", "topic_mode"))

subdat <- left_join(
    subdat,
    speaker_maps,
    by = join_by("doc_id" == "speech_id")
)

subdat <- mutate(subdat,
    fullname = paste(lastname, firstname, sep = " "),
    date = ymd(date)
)
rm(list = c("speaker_maps"))

alltops <- unique(subdat$topic_mode) %>%
    gtools::mixedsort(alltops, decreasing = FALSE)
allvar <- list()
for (i in 1:length(alltops)) {
    var <- ifelse(alltops[i] == subdat$topic_mode, 1, 0)
    allvar <- c(allvar, list(var))
}
onehot_tops <- do.call(cbind, allvar)
colnames(onehot_tops) <- alltops
colnames(onehot_tops) <- gsub("[[:punct:][:digit:]]", "", colnames(onehot_tops))
onehot_topics <- as_tibble(onehot_tops)
alltops <- names(onehot_topics)
onehot_topics$doc_id <- as.numeric(subdat$doc_id)

subdat <- left_join(subdat, onehot_topics)

quarterly_metadata <- subdat %>%
    mutate(
        quarter = quarter(date, type = "year.quarter")
    )
topsums <- quarterly_metadata %>%
    group_by(fullname, quarter) %>%
    summarise_at(vars(any_of(alltops)), sum, na.rm = TRUE)
q_data <- quarterly_metadata %>%
    group_by(fullname, quarter) %>%
    summarise(
        party = first(party),
        chamber = first(chamber),
        state = first(state),
        district = first(district),
        gender = first(gender),
        word_count = sum(word_count)
    )
preds_withmeta_byquart <- left_join(q_data, topsums, relationship = "one-to-many")

rm(list = c(
    "quarterly_metadata", "quart_conts_bysector",
    "pacdata_quarter", "subdat", "topsums",
    "q_data", "onehot_topics", "allvar",
    "doc_id"
))

setwd(datastorage_dir)
write_delim(preds_withmeta_byquart, file = "subsampledata_NEW_1.txt", delim = "|")
# write_delim(preds_withmeta_byquart, file = "subsampledata.txt", delim = "|")
# write_delim(preds_withmeta_byquart, file = "metadata_withpreds_matched.txt", delim = "|")

write_delim(as.data.frame(alltops), file = "topiclist_newmod.txt", delim = "|")



### Evaluating topic models
### generating different k-values models  ------------------------------------------------------
small_together_model <- function(save_location,
                                 seed_m,
                                 nloops,
                                 keyword_v = topic_phrases,
                                 n_iteration = 250,
                                 nokey_topics = 2) {
    start_time <- Sys.time()
    print(paste("Model Started at: ", start_time))
    setwd(save_location)
    for (i in 1:nloops) {
        looptime <- Sys.time()
        print(paste("Loop #", i, "Computing!"))

        model <- keyATM( # Runs model with iteration session
            docs = key_bigrams,
            no_keyword_topics = nokey_topics,
            keywords = keyword_v,
            model = "base",
            options = list(
                seed = seed_m,
                iterations = n_iteration,
                verbose = TRUE
            )
        )
        end_time <- Sys.time()
        loop_time <- paste("Loop Time: ", end_time - start_time)
        timelog <- paste(
            "start time: ", start_time, "\n",
            "end time: ", end_time, "\n",
            "total loop time: ", loop_time, "\n",
            "Loop #: ", i, "\n",
            "iterations: ", n_iteration, "\n",
            "nokey_topics: ", nokey_topics, "\n",
            "seed(s): ", seed_m, "\n"
        )
        savefile <- paste0(save_location, "model.rds")
        save.keyATM_output(model, file = savefile)
        files <- list.files(pattern = ".txt")
        le <- length(files)
        write(timelog, file = paste0("time_log_", (le + 1), ".txt"))
    }
    print(paste("Model Finished at: ", end_time))
    print(end_time - start_time)
    return(model)
}

setwd(datastorage_dir)
key_bigrams <- read_rds(file = "keyatm_TRAINING_dfm.rds")
topic_keywords <- read_rds(file = "topic_keywords.rds")
topic_keywords <- lapply(topic_keywords, function(x) gsub("_", " ", x))

setwd("D:/Data/ModifiedFiles/k_searches")

for (i in seq(0, 10, by = 1)) {
    temp_model <- small_together_model(
        nloops = 1,
        keyword_v = topic_keywords,
        seed_m = 150296,
        n_iteration = 75, ## should be ~ 40 mins per loop
        nokey_topics = i,
        save_location = "D:/Data/ModifiedFiles/k_searches"
    )
    model_name <- paste0("model_k_", i, ".rds")
    write_rds(temp_model, file = model_name)
}

### Calculate UMass coherence and exclusivity --------------------------------------------------------------------------------
## https://aclanthology.org/D11-1024.pdf
keyATM_coherence <- function(mod, dfm) {
    top_10 <- top_words(mod, show_keyword = FALSE)
    top_10 <- lapply(top_10, function(x) gsub("_", " ", x))
    coherence_scores <- lapply(top_10, function(words) {
        pair_coherence <- sapply(1:(length(words) - 1), function(i) {
            w1 <- words[i]
            w2 <- words[i + 1]
            D_w1_w2 <- sum(dfm[, w1] & dfm[, w2])
            D_w1 <- sum(dfm[, w1])
            log((D_w1_w2 + 1) / D_w1)
        })
        mean(pair_coherence)
    })

    return(coherence_scores)
}

## https://icml.cc/2012/papers/113.pdf
exclusivity_keyATM <- function(mod, top_n_tokens = 10,
                               excl_weight = 0.5) {
    # Obtain the beta matrix from the topicmodel object
    beta_mat <- exp(mod$phi)
    # Normalize the beta values within each topic
    # SO link for reference - https://stats.stackexchange.com/a/51750
    beta_normed <- beta_mat %*% diag(1 / colSums(beta_mat))
    # Calculate exclusivity (using approx ECDF)
    excls <- apply(beta_normed, 1, rank) / ncol(beta_normed)
    # Calculate frequency (using approx ECDF)
    freqs <- apply(beta_mat, 1, rank) / ncol(beta_mat)

    # Obtain the indicies of the top terms in the model
    # and select only those from the exclusivity and frequency matrices
    top_10 <- top_words(mod, n = top_n_tokens, show_keyword = FALSE)
    top_10 <- lapply(top_10, function(x) gsub("_", " ", x))

    mod$vocab <- lapply(mod$vocab, function(x) gsub("_", " ", x))

    top_terms <- lapply(top_10, function(x) {
        indi <- mod$vocab %in% x

        return(indi)
    })

    # Create an empty vector for the frex scores
    frex <- vector(length = ncol(freqs))

    # Loop through each topic's terms and calculate its total frex score
    i <- 1
    for (i in 1:ncol(freqs)) {
        # Identifying and selecting rows for this topic's terms
        term_inds <- top_terms[[i]]
        this_excls <- excls[term_inds, i]
        this_freqs <- freqs[term_inds, i]

        # Calculate frex score using the provided exclusivity weight
        excl_term <- excl_weight / this_excls
        freq_term <- (1 - excl_weight) / this_freqs
        frex[i] <- sum(1 / (excl_term + freq_term))
    }

    return(frex)
}

## list models
test_dir <- "D:/Data/ModifiedFiles/final_model/"
model_names <- list.files(path = test_dir, pattern = "iteration.rds")
model_names <- gtools::mixedsort(model_names)
# all_bigrams <- read_rds("all_bigrams.rds")

sample_indexes <- read_csv("sampled_indexes.csv")$sample_indexes
test_indexes <- !(1:nrow(all_bigrams) %in% sample_indexes)

training_bigrams <- read_rds(file = "training_bigrams_dfm.rds")
test_bigrams <- read_rds(file = "test_bigrams.rds")
# test_bigrams <- all_bigrams[test_indexes, ]
# write_rds(test_bigrams, file = "test_bigrams.rds")


## find exclusivity and coherence for each model
test <- lapply(model_names, function(model) {
    mod <- read_rds(file = paste0(test_dir, model))
    coherence <- keyATM_coherence(mod, training_bigrams)
    exclusivity <- exclusivity_keyATM(mod)

    return(list(coherence = coherence, exclusivity = exclusivity))
})

mod <- read_rds(paste0(test_dir, model_names[1]))

## using test_bigrams instead
test2 <- lapply(model_names, function(model) {
    mod <- read_rds(file = paste0(test_dir, model))
    coherence <- keyATM_coherence(mod, test_bigrams)
    return(list(coherence_test = coherence))
})

## seperate values into objects
coherence_values <- lapply(test, function(x) x$coherence)
coherence_testset <- lapply(test2, function(x) x$coherence_test)
exclusivity_values <- lapply(test, function(x) x$exclusivity)

## convert to dataframes
coherence_values_df <- plyr::ldply(coherence_values, data.frame) %>%
    t()
coherence_test_df <- plyr::ldply(coherence_testset, data.frame) %>%
    t()
exclusivity_values_df <- data.frame(lapply(exclusivity_values, `length<-`, max(lengths(exclusivity_values))))

## find the means of each, accross topics and make into single dataframe
coherence_mean <- apply(coherence_values_df, 2, mean, na.rm = TRUE)
coherence_mean_test <- apply(coherence_test_df, 2, mean, na.rm = TRUE)
exclusivity_mean <- apply(exclusivity_values_df, 2, mean, na.rm = TRUE)
iterations <- (1:length(test) * 50)
metrics <- tibble(
    iterations = iterations, coherence_mean = coherence_mean,
    coherence_mean_test = coherence_mean_test, exclusivity_mean = exclusivity_mean
)
names(metrics)

metrics <- read_rds(file = "model_rankings.rds")

## find rankings
coherence_rank <- rank(-metrics$coherence_mean, ties.method = "first")              ## WANTS HIGHER VALUES
coherence_rank_test <- rank(-metrics$coherence_mean_test, ties.method = "first")    ## WANTS HIGHER VALUES
exclusivity_rank <- rank(metrics$exclusivity_mean, ties.method = "first")           ##  WANTS LOWER VALUES

metrics$coherence_rank <- coherence_rank
metrics$exclusivity_rank <- exclusivity_rank
metrics$coherence_rank_test <- coherence_rank_test

metrics$avg_rank <- (metrics$coherence_rank + metrics$exclusivity_rank + metrics$coherence_rank_test) / 3

write_rds(metrics, file = paste0(datastorage_dir, "model_rankings.rds"))

### BEST NUMBER OF NOKEY TOPS = 4
## BEST ON EXCLUSIVITY, AND SECOND BEST ON COHERENCE, NO OTHER VALUE CLOSE
setwd("D:/Data/ModifiedFiles/final_model")
library(keyATM)
library(quanteda)

key_bigrams <- read_rds(file = "keyatm_TRAINING_dfm.rds")
topic_keywords <- read_rds(file = "topic_keywords.rds")



start_time <- Sys.time()
for (i in 2:66) {
    model <- keyATM( # Runs model with iteration session
        docs = key_bigrams,
        no_keyword_topics = 4,
        keywords = topic_keywords,
        model = "base",
        options = list(
            seed = 150296,
            iterations = 50,
            verbose = TRUE,
            resume = "keyATM_resume.rds"
        )
    )
    save.keyATM_output(model, file = paste0("final_model_", i * 50, "_iteration.rds"))
}
end_time <- Sys.time()

start_time - end_time

## best models: (ON TRAINING SET)
## 50 iterations 1, 1
## 150 iterations 2, 4
## 300 iterations 5, 7

mod50 <- read_rds(file = paste0(test_dir, model_names[1]))
mod150 <- read_rds(file = paste0(test_dir, model_names[3]))
mod300 <- read_rds(file = paste0(test_dir, model_names[6]))


top10_50 <- top_words(mod50, n = 10)
top10_150 <- top_words(mod150, n = 10)
top10_300 <- top_words(mod300, n = 10)

view(top10_50)
view(top10_150)
view(top10_300)
