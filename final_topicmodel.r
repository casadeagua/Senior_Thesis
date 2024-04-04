### This file is intended to be a compact script that can be run through an R terminal for extended periods of time.
### Saves the models at various interations, in order for evaluation later.

library(tidyverse)
library(keyATM)
library(quanteda)

setwd("D:/Data/ModifiedFiles/")
key_bigrams <- read_rds(file = "keyatm_TRAINING_dfm.rds")
topic_keywords <- read_rds(file = "topic_keywords.rds")

setwd("D:/Data/ModifiedFiles/final_model")
start_time <- Sys.time()
for (i in 1:66) {
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

value <- start_time - end_time

log <- paste(
    "Start time: ", start_time, "\n",
    "End time: ", end_time, "\n",
    value, "\n"
)

write_file(log, file = "log.txt")
