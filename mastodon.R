library(tidyverse)
library(rtoot)
library(spacyr)
library(textcat)

# Search toots with the hashtag #20books20days, filter unique ones, 
# and parse media and its alt tag. Try to find out the most
# popular authors. 
# 
# Here, using only the mastodon.social instance. First, the newest 100 toots,
# and then incrementally with 'since_id' taken from the last toot
# returned by the previous call.
#
# Another strategy would be to choose a couple of the most active instances
# (by the number of users or by the number of connections) and search 
# from them.

#---------------------------------------------------------------
# auth_setup(path = "[working dir]", 
#            instance = "mastodontti.fi", 
#            type = "user") #perhaps `public` would've been enough?
#---------------------------------------------------------------
rtoot_token <- readRDS("rtoot_token.rds")
token <- rtoot_token$bearer

#-----------
# Functions
#-----------

get_id <- function(df){
  id <- df %>% 
    slice_min(id, n = 1) %>% 
    select(id) %>% 
    as.character()
  return(id)
}

get_toots <- function(inst, id = NULL){
  toots <- get_timeline_hashtag(hashtag = "20books20days", 
                                only_media = TRUE,
                                instance = inst,
                                limit = 100,
                                since_id = id, 
                                token = token) 
  return(toots)
}

fetch_and_extract_id <- function(inst, last_id = NULL) {
  toots <- get_toots(inst, id = last_id)
  list(
    toots = toots,
    last_id = get_id(toots)
  )
}

if_null_na <- function(x){
  if(is.null(x)) return(NA)
  x
}

#------------- 
# Get toots
#-------------

# Number of iterations
n <- 100

# Initialize the process
initial_result <- fetch_and_extract_id("mastodon.social")
res_df <- initial_result$toots
last_id <- initial_result$last_id

# Perform iterations
results <- accumulate(
  seq_len(n - 1),
  ~ {
    fetch_result <- fetch_and_extract_id("mastodon.social", last_id)
    last_id <<- fetch_result$last_id
    bind_rows(.x, fetch_result$toots)
  },
  .init = res_df
)

# Combine all results into a single data frame
res_df <- results[[length(results)]]

# That was terminated when there were 6574 toots
saveRDS(res_df, "booktoots.RDS")

# The next day I tried first to fetch potential remaining toots with the last since_id param
# from the earlier fetch, but got a persistent 503 error code. 
# Instead, left that param out but increased the limit to 10000.
# That yielded only 6594, probably because of timeout.

#-------------------------
# Pick media elements
#-------------------------

out <- res_df %>% 
  select(id, uri, media_attachments) %>% 
  distinct(uri, .keep_all = TRUE) 

media <- out[, "media_attachments"]
media2 <- media %>% 
  filter(lengths(media_attachments) > 0)
allmedia <- bind_rows(media2$media_attachments)

alts <- distinct(allmedia, remote_url, .keep_all = TRUE) %>% 
  filter(!is.na(description)) %>% 
  select(description)
saveRDS(alts, "booktoots_alts.RDS") 

alts <- alts %>% 
  distinct(description) %>% 
  filter(description != "") # 5323 distinct ones
saveRDS(alts, "booktoots_alts_unique.RDS")

alts <- readRDS("booktoots_alts_unique.RDS")

rm(res_df, out, allmedia, media, media2)
gc()

#--------------------------------------------
# Language detection. 
# Trying the three most visible ones (to me)
#--------------------------------------------
langs <- c("english", "german", "finnish")
my.profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% langs]

# But - is this really necessary? After all, I want to parse ALL person
# entities regardless of the lang of the alt text. In other words, even though
# the alt text is, say, in Finnish, there do exists e.g. English author names.
# As long as Finnish (and other) person names are captured, I'm fine.
# That said, German might be a problem because it capitalizes all nouns,
# so parsing them separately.

alts <- alts %>% 
  mutate(lang = textcat(description, p = my.profiles))

#----------------------------------------------------------
# Person entity detection with all other langs but german.
# This is by no means faultless but will do here. 
#----------------------------------------------------------
spacy_initialize()

p_ent <- alts %>% 
  filter(lang != "german") %>% 
  pull(description) %>% 
  spacy_extract_entity() %>% 
  filter(ent_type == "PERSON")

#----------------
# and then german
#-----------------
spacy_finalize()
spacy_initialize(model = "de_core_news_lg")

p_ent_ge <- alts %>% 
  filter(lang == "german") %>% 
  pull(description) %>% 
  spacy_extract_entity() %>% 
  filter(ent_type == "PER")

#----------------------------------------
# Trying to find similar names with 'agrep'
# which uses the Levenshtein algorithm.
#----------------------------------------
person <- bind_rows(p_ent, p_ent_ge)

t <- as.vector(person$text)

groups <- list()
i <- 1
while(length(t) > 0)
{
  id <- agrep(t[1], t, ignore.case = TRUE, max.distance = 0.2)
  groups[[i]] <- t[id]
  t <- t[-id]
  i <- i + 1
}

# For an unknown reason, some vectors are exceptionally long, containing multiple
# seemingly unrelated names. Getting rid of them for now.
# Note also that in 'transpose()' below, the 1st element is used as a template 
# so it better be the longest one
groups_s <- groups[lengths(groups) <= 50]
len <- sapply(groups_s, length)
groups_s_sorted <- groups_s[order(len, decreasing = TRUE)]

# From a list of character vectors to a data frame
df <- groups_s_sorted %>% 
  transpose() %>% 
  map(~map(.x, if_null_na)) %>% 
  map(unlist) %>% 
  as_tibble(.name_repair = "unique")

saveRDS(df, "booktoot_similar_names.RDS")

# Looking at the first ~100 rows shows that
# the most popular names get >30 mentions
# and then the popularity decreases rapidly.
# Filtering down to those rows where there are
# at least 5 mentions.
df_top <- df %>% 
  rename(c5 = `...5`) %>% 
  filter(complete.cases(c5))

#-----------------------------------
# Manually checking all these
# by sorting by the first column
# and then just by looking which
# indeed refer to the same name.
#-----------------------------------
#
# Angela Carter
# Barbara Kingsolver
# Brian W. Kernighan
# Carl Sagan
# Douglas Adams
# Edgar Rice Burroughs
# Franz Kafka
# Fyodor Dostoyevsky
# Gabriel Garcia Marquez
# George Orwell
# Frank Herbert
# Isaac Asimov
# Jean-Paul Sartre
# John Steinbeck
# Lewis Carroll
# Michael Crichton
# Neal Stephenson
# Neil Gaiman
# Oliver Sacks
# Orson Scott Card
# Robert Graves
# Stephen Jay Gould
# Stephen King
# Suzanne Collins
# Tove Jansson

