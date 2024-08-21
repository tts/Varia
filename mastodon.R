library(tidyverse)
library(rtoot)

# From a number of most active Mastodon instances, search #20books20days,
# combine, filter unique ids, parse media and its alt tag. 
# 
# Try to clean and filter the name of the book.
# Which are the top titles?

#-----------------------------------------
# Auth
#
# auth_setup(path = "[working dir]", 
#            instance = "mastodontti.fi", 
#            type = "user")
#-----------------------------------------
fedi_token <- Sys.getenv("FEDI_INST_KEY") 
rtoot_token <- readRDS("rtoot_token.rds")
token <- rtoot_token$bearer

#-------------------------------------------
# All instances 
#
# 16453 as of writing this
# 
# Check: why does fedi_token return "" ?
#-------------------------------------------
fed <- get_fedi_instances(n = 0, 
                          token = "[your token]")

fed_active <- fed %>% 
  arrange(desc(active_users)) %>% 
  select(name, users, connections, active_users) %>% 
  top_n(200)

saveRDS(fed_active, "fed_active.RDS")

#-----------
# Func
#-----------
get_toots <- function(inst){
  booktoots <- get_timeline_hashtag(hashtag = "20books20days", 
                                   instance = inst,
                                   limit = 1000) 
  return(booktoots)
}

#------------- 
# Get toots
#-------------

# Instance names as a character vector 
insts <- as.vector(fed_active$name)

# First just from the biggest instance as a data frame
fed_active_booktoots <- get_toots(insts[1]) 
saveRDS(fed_active_booktoots, "mastodon_social_booktoots.RDS")
fed_active_booktoots <- readRDS("mastodon_social_booktoots.RDS")

# And then the next 19 until 20 as a list
insts_therest <- insts[2:20]
toots_list <- map(.x = insts_therest, 
                  .f = purrr::possibly(get_toots, otherwise = NA),
                  .progress = TRUE)

toots_list_no_na <- toots_list[!sapply(toots_list, function(x) all(is.na(x)))]

saveRDS(toots_list_no_na, "toots_list_fedi_2to20.RDS")
toots_list <- readRDS("toots_list_fedi_2to20.RDS")

#-------------------------
# Pick media elements
#-------------------------

out <- map_df(.x = toots_list,
             .f = ~ {
               tibble(id = .x$id,
                      media_attachments = .x$media_attachments)
             })

out2 <- fed_active_booktoots %>% 
  select(id, media_attachments)

toots_df <- rbind(out, out2)
toots_df <- toots_df %>% 
  distinct(id, .keep_all = TRUE) # no duplicates (?)

media <- toots_df[, "media_attachments"]
media2 <- media %>% 
  filter(lengths(media_attachments) > 0)
allmedia <- bind_rows(media2$media_attachments)
alts <- distinct(allmedia, id, .keep_all = TRUE) %>% 
  filter(!is.na(description)) %>% 
  select(description)
saveRDS(alts, "toots_alts.RDS") # 19141

# Am I right that the toot ID is not unique
# across federated toots? When the alts are sorted,
# there are exactly similar ones, which implies that
# they are indeed from the same original toot.

alts <- alts %>% 
  distinct(description) %>% # 5165
  filter(description != "")
saveRDS(alts, "toots_alts_unique.RDS")

#-----------------------
# Tokenize to words
# and try to parse names
#
# TODO
#------------------------






