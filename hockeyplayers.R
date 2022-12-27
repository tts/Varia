library(tidyverse)
library(rvest)

# Trying to answer to touqo's question
# https://twitter.com/touqo/status/1607440912061444097

url <- "https://fi.wikipedia.org/wiki/Suomen_j%C3%A4%C3%A4kiekkomaajoukkue"

links <- read_html(url) %>% 
  html_nodes(xpath = "//table[@style='background-color: transparent;width:100%'][1]//li/a") %>% 
  html_attr("href")

#----------
# Functions
#----------

harvest_table <- function(vec, xpath) {
  res <- map_df(vec, function(link) {
    url <- paste0("https://fi.wikipedia.org", link)
    page <- read_html(url)
    data.frame(url = url,
               players = page %>% 
                 html_node(xpath = xpath) %>% 
                 html_table(),
               stringsAsFactors = FALSE)
  })
  return(res)
}

harvest_text <- function(vec, xpath) {
  res <- map_df(vec, function(link) {
    url <- paste0("https://fi.wikipedia.org", link)
    page <- read_html(url)
    data.frame(url = url,
               players = page %>% 
                 html_nodes(xpath = xpath) %>% 
                 html_text(),
               stringsAsFactors = FALSE)
  })
  return(res)
}

#----------------------------------------------------------
# Links with an URI fragment (#) contains data of all teams. 
# Without it, just Finland. Few exceptions, though.
#----------------------------------------------------------

links_all <- grep("#", links, value = TRUE)
links_fi <-  setdiff(links, links_all)

# The following are treated separately below
links_fi <- links_fi[!links_fi %in% c('/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2006',
                                      '/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2007',
                                      '/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2009',
                                      '/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2010',
                                      '/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2011')]

#----------------
# Just FI
#
# Typical cases
#----------------

df_fi <- harvest_table(links_fi, "//table[1]")
df_fi <- df_fi %>% 
  select(url, players.Pelaaja) %>% 
  rename(game = url,
         player = players.Pelaaja)

#------------------------------
# Just FI
#
# 3 types of exceptions (d1-d3)
#------------------------------

links_fi_d1 <- c("/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2006",
                 "/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2007")

df_fi_d1 <- harvest_text(links_fi_d1, 
                         "//span[@id = 'Maalivahdit' or @id = 'Puolustajat' or starts-with(@id, 'Hy')]/../following-sibling::ul[1]//li/a[1]")
names(df_fi_d1) <- c("game", "player")

links_fi_d2 <- c("/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2009",
                 "/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2010")

df_fi_d2 <- harvest_table(links_fi_d2, "//span[@id = 'Suomi']/../following-sibling::table[1]")

df_fi_d2 <- df_fi_d2 %>% 
  select(url, players.Pelaaja) %>% 
  rename(game = url,
         player = players.Pelaaja)

# The third exception was a bit tricky. Therefore, code separated here by player role
links_fi_d3 <- "/wiki/Suomen_joukkue_j%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailuissa_2011"

df_fi_d3_m <- harvest_table(links_fi_d3, "//span[@id = 'Maalivahdit']/../following-sibling::table")
df_fi_d3_p <- harvest_table(links_fi_d3, "//span[@id = 'Puolustajat']/../following-sibling::table")
df_fi_d3_h <- harvest_table(links_fi_d3, "//span[starts-with(@id, 'Hy')]/../following-sibling::table")

df_fi_d3_m <- df_fi_d3_m %>% 
  select(url, ends_with("Nimi")) 
names(df_fi_d3_m) <- c("game", "player")

df_fi_d3_p <- df_fi_d3_p %>% 
  select(url, ends_with("Nimi")) 
names(df_fi_d3_p) <- c("game", "player")

df_fi_d3_h <- df_fi_d3_h %>% 
  select(url, ends_with("Nimi")) 
names(df_fi_d3_h) <- c("game", "player")

df_fi_d3 <- rbind(df_fi_d3_m, df_fi_d3_h, df_fi_d3_p)

df_fi_all <- rbind(df_fi, df_fi_d1, df_fi_d2, df_fi_d3) 

df_fi_all_data <- df_fi_all %>% 
  mutate(player2 = stringi::stri_extract(player, regex = "[a-zåäö][A-ZÅÄÖ].*"),
         player3 = stringi::stri_replace_first(player2, "", regex = "[a-zåäö]"),
         player = ifelse(is.na(player3), player, player3)) %>% 
  select(game, player)
  
#----------------
# All teams
#
# Typical cases
#----------------

links_all_most <- links_all[c(1:9, 11:18, 20:23)]
links_all_most <- links_all_most[!links_all_most %in% c("/wiki/J%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailujen_2012_kokoonpanot#Suomi")]

df_all_most <- harvest_table(links_all_most, "//span[@id = 'Suomi']/../following-sibling::table[1]")

df_all_most <- df_all_most %>% 
  select(url, players.Pelaaja) %>% 
  rename(game = url,
         player = players.Pelaaja)


#-----------------------
# All teams
#
# 2 types of exceptions
#-----------------------

links_all_diff <- links_all[c(10,19)]

df_all_diff <- harvest_table(links_all_diff, "//span[@id = 'Joukkue_MM-kisoissa_2008' or @id = 'Miesten_turnaus']/../following-sibling::table[1]")

df_all_diff <- df_all_diff %>% 
  select(url, players.Pelaaja) %>% 
  rename(game = url,
         player = players.Pelaaja)

links_all_diff2 <- "/wiki/J%C3%A4%C3%A4kiekon_maailmanmestaruuskilpailujen_2012_kokoonpanot#Suomi"

df_all_diff2 <- harvest_text(links_all_diff2, "//span[@id = 'Suomi']/../following-sibling::table[1]//a[1]")

df_all_diff2 <- df_all_diff2 %>% 
  select(url, players) %>% 
  rename(game = url,
         player = players)

#-------------
# Combine all
#-------------

players <- rbind(df_fi_all_data, df_all_most, df_all_diff, df_all_diff2)

#---------- 
# Cleaning
#----------

names <- players %>% 
  filter(!grepl("Finland|Suomen", player)) %>% 
  mutate(player = str_trim(sub("–\\s[A-Z]|\\([A-Z]\\))", "", player)),
         playername = ifelse(grepl("\\,", player), 
                              str_trim(stringi::stri_extract(player, regex = "\\s.*")),
                              player),
         surname = str_trim(stringi::stri_extract(playername, regex = "\\s.*")),
         surname = sub("\\s[A-Z]$|\\([A-Z]\\)$|\\[[0-9]+\\]", "", surname))

#-----------
# Statistics
#-----------

names_stat <- names %>% 
  group_by(surname) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count), .by_group = TRUE)

write.csv(names_stat, "fi_ishockey_players_surnames.csv", row.names = FALSE)  
  