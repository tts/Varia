library(tidyverse)
library(rvest)

urls <- c("https://fi.wikipedia.org/wiki/Luokka:Vuorineuvokset", 
         "https://fi.wikipedia.org/w/index.php?title=Luokka:Vuorineuvokset&pagefrom=Petersen%2C+Wolter%0AWolter+Petersen#mw-pages")

raw_links <- map(urls, ~read_html(.x) %>% 
                   html_nodes(xpath = "//div[@class='mw-category-group']//a") %>%
                   html_attr("href") %>% 
                   paste0("https://fi.wikipedia.org", .))

links <- unlist(raw_links)
saveRDS(links, "links.RDS")

try_references <- function(vec, xpath) {
  res <- map_df(vec, function(link) {
    page <- read_html(link) 
    nodes <- page %>% 
      html_nodes(xpath = xpath)
    data.frame(url = link,
               text = nodes %>% 
                 html_text(),
               link = nodes %>%
                 html_attr("href"),
               stringsAsFactors = FALSE)
    })
    return(res)
}

get_references <- safely(try_references, otherwise = tibble(url = NA_character_, text = NA_character_, link = NA_character_))

xpath <- "//span[@class='mw-headline']/../following-sibling::div[1]//span[@class='reference-text']//a|//span[@class='mw-headline']/../following-sibling::ul[1]//li//a"

res <- map(links, ~get_references(.x, xpath)) %>% 
  map("result") 

saveRDS(res, "vuorineuvos_raw.RDS")

res_df <- as.data.frame(do.call(rbind, res))

# Join the links list with this result to ferret out items with zero web references
links_df <- as.data.frame(do.call(rbind, as.list(links)))
res_vs_links_df <- right_join(res_df, links_df, by = c("url" = "V1"))

withouth_links <- res_vs_links_df %>% 
  filter(is.na(text))

with_links <- res_vs_links_df %>% 
  filter(!is.na(text))

with_no_bio_links <- with_links %>% 
  filter(!grepl("biografia", link))

with_bio_links <- with_links %>% 
  filter(grepl("biografia", link)) %>% 
  distinct(url, .keep_all = TRUE)

write.csv(with_bio_links, "vuorineuvos.csv", row.names = FALSE)
saveRDS(with_bio_links, "vuorineuvos_bio.RDS")

persons <- with_bio_links %>% 
  rowwise() %>% 
  mutate(person = gsub("_", " ", strsplit(url, "/")[[1]][5]),
         person = gsub("\\([^\\)]+\\)", "", person),
         person = gsub("%C3%B6", "ö", person),
         person = gsub("%C3%A9", "é", person),
         person = gsub("%C3%85", "Å", person),
         person = gsub("%C3%A5", "å", person),
         person = gsub("%C3%A4", "ä", person),
         person = str_trim(person))

# Done the first 8 manually already
persons <- persons[9:nrow(persons),]

library(WikidataR)
library(WikidataQueryServiceR)

find_id <- as.vector(persons$person)

find_id_res <- map(find_id, ~{
  WikidataR::qid_from_name(.x)
})

saveRDS(find_id_res, "find_id_res.RDS")

names <- map(find_id_res, ~{
  names(.x)
})
ids <- map(find_id_res, ~{
  .x
})

# Replace duplicate items with the correct one (chosen manually)
df <- do.call(rbind, mapply(cbind, names, ids))    
df <- as.data.frame(df, stringsAsFactors = FALSE)
names(df) <- c("name", "id")
df <- df %>% 
  mutate(name = gsub("[0-9]", "", name))

p_q <- data.frame(name = character(0), id = character(0), stringsAsFactors=FALSE)

p_q <- p_q %>% 
add_row(name="Antti Antero",id="Q11852049") %>%
add_row(name="Casimir Ehrnrooth",id="Q5048800") %>%
add_row(name="Georg Ehrnrooth",id="Q5401438") %>%
add_row(name="Erik Gillberg",id="Q15635121") %>%
add_row(name="Kim Gran",id="Q16298342") %>%
add_row(name="Lauri Haarla",id="Q11875021") %>%
add_row(name="Juhani Heinonen",id="Q16298768") %>%
add_row(name="Matti Honkala",id="Q11881565") %>%
add_row(name="Olav Jensen",id="Q11885255") %>%
add_row(name="Waldemar Jensen",id="Q17381167") %>%
add_row(name="Fritz Arthur Jusélius",id="Q5403619") %>%
add_row(name="Jorma Järvi",id="Q18688480") %>%
add_row(name="Matti Kankaanpää",id="Q16299638") %>%
add_row(name="Aarne Karjalainen",id="Q28356837") %>%
add_row(name="Jussi Ketola",id="Q17381608") %>%
add_row(name="Eero Kinnunen",id="Q110322398") %>%
add_row(name="Lauri Kivekäs",id="Q6501401") %>%
add_row(name="Antti Koivuniemi",id="Q11852123") %>%
add_row(name="Anders Kramer",id="Q5400759") %>%
add_row(name="Magnus Linder",id="Q108229520") %>%
add_row(name="Heikki Mustakallio",id="Q20487762") %>%
add_row(name="Eero Mäkinen",id="Q11856763") %>%
add_row(name="Juha Niemelä",id="Q11867393") %>%
add_row(name="Erkki Partanen",id="Q11858388") %>%
add_row(name="Aarne Pelkonen",id="Q11167329") %>%
add_row(name="Matti Puttonen",id="Q109774882") %>%
add_row(name="Juha Rantanen",id="Q11867403") %>%
add_row(name="Risto Rinne",id="Q11890918") %>%
add_row(name="Urho Ruola",id="Q23040809") %>%
add_row(name="Magnus Rydman",id="Q5493911") %>%
add_row(name="Veikko Räsänen",id="Q22696756") %>%
add_row(name="Pentti Salmi",id="Q11887394") %>%
add_row(name="Reino Salo",id="Q22133172") %>%
add_row(name="Wilhelm Schauman",id="Q8002287") %>%
add_row(name="Gösta Serlachius",id="Q6173014") %>%
add_row(name="Arno Solin",id="Q21558169") %>%
add_row(name="Erkki Toivanen",id="Q23040866") %>%
add_row(name="Yrjö Vesa",id="Q11903067") %>%
add_row(name="Matti Viljanen",id="Q11881740") %>%
add_row(name="Pertti Voutilainen",id="Q16991316") %>%
add_row(name="Claes Wahlberg",id="Q108229489") %>%
add_row(name="Wilhelm Wahlforss",id="Q4103113") %>%
add_row(name="Marcus Wallenberg",id="Q6230601")

df_nodup <- left_join(df, p_q, "id")
df_id <- df_nodup %>% 
  filter(!is.na(name.y))

# Georg Ehrnrooth namesakes: Q5401438 and Q11860374
df_id[3,]$id <- "Q11860374"

df_id <- df_id %>% 
  select(name.x, id) %>% 
  rename(name = name.x)

# Exclude all these names from the df
df_d <- right_join(df_id, df, "name")
df_therest <- df_d %>% 
  filter(is.na(id.x)) %>% 
  select(name, id.y) %>% 
  rename(id = id.y)

# Merge df_therest (=no duplicates initially) and df_id (=duplicates cleaned)
df_all <- rbind(df_id, df_therest)
df_all <- df_all %>% 
  mutate(name = str_trim(name),
         wd = paste0("wd:", id)) %>% 
  filter(name != "Amos Anderson") %>% 
  filter(name != "Harald Jensen")

# Few of these already have a claim about vuorineuvos, which ones?
df_wd <- paste(df_all[, "wd"], collapse = " ")

q <- paste0("SELECT ?person ?personLabel WHERE {
  VALUES ?person {", df_wd, "}
  { ?person wdt:P511 wd:Q2089657 } UNION
  { ?person wdt:P166 wd:Q2089657 } UNION
  { ?person wdt:P97 wd:Q2089657 }
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'fi,en'. }
}")

q_res <- query_wikidata(q)

# Exclude these from df_all
q_res_therest <- left_join(df_all, q_res, by = c("name"="personLabel")) %>% 
  filter(is.na(person)) %>% 
  select(-person)

# Now we have the data ready for import, i.e. for these we add a claim about vuorineuvos
# First, add the reference link
q_res_therest_bio <- left_join(q_res_therest, persons, by = c("name"="person")) %>% 
  select(id, name, link)

# G Ehrnrooth older Q5401438 http://biografiasampo.fi/henkilo/p10354
# G Ehrnrooth younger Q11860374 https://kansallisbiografia.fi/talousvaikuttajat/henkilo/1927
q_res_therest_bio <- q_res_therest_bio %>% 
  mutate(link = ifelse(id == "Q11860374", "https://kansallisbiografia.fi/talousvaikuttajat/henkilo/1927", 
                       ifelse(id == "Q5401438", "http://biografiasampo.fi/henkilo/p10354", link))) %>% 
  distinct(id, .keep_all = TRUE)

#-----------------
# Import
#-----------------

# Actually, did this in several batches but here's the idea
q_res_therest_bio <- q_res_therest_bio %>% 
  mutate(prop = "P511",
         val = "Q2089657",
         sprop1 = "S854",
         sval1 = link,
         sprop2 = "S813",
         sval2 = "+2023-01-25T00:00:00Z/11",
         link = str_trim(link)) %>% 
  select(id, name, prop, val, sprop1, sval1, sprop2, sval2)

write_wikidata(items = q_res_therest_bio$id,
               properties = q_res_therest_bio$prop,
               values = q_res_therest_bio$val,
               src.properties = q_res_therest_bio$sprop1, q_res_therest_bio$sprop2,
               src.values = q_res_therest_bio$sval1, q_res_therest_bio$sval2,
               format = "api",
               api.username = "Ttso",
               api.token = "(your token here)",
               api.format = "v1",
               api.batchname = "Vuorineuvos")

#---------------
# Check
#---------------

q <- "SELECT ?person ?personLabel ?genderLabel WHERE {
  ?person wdt:P31 wd:Q5 ;
  wdt:P27 wd:Q33 .
  OPTIONAL { ?person wdt:P21 ?gender . }
  { ?person wdt:P511 wd:Q2089657 } UNION
  { ?person wdt:P166 wd:Q2089657 } UNION
  { ?person wdt:P97 wd:Q2089657 }
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'fi,en'. }
}"

q_res <- query_wikidata(q)

