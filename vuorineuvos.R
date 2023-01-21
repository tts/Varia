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

# https://stackoverflow.com/a/61658681 didn't work here
#res <- 
#  pmap(list(links, seq_along(links), length(links)), ~ {
#  r <- get_references(..1, xpath)
#  cat(..2, "/", ..3, "done\n") 
#  return(r)
#})

res <- map(links, ~get_references(.x, xpath)) %>% 
  map("result") 

saveRDS(res, "vuorineuvos_raw.RDS")
res <- readRDS("vuorineuvos_raw.RDS")

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
