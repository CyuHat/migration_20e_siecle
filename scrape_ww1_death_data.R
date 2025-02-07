library(rvest)
library(janitor)

link <- "https://fr-academic.com/dic.nsf/frwiki/1316714"

page <- read_html(link)

raw_table <-
  page %>% 
  html_elements("table") %>% 
  pluck(3) %>% 
  html_table() %>% 
  clean_names()


morts_pmg <-
  raw_table %>% 
  mutate(
    puissances_alliees = str_remove_all(puissances_alliees, "\\[\\d{1,3}\\]"),
    population_en_millions = str_replace_all(population_en_millions, "\\,", "."),
    pertes_militaires = str_remove_all(pertes_militaires, "\\s"),
    pertes_civiles = str_remove_all(pertes_civiles, "\\s"),
    total = str_remove_all(total, "\\s"),
    blesses_militaires = str_remove_all(blesses_militaires, "\\s"),
    ) %>% 
  slice(-20, -26) %>%
  mutate(
    groupe = c(
      rep("Puissance alliées", 19),
      rep("Empires centraux", 5),
      rep("Pays neutres", 4)
    ),
    groupe2 = if_else(str_detect(puissances_alliees, "Total"), "Total", "Valeur"),
    across(
      population_en_millions:blesses_militaires, as.numeric
    ),
    population_en_millions = population_en_millions*1000000
  )

write_csv(morts_pmg, file = "data/morts_pmg.csv")
