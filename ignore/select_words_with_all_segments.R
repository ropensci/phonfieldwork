library(tidyverse)
df <- read_csv("~/Desktop/andic_dicts - Arkusz1.csv")
df %>%
  filter(reference == "Magomedova, Khalidov 2001") %>%
  select(ipa) %>%
  distinct() ->
  karata

karata %>%
  write_csv("~/Desktop/karata.csv")

library(tidytext)
read_csv("~/Desktop/karata.csv") %>%
  mutate(id = 1:n(),
         n = str_count(ipa, "-")+1) ->
  karata

karata %>%
  unnest_tokens(output = "segments", input = ipa, token = stringr::str_split, pattern = "-", drop = FALSE) %>%
  arrange(segments, n) ->
  karata_long

final <- tibble(sl = integer(), n = integer(), j  = integer())
j = 0
karata_long %>%
  count(segments) %>%
  arrange(n)  ->
  seg

results <- integer()

while(nrow(seg) > 0) {
  karata_long %>%
    filter(segments == seg$segments[1]) %>%
    slice(1) %>%
    pull(id) %>%
    append(results, values = .) ->>
    results

  tibble(id = results) %>%
    left_join(karata_long, by = "id") %>%
    anti_join(seg, ., by = "segments") ->>
    seg
}

j = j+1

karata %>%
  filter(id %in% results) %>%
  summarise(sl = sum(n), n = n(), j = j) %>%
  bind_rows(final) ->
  final


final %>%
  mutate(type = ifelse(sl < 300, "sorted", "random sample")) %>%
  ggplot(aes(sl, n, color = type))+
  geom_point()+
  theme_minimal()+
  labs(x = "overall length sum", y = "number of words")+
  theme(text = element_text(size = 13))
