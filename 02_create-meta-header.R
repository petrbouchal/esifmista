library(metathis)

meta() %>%
  meta_robots(robots = "noindex,nofollow") %>% write_meta(path = "meta.html")
