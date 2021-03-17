library(osmdata)
bb <- getbb('Paris, France',format_out = 'polygon')

mail <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = 'post_box') %>%
  osmdata_sf () %>%
  trim_osmdata (bb)

mail$osm_points

mail_tbl <- as_tibble(st_coordinates(mail$osm_points$geometry)) %>% 
  rename(x=1,y=2)


post <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = 'post_office') %>%
  osmdata_sf () %>%
  trim_osmdata (bb)

post_tbl <- as_tibble(st_coordinates(post$osm_points$geometry)) %>% 
  dplyr::filter(!duplicated(post$osm_points$name)) %>% 
  rename(x=1,y=2)

write_csv(mail_tbl,"data/mail_boxes.csv")
write_csv(post_tbl,"data/post_offices.csv")
