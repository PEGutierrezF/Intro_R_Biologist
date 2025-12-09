


# install.packages("dplyr")
library(dplyr)
library(ggplot2)

data("starwars")
head(starwars, 3)
View(starwars)

str(starwars)

starwars %>%  
  filter(species == "Droid")

subset(starwars, species== "Droid")

filter(starwars, 
         species == "Droid")


starwars %>% 
  filter(skin_color=="light",
         eye_color== "brown" &
           hair_color == "black" )


starwars %>% 
  arrange(height, mass)


starwars %>% 
  arrange(desc(height))


starwars %>% 
  slice(5:10)

starwars %>% 
  slice_head(n=8)

starwars %>% 
  select(name, hair_color, skin_color,
         eye_color) %>% 
  filter(skin_color == "white")


# Selec() -----------------------------------------------------------------
starwars %>% 
  select(!name & !skin_color)

starwars %>% 
  select(-name, -skin_color)
  
starwars %>% 
  select(1, 2, species) %>% 
  filter(species== "Droid")

starwars %>% 
  select(contains('y'))


starwars %>% 
  select(starts_with('na'))


starwars %>% select(ends_with('or'))


starwars %>% 
  dplyr::rename(Character = name)


starwars %>% select(contains('c'))


starwars %>% 
  mutate(height_cat = ifelse(height > 100, "tall", "small")) %>% 
  ggplot(aes(x=height, fill= height_cat )) +
  geom_histogram()




# Mutate ------------------------------------------------------------------
starwars %>% 
  mutate(new_col= height/100) %>% 
  select(new_col, height, everything())

starwars %>% 
  mutate(new_col= height/100,
         .keep='none')

newstarwars <- starwars %>%
  mutate(height_m = height / 100)

newstarwars %>% 
  select(height_m)


starwars %>% 
  mutate(height_cat= ifelse(height >100,
                            "tall",
                            "small")) %>% 
  select(height, height_cat, everything())


starwars %>% 
  mutate(height_cat = ifelse(height > 100, 
                             "tall", "small")) %>% 
  ggplot(aes(x=height, fill= height_cat )) +
  geom_histogram() +
  labs(x='algo', y='otro')


starwars %>% 
  summarise(mean = mean(height, na.rm=T),
            min= min(height, na.rm=T),
            max= max(height, na.rm=T),
            sd= sd(height, na.rm=T)
            )


starwars %>% 
  group_by(species) %>% 
  summarise(
    mean= mean(mass, na.rm=T),
    sd= sd(mass, na.rm=T)
  ) %>% 
print(n=20) 


starwars_new <- starwars %>% 
  group_by(species) %>% 
  count(homeworld, sort=T)
  
  
starwars %>% sample_n(10, 
                      replace = T)
  
  