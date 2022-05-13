# load in packages and fonts ----
library(tidyverse)
library(janitor)
library(cowplot)
library(ggimage)
library(here)
library(magick)
library(ggtext)
library(showtext); showtext_auto()
font_add_google("Loved by the King", "king")
font_add_google("Amatic SC", "amatic")
font_add_google("Just Me Again Down Here", "me_again")

# type colours ----
type_col <- c(Bug = "#6a8b5a", Dark = "#414152", Dragon = "#5a8bee", 
              Electric = "#f6e652", Fairy = "#ffd5bd", Fighting = "#b40000", 
              Fire = "#ee8329", Flying = "#6ab4e6", Ghost = "#8b6283", Grass = "#20b49c", 
              Ground = "#c57341", Ice = "#e6e6f6", Normal = "#E8E8E8", 
              Poison = "#a483c5", Psychic = "#f65273", Rock = "#e6d5ac", 
              Steel = "#A1A1A1", Water = "#083962")

scales::show_col(type_col)

# load data ----
pokemon_raw <- read_csv("https://raw.githubusercontent.com/andrewmoles2/webScraping/main/R/data/pokemon.csv") %>%
  clean_names()

# data cleaning ----
# pivot data longer over statistics hp, speed etc., and tidy names
pokemon_long <- pokemon_raw |>
  pivot_longer(
    cols = c(hp:speed),
    names_to = "stats",
    values_to = "value"
  ) |>
  mutate(
    name = str_squish(name),
    name = factor(name)
    )

# add fill colour for each stat, and tidy names of statistics for plotting
pokemon <- pokemon_long |> 
  mutate(fill = case_when(
    stats == "hp" ~ "#ffd5bd",
    stats == "attack" ~ "#b40000",
    stats == "defense" ~ "#e6d5ac",
    stats == "sp_atk" ~ "#f65273",
    stats == "sp_def" ~ "#5a8bee",
    stats == "speed" ~ "#f6e652",
    TRUE ~ stats
  )) |> 
  group_by(stats) |> 
  mutate(stats = case_when(
    stats == "sp_atk" ~ "special-attack",
    stats == "sp_def" ~ "special-defense",
    TRUE ~ stats
  )) |> 
  ungroup() |> 
  mutate(stats = factor(stats))

# add second fill for each pokemon type, fire is orange for example  
pokemon <- pokemon |> 
  mutate(fill2 = case_when(
    type1 == "Bug" ~ "#6a8b5a",
    type1 == "Dark" ~ "#414152",
    type1 == "Dragon" ~ "#5a8bee",
    type1 == "Electric" ~ "#f6e652",
    type1 == "Fairy" ~ "#ffd5bd",
    type1 == "Fighting" ~ "#b40000",
    type1 == "Fire" ~ "#ee8329",
    type1 == "Flying" ~ "#6ab4e6",
    type1 == "Ghost" ~ "#8b6283",
    type1 == "Grass" ~ "#20b49c",
    type1 == "Ground" ~ "#c57341",
    type1 == "Ice" ~ "#e6e6f6",
    type1 == "Normal" ~ "#E8E8E8",
    type1 == "Poison" ~ "#a483c5",
    type1 == "Psychic" ~ "#f65273",
    type1 == "Rock" ~ "#e6d5ac",
    type1 == "Steel" ~ "#A1A1A1",
    type1 == "Water" ~ "#083962",
    TRUE ~ type1
  ))

# adjust ordering of stats for plotting
fc_levels = c("hp", "attack", "defense", "special-attack", "special-defense", "speed")
pokemon <- pokemon |> 
  mutate(stats = factor(stats, levels = fc_levels)) |> 
  group_by(name) |> 
#  arrange(desc(value)) |> 
  mutate(id = row_number()) |> 
  ungroup()

# plotting ----
# extract favourite pokemon for loop
# note: geom_pokemon only works for gens 1 and 2! 
fav_pokemon <- c("Arcanine", "Charizard", "Zapdos", "Gyarados", "Lapras", "Porygon2")

fav_pokemon_df <- pokemon |> 
  filter(name %in% fav_pokemon)

# loop to make plot for each selected pokemon and save output
for (i in unique(as.character(fav_pokemon_df$name))) {
  
  p <- ggdraw(
    pokemon |> 
      filter(name == i) |> 
      ggplot() +
      geom_segment(data = data.frame(y=seq(0,250,25)), aes(x=-0.5, xend=6.5, y=y, yend=y), linetype = 2, colour = "grey90") +
      geom_text(data = data.frame(y=seq(0,250,25)), aes(x=0.06, y=y+0.5, label=y), family="king", size=12, fontface="bold") +
      geom_col(aes(x=id, y=value, fill=fill2), show.legend = FALSE, alpha = 0.85) +
      geom_pokemon(aes(x=0.5, y =-165.5, image=str_to_lower(name)), size=0.155) +
      geom_text(aes(x=id,y=160,label=stats), size=16, fontface="bold", family="king") +
      geom_text(aes(label=str_wrap(name,20)), x=-0.5, y=-40, size=16, fontface = "bold", family = "me_again") +
      geom_text(aes(label=strwrap(
        ifelse(is.na(type2), paste(type1), paste(type1, "&", type2)),20)), 
        x=-0.5, y=-65, size = 10, family = "me_again") +
      coord_polar(clip = "off") +
      scale_fill_identity() +
      theme_void() +
      theme(plot.margin = margin(1.5,0,0,0, unit = "cm")) +
      geom_hline(yintercept = 90, linetype = "ff", colour = "darkgrey")
  ) +
    draw_text("Pokémon",    x = 0.5 , y = 0.95, size = 80, family = "amatic") + 
    draw_text("Strengths and weaknesses of each Pokémon",    x = 0.5 , y = 0.90, size = 55, family = "amatic") +
    draw_text("Statistics: over 90 is strong",       x = 0.5 , y = 0.86, size = 40, family = "amatic") +
    draw_text("Data: Pokémon Database | Graphic: Andrew Moles", x = 0.435 , y = 0.37, size = 12, family = "me_again", hjust = -0, color = "grey30")
  
  print(p)
  
 ggsave(filename = here("outputs", paste0(i,".png")),
        dpi = 350, height = 6, width = 6, bg = "white")
  
}


# single plot ----
# testing with just one pokemon - testing with Mew
ggdraw(
  pokemon |> 
    filter(name == "Mew") |> 
    ggplot() +
    geom_segment(data = data.frame(y=seq(0,250,25)), aes(x=-0.5, xend=6.5, y=y, yend=y), linetype = 2, colour = "grey90") +
    geom_text(data = data.frame(y=seq(0,250,25)), aes(x=0.06, y=y+0.5, label=y), family="king", size=12, fontface="bold") +
    geom_col(aes(x=id, y=value, fill=fill2), show.legend = FALSE, alpha = 0.85) +
    geom_pokemon(aes(x=0.5, y =-165.5, image=str_to_lower(name)), size=0.155) +
    geom_text(aes(x=id,y=160,label=stats), size=16, fontface="bold", family="king") +
    geom_text(aes(label=str_wrap(name,20)), x=-0.5, y=-40, size=16, fontface = "bold", family = "me_again") +
    geom_text(aes(label=strwrap(
      ifelse(is.na(type2), paste(type1), paste(type1, "&", type2)),20)), 
      x=-0.5, y=-65, size = 10, family = "me_again") +
    coord_polar(clip = "off") +
    scale_fill_identity() +
    theme_void() +
    theme(plot.margin = margin(1.5,0,0,0, unit = "cm")) +
    geom_hline(yintercept = 90, linetype = "ff", colour = "darkgrey")
) +
  draw_text("Pokémon",    x = 0.5 , y = 0.95, size = 80, family = "amatic") + 
  draw_text("Strengths and weaknesses of each Pokémon",    x = 0.5 , y = 0.90, size = 55, family = "amatic") +
  draw_text("Statistics: over 90 is strong",       x = 0.5 , y = 0.86, size = 40, family = "amatic") +
  draw_text("Data: Pokémon Database | Graphic: Andrew Moles", x = 0.435 , y = 0.37, size = 12, family = "me_again", hjust = -0, color = "grey30")

ggsave(filename = here("outputs", paste0("Mew",".png")),
       dpi = 350, height = 6, width = 6, bg = "white")

# Make gif of selected pokemon to display on README ----
list.files(path = here("outputs"), pattern = ".png", full.names = T) %>%
  lapply(., image_read) %>%
  image_join() %>%
  image_animate(fps = 1) %>%
  image_write(path = "pokemon.gif")

# line by line method
#images <- list.files(here("outputs"), full.names = TRUE)
#images_list <- lapply(images, image_read)
#images_joined <- image_join(images_list)
#images_anim <- image_animate(images_joined, fps = 2)
#images_anim
#image_write(image = images_anim,
#            path = "pokemon.gif")


# things to do:
# loop over fav pokemon and save - done
# adjust text fonts, colours
# adjust line colours (slightly darker)
# use magic lib to grid 6 fav pokemon - without title

# testing out things with magick ----
arcanine <- image_read("outputs/Arcanine.png")
pory <- image_read("outputs/Porygon2.png")
zap <- image_read("outputs/Zapdos.png")
gary <- image_read("outputs/Gyarados.png")
img <- c(arcanine, pory, zap, gary)
img <- image_scale(img, "300x300")
#image_append(img)
# make into grid style - might be a cool way to viz Ash team - just remove titles...
image_montage(img, tile = "2x2")

# testing pulling pokemon image from pokemon database ----
# https://pokemondb.net/sprites
pokemon_raw |> 
  filter(name == "Togekiss")

pokemon_raw |> 
  filter(type2 == "Flying") |> 
  ggplot(aes(x = attack, y = sp_atk)) +
  geom_image(image = "https://img.pokemondb.net/sprites/home/normal/togekiss.png",
            size = 0.08)

pokemon_raw |> 
  filter(type2 == "Flying") |> 
  ggplot(aes(x = attack, y = sp_atk)) +
  geom_image(image = "https://img.pokemondb.net/sprites/bank/normal/togekiss.png",
             size = 0.08)

pokemon_raw |> 
  filter(type2 == "Dragon") |> 
  ggplot(aes(x = attack, y = sp_atk)) +
  geom_image(image = "https://img.pokemondb.net/sprites/home/normal/salamence.png",
             size = 0.08)

# test constructing link for select pokemon
set.seed(12)
x <- subset(pokemon_raw, generation >= 3)
n <- sample(x$name, 5, replace = F) |> tolower()
             
paste0("https://img.pokemondb.net/sprites/home/normal/", n,".png")

# it works! Now can integrate this to add column into script
# will use ggimage instead of geom_pokemon

# testing out adding coloured text (bit buggy)
title <- "<span style='color:#2F7AC0;'>Strengths</span>and weaknesses<br> of each<span style='color:#E9E32E;'>Pokémon</span>"
# #2F7AC0 - blue
# #E9E32E - yellow 
pokemon_raw |> 
  filter(type2 == "Dragon") |> 
  ggplot(aes(x = attack, y = sp_atk)) +
  geom_image(image = "https://img.pokemondb.net/sprites/home/normal/salamence.png",
             size = 0.08) +
  theme_bw() +
  labs(title = title) +
  theme(plot.title = element_markdown())

