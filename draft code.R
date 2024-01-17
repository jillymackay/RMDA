library(tidyverse)

plants <- tibble(none = c(4.8, 4.8, 3.94, 4.4,4.5,4.6),
                 nutrients1  = c( 10.1, 9.7, 9.8, 9.9, 9.3, 10.1),
                 nutrients2 = c(14.8, 14.6, 14.8, 14, 13.8, 14.6))

plants |> 
  pivot_longer(cols = c(none, nutrients1,nutrients2),
               names_to = "nutrients",
               values_to = "height") |> 
  ggplot(aes(x = nutrients, y = height)) +
  geom_boxplot()


longplants <- plants |> 
  pivot_longer(cols = c(none, nutrients1,nutrients2),
                             names_to = "nutrients",
                             values_to = "height")

plant_model <- aov(height ~ nutrients, data = longplants)
summary(plant_model)






crudechicks <- tibble(year = c("2000", "2001", "2002", "2003",
                               "2004", "2005", "2006", "2007",
                               "2008", "2009"),
                      chicken = c(54.2, 54, 56.8, 57.5, 59.3, 60.5, 60.9,
                                  59.9, 58.7, 56),
                      crude = c(3311, 3405, 3336, 3521, 3674, 3670, 3685,
                                3656, 3571, 3307))


cor.test(crudechicks$chicken, crudechicks$crude, method = "spearman")


# Week 2


starwars |> 
  filter(species == "Human") |> 
  ggplot(aes(x = height, y = mass, colour = sex)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex")


starwars |> 
  filter(species == "Human") |> 
  mutate(BMI = case_when((mass/(height^2)) > 25 ~ "High BMI",
                         (mass/(height^2)) > 18 ~ "Healthy BMI",
                         TRUE ~ "Low BMI")) |> 
  ggplot(aes(x = height, y = mass, colour = sex)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex") +
  geom_smooth(method = lm, se = FALSE)


starwars |> 
  filter(species == "Human") |> 
  mutate(BMI = case_when((mass/(height^2)) > 25 ~ "High BMI",
                           (mass/(height^2)) > 18 ~ "Healthy BMI",
                           TRUE ~ "Low BMI")) |> 
  ggplot(aes(x = height, y = mass, colour = sex, shape = BMI)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,150)) +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  theme(legend.position = "bottom") + 
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       title = "Height and Weight of Human Characters in Star Wars by Sex") +
  geom_smooth(method = lm, se = FALSE)



starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 3, fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Count",
       title = "Histogram of height of male human Star Wars characters")


starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 8, fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Count",
       title = "Histogram of height of male human Star Wars characters")


starwars |> 
  filter(species == "Human",
         sex == "male") |>
  ggplot(aes(x = height)) +
  geom_density(fill = "#bb9cd1") +
  theme_classic() +
  labs(x = "Height (cm)",
       y = "Density",
       title = "Density plot of height of male human Star Wars characters")



starwars |> 
  filter(species == "Human") |>
  ggplot(aes(y = height, x = sex, colour = sex)) +
  geom_boxplot() +
  theme_classic() +
  scale_colour_brewer(palette = "Accent", name = "Sex") +
  labs(y = "Height (cm)",
       x = "Sex",
       title = "Boxplot of height by sex of human characters in Star Wars") +
  theme(legend.position ="none")

hums <- starwars |> 
  filter(species == "Human")

humsm <- lm(height ~ sex, data = hums)
summary(humsm)
report::report(humsm)


starwars |> 
  filter(species == "Human") |> 
  group_by(sex) |> 
  summarise(ht = mean(height, na.rm = TRUE),
            sd = sd(height, na.rm = TRUE)) |> 
  ggplot(aes(x = sex, y = ht, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ht-sd, ymax = ht+sd), width = 0.2)+
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Mean Height (cm)",
       x = "Sex",
       title = "Mean height by sex of human characters in Star Wars") +
  theme_classic() +
  theme(legend.position = "none")



starwars |> 
  filter(species == "Human") |> 
  group_by(sex) |> 
  summarise(ht = mean(height, na.rm = TRUE),
            sd = sd(height, na.rm = TRUE)) |> 
  ggplot(aes(x = sex, y = ht, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ht-sd, ymax = ht+sd), width = 0.2)+
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Mean Height (cm)",
       x = "Sex",
       title = "Mean height by sex of human characters in Star Wars") +
  theme_classic() +
  theme(legend.position = "none") + 
  coord_flip()


library(likert)

dat <- tibble (grp = c("Dislikes Star Wars","Likes Star Wars","Dislikes Star Wars","Likes Star Wars","Dislikes Star Wars","Likes Star Wars"),
               `How helpful was this lecture?` = c("No help", "No help", "No help", "Helpful", NA, "Helpful"),
               `How helpful was this course?` = c("Unsure", "No help", "Helpful", "Unsure", "Helpful", "Helpful")) |> 
  mutate_at (.vars = vars(`How helpful was this course?`:`How helpful was this lecture?`), 
             .funs = funs(factor(., levels = c("No help",
                                               "Unsure",
                                               "Helpful"))))   |>  
  as.data.frame()

dlik <- likert (items = dat[,2:3], grouping = dat[,1])
plot(dlik)


library(ggstatsplot)
starwars |> 
  mutate(spec = fct_lump_n(species, 1),
         eye = fct_lump_n(eye_color,1)) |> 
  select(spec, eye) |> 
  ggbarstats(x = eye, y = spec) +
  labs(x = "Species (Human vs Other)",
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3", name = "Eye Colour (Brown vs other)") +
  theme_classic()


starwars |> 
  mutate(spec = fct_lump_n(species, 1),
         eye = fct_lump_n(eye_color,1)) |> 
  select(spec, eye) |> 
  ggbarstats(x = eye, y = spec) +
  labs(x = "Species (Human vs Other)",
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3", name = "Eye Colour (Brown vs other)") +
  theme_light()


library(vcd)


startbl <-  starwars |> 
  mutate(Species = fct_lump_n(species, 2),
         EyeColour = fct_lump_n(eye_color,2)) 
startbl <-  structable(Species ~ EyeColour, startbl)
mosaic(startbl, highlighting = EyeColour)

mosaic(~ Species + EyeColour, data = startbl, highlighting = "Species",
       highlighting_fill = c("#A6CEE3", "#B2DF8A", "#FB9A99"))


mosaic(~ Species + EyeColour, data = startbl,shade = TRUE, legend = TRUE)





starwars |> 
  mutate(species = fct_lump_n(species, 4)) |> 
  group_by(species) |> 
  filter(!is.na(species)) |> 
  tally() |> 
  ggplot(aes(x = "", fill = species, y = n)) +
  geom_bar(stat = "identity", width = 1) +
  theme_void() +
  coord_polar("y", start = 0)


starwars |> 
  mutate(species = fct_lump_n(species, 4)) |> 
  group_by(species) |> 
  filter(!is.na(species)) |> 
  tally() |> 
  ggplot(aes(x = species, fill = species, y = n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Species", y = "Count") +
  theme(legend.position = "none")



library(easystats)
library(BayesFactor)

starcorr <- correlationBF(starwars$height, starwars$mass)
describe_posterior(starcorr)
bayesfactor_models(starcorr)

plot(bayesfactor_models(starcorr)) +
  scale_fill_pizza() +
  labs(title = "Bayes Factor 'Pizza Plot' for A Bayesian correlation between height and weight for Star Wars characters")




library (tidytext)
library (wordcloud)
library (textstem)
library (janeaustenr) 

austen <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  mutate (lemma = (lemmatize_strings(word))) %>%
  anti_join(stop_words)


austen %>%
  count (lemma) %>%
  with (wordcloud(words = lemma, freq = n, max.words = 200, random.order = FALSE, rot.per = 0,
                  colors = brewer.pal(12, "Paired"), use.r.layout = FALSE))
starwars |> 
  count(homeworld) |> 
  with(wordcloud(words = homeworld, freq = n, min.freq=1, random.order = FALSE, rot.per = 0,
                 colors = brewer.pal(6, "Accent"), use.r.layout = FALSE))

starwars |> 
  count(homeworld) |> 
  ggplot(aes(x = reorder(homeworld, desc(n)), y = n, fill = as.factor(n))) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Homeworld", y = "Count") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip()








starwars |> 
  mutate(species = fct_lump_n(species,2)) |> 
  filter(!is.na(species)) |> 
  ggplot(aes(x = species)) +
  geom_point(aes(y = height, colour = species), position = position_jitter(width = .13), size = 1, alpha = 0.6) +
  see::geom_violinhalf(aes(y = height, alpha= 0.3, fill = species), linetype = "dashed", position = position_nudge(x = .2)) +
  geom_boxplot(aes(y = height, alpha = 0.3, colour = species), position = position_nudge(x = -.1), width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(x = "Species", y = "Height (cm)") +
  theme(legend.position = "none") +
  coord_flip()


starwars |>
  select(height, mass, birth_year) |> 
  ggcorrmat()

starwars |>
  mutate(col = fct_lump_n(species, 2)) |> 
  ggplot(aes(x = birth_year, y = mass, size = height, colour = col)) +
  geom_point() +
  scale_size(range = c(.1, 24), name="Height") +
  theme_classic() +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(0,300)) +
  scale_colour_brewer(palette = "Accent", name = "Species") +
  theme(legend.position = "bottom") + 
  labs(x = "Birth  Year (Before Battle of Yavin)",
       y = "Weight (kg)",
       title = "Weight by Age of Characters in Star Wars") 



starwars |> 
  mutate(species = fct_lump_n(species,2)) |> 
  filter(!is.na(species)) |> 
  ggplot(aes(x = species, y = height, fill = species)) +
  geom_violin() +
  theme_classic() +
  labs(x = "Species", y = "Height (cm)") +
  theme(legend.position = "none")  +
  scale_fill_brewer(palette = "Accent")


# Week 4 

cat_weights <- tibble(avg_daily_snacks  = c(3, 2, 4, 2, 3, 1, 1, 0, 1, 0, 2, 3, 1, 2, 1, 3),
                      weight = c(3.8, 3.9, 5, 3.7,  4.1, 3.6, 3.7, 3.6, 3.8, 4.1, 4.3, 3.9, 3.7, 3.8, 3.5, 4.3))


cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5))

cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) + 
  geom_abline(slope = 1.998, intercept = -6.028)


cat_weights |> 
  ggplot(aes(x = avg_daily_snacks, y = weight)) +
  geom_point() +
  labs(x = "Average Daily Snacks", y = "Cat Weight") +
  theme_classic() +
  scale_y_continuous(limits = c(0,5)) +
  geom_abline(slope = 0.204, intercept = 3.55)

model_cat <- lm(avg_daily_snacks ~ weight, data = cat_weights)
summary(model_cat)
report::report(model_cat)


model_cat2 <- lm(weight ~ avg_daily_snacks, data = cat_weights)
summary(model_cat2)
report::report(model_cat2)