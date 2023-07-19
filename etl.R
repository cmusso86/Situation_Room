rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf , rio, 
               ggthemes, DataExplorer, haven, corrplot)


## BASES ----

data_raw <-  import("disease.csv")


data_raw %>% 
  ggplot(aes(x=date_onset ))+
  geom_bar()+
  theme_classic(base_size = 12)

ggsave("epidemic2.png", width=10)

data_raw %>% 
  mutate(Sint=epiweek(date_onset),
         Not=epiweek(date_notific))%>% 
  select(Sint, Not)%>% 
  mutate(Not=ifelse(is.na(Not), Sint, Not)) %>% 
  #mntidyr::complete(expand(Sint, Not)=1:29, fill= list(n=0))
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  tidyr::complete(name, value = 1:29, fill = list( freq = 0)) %>% 
  #mutate(value=paste0("W-", value)) %>% 
  ggplot(aes(x=factor(value), y=freq, color=name, group=name))+
  geom_point(size=2)+
  geom_line(size=1)+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x="EpiWeek", y="Cases")

ggsave("epidemic.png", width=10)
plot_bar(data_raw )

summary(data_raw)

vac_region <- data_raw %>% 
  group_by(region) %>% 
  summarise(vaccnine = mean(vaccination=="yes")*100)


data <- data_raw %>% 
  filter(!is.na(hospital))%>% 
  ungroup() %>% 
  group_by(hospital) %>% 
  summarise(Tot=n())


%>% 
  group_by(sex) %>%
  summarise(n=n(),
            prop=n()/Tot )


  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

## Time series ----








## Symptoms ----


pacman::p_load(ggupset)


data_sintomas <- data_raw %>% 
  mutate(fever = ifelse(fever == "yes", "fever", NA), 
         jaundice = ifelse(jaundice == "yes", "jaundice", NA),
         faget_signal= ifelse(faget_signal == "yes", "faget_signal", NA)) %>% 
  unite(col = "all_symptoms",
        c(fever, jaundice, faget_signal), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    all_symptoms_list = as.list(strsplit(all_symptoms, "; "))
  )
  
  
class(data_sintomas)

graf <- ggplot(
  data = data_sintomas,
  mapping = aes(x = all_symptoms_list)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("fever", "jaundice", "faget_signal"))+
  labs(
    title = "Signs & symptoms YF",
    subtitle = "10 most frequent combinations of signs and symptoms",
    caption = "",
    x = "Symptom combination",
    y = "Frequency in dataset")+
  theme_bw(base_size = 16)

ggsave(plot=graf, "sintoma.png")



# Efficiently convert "yes" to 1 and 0
linelist_sym_2 <- data_raw %>% 
  
  # convert the "yes" and "no" values into 1s and 0s
  mutate(across(c(fever, jaundice, faget_signal), .fns = ~+(.x == "yes")))

# Make the plot
linelist_sym_2 %>% 
  UpSetR::upset(
    sets = c("fever", "jaundice", "faget_signal"),
    mb.ratio = c(0.5, 0.5),
    order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE),
    #sets.bar.color = c("blue", "red", "yellow"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Symptoms Combinations",
    sets.x.label = "Patients with Symptom")



movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=TRUE, sep=";" )

require(ggplot2); require(plyr); require(gridExtra); require(grid);

between <- function(row, min, max){
  newData <- (row["ReleaseDate"] < max) & (row["ReleaseDate"] > min)
}

plot1 <- function(mydata, x){
  myplot <- (ggplot(mydata, aes_string(x= x, fill = "color"))
             + geom_histogram() + scale_fill_identity()
             + theme(plot.margin = unit(c(0,0,0,0), "cm")))
}

plot2 <- function(mydata, x, y){
  myplot <- (ggplot(data = mydata, aes_string(x=x, y=y, colour = "color"), alpha = 0.5)
             + geom_point() + scale_color_identity()
             + theme_bw() + theme(plot.margin = unit(c(0,0,0,0), "cm")))
}

attributeplots <- list(gridrows = 55,
                       plots = list(list(plot = plot1, x= "ReleaseDate",  queries = FALSE),
                                    list(plot = plot1, x= "ReleaseDate", queries = TRUE),
                                    list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = FALSE),
                                    list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = TRUE)),
                       ncols = 3)

upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
      order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))

upset(movies, sets = c("Drama", "Comedy", "Action", "Thriller", "Western", "Documentary"),
      queries = list(list(query = intersects, params = list("Drama", "Action")),
                     list(query = between, params = list(1970, 1980), color = "red", active = TRUE)))

upset(movies, attribute.plots = attributeplots,
      queries = list(list(query = between, params = list(1920, 1940)),
                     list(query = intersects, params = list("Drama"), color= "red"),
                     list(query = elements, params = list("ReleaseDate", 1990, 1991, 1992))),
      main.bar.color = "yellow")


ggplot(
  data = data_sintomas,
  mapping = aes(x = all_symptoms)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("fever", "chills", "cough", "aches", "vomit"))+
  labs(
    title = "Signs & symptoms",
    subtitle = "10 most frequent combinations of signs and symptoms",
    caption = "Caption here.",
    x = "Symptom combination",
    y = "Frequency in dataset")



mb.ratio = c(0.5, 0.5),
order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE)
## MAP -----

map <- st_read("map.gpkg")
st_crs(map) <- 4326
map_transformed <- st_transform(map, crs = 4326)

(ggmap <- spatial_data_transformed %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=hdi))+
  scale_fill_distiller(direction=1)+
  theme_map())


## other plot
plot(map["hdi"])