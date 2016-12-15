## extract names from meetups
# Adelaide, SA: does not exist
require("tidyverse")
require("genderizeR")
require("purrr")
require("rvest")
require("stringr")
require("XML")
require("reshape2")

get_gender <- function(event_url){
  
  # Scrap HTML, member names are names of attendants
  event_page <- event_url %>% 
    read_html()
  member_nodes <- event_page %>%
    html_nodes(".member-name")
  
  name_text_with_plusses <- member_nodes %>% 
    map(. %>% html_text() %>% str_trim()) %>%
    unlist()
  
  plusses <- sum(!is.na(str_extract(name_text_with_plusses,"[1-9]")))
  
  givenNames = data.frame(findGivenNames(name_text_with_plusses, 
                                         progress = FALSE, 
                                         apikey="a902c2108f17334e5a2d596c676d4fb9"))
  givenNames$probability <- as.numeric(as.character(givenNames$probability))
  
  mean_prob <- data.frame(givenNames) %>% 
    group_by(gender) %>% 
    summarise(Npeople = length(gender), 
              ProbEstFemale = mean(probability),
              Proportion = length(gender)/nrow(givenNames)) %>% 
    mutate(TotalPart=sum(Npeople))
  
  mean_prob$unknown <- plusses
  
  list(TableResults = mean_prob,
       givenNames = givenNames)
  
}

meetups <- data.frame(Name = c("Reunión Mayo 2016: ¡Big Data en Paradigma Digital!",
                               "Reunión Junio 2016: ¡Python y ciencia!",
                               "Reunión Julio 2016: Taller básico de Python Científico",
                               "Reunión Septiembre 2016: ¡PyDay Madrid!",
                               "Grupo de estudio: Reunión 1",
                               "Taller Color, Árboles en R y SAS y una píldora sobre Talentyon...",
                               "Un poco de MaRketing CustomeR Lifecycle Value y Watson Analytics vs R.",
                               "Segmentación de películas: Movielens",
                               "R en las III Jornadas de Periodismo de Datos",
                               "Tributo a Gregorio Serrano",
                               "Nueva reunión del Grupo de R de Madrid.",
                               "Nueva reunión del Grupo de R de Madrid - Jueves 12-Noviembre",
                               "Nueva reunión del Grupo de R de Madrid - Jueves 10-Diciembre",
                               "Reunión Grupo de R de Madrid - Jueves 14 de enero 2016",
                               "32 - Reunión Grupo de R de Madrid: Jueves 11 de febrero",
                               "33 - Reunión Grupo de R y Python de Madrid: Jueves 17 de marzo",
                               "34 - Reunión Grupo de R: Jueves 14 de abril",
                               "35 - Reunión Grupo de R: Jueves 12 de mayo",
                               "36 - Reunión Grupo de R: Jueves 9 de junio",
                               "37 - Reunión Grupo de R: Jueves 15 de septiembre",
                               "38 - Reunión Grupo de R: Jueves 13 de octubre"),
                      address = c("https://www.meetup.com/es-ES/Madrid-Python-Meetup/events/230489896/",
                                  "https://www.meetup.com/es-ES/Madrid-Python-Meetup/events/231559306/",
                                  "https://www.meetup.com/es-ES/Madrid-Python-Meetup/events/232582139/",
                                  "https://www.meetup.com/es-ES/Madrid-Python-Meetup/events/233978994/",
                                  "https://www.meetup.com/es-ES/Madrid-Python-Meetup/events/234911182/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/220646292/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/221104822/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/222140709/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/222764487/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/222955947/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/225660015/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/226088290/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/226906675/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/227983479/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/228578129/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/229050729/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/230110876/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/230458570/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/231334596/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/233805272/",
                                  "https://www.meetup.com/es-ES/Grupo-de-Usuarios-de-R-de-Madrid/events/234573490/"),
                        group = c("Madrid Python Meetup",
                                  "Madrid Python Meetup",
                                  "Madrid Python Meetup",
                                  "Madrid Python Meetup",
                                  "Madrid Python Meetup",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid",
                                  "Grupo de Usuarios de R de Madrid"),
                      Number = c(1,2,3,4,5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                      City = c(rep(c("Madrid_Python", "Madrid_R"), times = c(5, 16))))
meetups$key <- paste(meetups$City, meetups$Number, sep = "_")

# Run other R groups
urls <- as.character(meetups[,"address"])
names(urls) <- meetups[,"key"]
list_estimates <- urls %>% map(. %>% get_gender)
estimates <- melt(sapply(list_estimates, `[`, "TableResults"))

# Run R-Ladies
event_url <- "https://www.meetup.com/es-ES/rladies-mad/events/233829731/"
Rladies <- get_gender(event_url)
Rladies_summary <- as.data.frame(Rladies$TableResults)
Rladies_names <- as.data.frame(Rladies$givenNames)

saveRDS(estimates, "groups_r_gender.rds")
saveRDS(Rladies_summary, "rladies_gender.rds")
