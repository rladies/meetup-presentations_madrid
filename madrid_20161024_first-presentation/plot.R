library(dplyr)
library(ggplot2)
library(extrafont)

speakers <- read.csv('Speakers R.csv', header = TRUE)
meetup <- read_rds('groups_r_gender.rds')
encuesta <- read.csv('R-Ladies pre-first Madrid-report.csv')

## Comunidad Hispano de R

speakers_gender <- speakers %>%
  filter(gender %in% c("M", "H")) %>%
  arrange(desc(gender)) %>% 
  group_by(año, gender) %>%
  summarise(n = n()) %>%
  mutate(per = round(n/sum(n)*100, 2))

ggplot(data = speakers_gender, aes(x = factor(año), y = n)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = factor(gender))) + 
  geom_text(aes(label = paste0(per, '%'), group = gender), 
            position = position_dodge(w = 0.9), vjust = -0.5, size = 3) +
  theme_minimal(base_size = 13) +
  labs(title = "Diferencia de género en el cartel de las Jornadas de R en España",
       x = "Año") +
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values = c("#D3D3D3", "#88398A"),
                    name = "Género") +
  theme(text = element_text(size=12, family="Arial")) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("cartel_jornadasr.png")

# Meetup

meetup_gender <- meetup %>%
  filter(variable == 'Proportion') %>%
  mutate(Evento = L1) %>%
  select(-c(L1, variable)) %>%
  mutate(Evento = gsub(".TableResults", "", Evento)) %>%
  mutate(Tipo = ifelse(grepl("Python", Evento), "Python","R"))

ggplot(data = meetup_gender, 
       aes(x = factor(Evento), y = value, fill = factor(gender))) + 
  geom_bar(stat = 'identity') +
  theme_minimal(base_size = 13) +
  labs(title = "Grupos Python Madrid y R Madrid desglosados",
       x = "Evento") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("#88398A", "#D3D3D3"),
                    name = "Género",
                    labels = c("M", "H")) +
  theme(text = element_text(size=12, family="Arial"))

ggsave("proporcion_meetup.png")

meetup_totalpersonas <- meetup %>%
  filter(variable == 'Npeople') %>%
  mutate(Evento = L1) %>%
  select(-c(L1, variable)) %>%
  mutate(Evento = gsub(".TableResults", "", Evento)) %>%
  mutate(Evento = gsub("Madrid_", "", Evento)) %>%
  mutate(Tipo = ifelse(grepl("Python", Evento), "Python","R")) %>%
  group_by(Evento) %>%
  mutate(per = round(value/sum(value)*100, 2))

ggplot(data = meetup_totalpersonas, 
       aes(x = factor(Evento), y = value, fill = factor(gender))) + 
  geom_bar(stat = 'identity') +
  facet_grid(~Tipo, scales="free") + 
  theme_minimal(base_size = 13) +
  labs(title = "Grupos Python Madrid y R Madrid desglosados",
       x = "Evento") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_fill_manual(values = c("#88398A", "#D3D3D3"),
                    name = "Género",
                    labels = c("M", "H")) +
  theme(text = element_text(size=12, family="Arial")) 

ggsave("totalpersonas_meetup.png")


# Encuesta

encuesta_level <- encuesta %>%
  group_by(X.Cuál.es.tu.nivel.de.R.) %>%
  summarize(n = n()) %>%
  mutate(X.Cuál.es.tu.nivel.de.R. = factor(X.Cuál.es.tu.nivel.de.R., 
                                           levels = c("¿R? ¿Cómo en Roquefort?",
                                                      "Sé algo, pero como las meigas.",
                                                      "Me manejo, lo uso de vez en cuando.",
                                                      "Es mi compañero de trabajo, me espera todas las mañanas.",
                                                      "Lo uso hasta para hacerme las tostadas.")))

ggplot(data = encuesta_level, 
       aes(x = X.Cuál.es.tu.nivel.de.R., y = n)) + 
  geom_bar(stat = 'identity', fill = "#88398A") +
  theme_minimal(base_size = 13) +
  labs(title = "Resultados de la encuesta") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(text = element_text(size=12, family="Arial")) 

ggsave("nivel_encuesta.png")

encuesta_temas <- data.frame(tema = c("Tutoriales.de introducción a R", "Tutoriales de paquetes de R",
                                      "Otros tutoriales de R", "Presentaciones de investigación", 
                                      "Presentaciones de trabajos en R",
                                      "Ofertas de trabajo de R", "Networking", "Mentoring", "Otros"),
                             n = c(10, 22, 11, 17, 20, 12, 17, 12, 3))

ggplot(data = encuesta_temas, 
       aes(x = factor(tema), y = n)) + 
  geom_bar(stat = 'identity', fill = "#88398A") +
  theme_minimal(base_size = 13) +
  labs(title = "Resultados de la encuesta") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(text = element_text(size=12, family="Arial"))

ggsave("temas_encuesta.png")
