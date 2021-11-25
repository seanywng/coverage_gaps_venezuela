# this is going to take too long. I don't really have patience for this now, just do it later

ven2 %>% 
  mutate(actividad_short = 
           str_replace_all(actividad, 
                           c("E1.01 reparaciones menores" = "E 1.1 APOYO A REPARACIONES MENORES EN LOS CENTROS EDUCATIVOS.",
                             "E1.02 alimentacion escolar" = "E 1.2 APOYO A INICIATIVAS PARA FORTALECER LOS PROGRAMAS DE ALIMENTACION ESCOLAR."
                             "E1.03 kits escolares" = "E 1.3 ENTREGA DE KITS DE MATERIALES ESCOLARES.",
                             "E1.04 materiales ensenanza" = "E 1.4 ENTREGA DE MATERIALES PARA LA ENSENANZA.",
                             "E1.05 incetivos maestros" = "E 1.5 ENTREGA DE INCENTIVOS A MAESTROS PARA LA PERMANENCIA DE LOS SERVICIOS EDUCATIVOS",
                             "E1.06 campanas asistencia permanencia escolar" = "E 1.6 DESARROLLO DE CAMPANAS CON NNA PARA FOMENTAR LA ASISTENCIA Y LA PERMANENCIA ESCOLAR.",
                             "E1.07 reinsercion escolar" = "E 1.7 DESARROLLO DE          28  
  ESTRATEGIAS PEDAGOGICAS CON          
   NNA EN SITUACION DE MAYOR           
  VULNERABILIDAD PARA PROMOVER         
    SU REINGRESO AL SISTEMA            
            ESCOLAR."
                             
                           )))

rename(percent_pobre_ham_2019 = ham_2019_xx_pobreza_env_por_parroquia,
       percent_sin_saneamiento_mejorado = x_saneamiento_percent_sin_saneamiento_mejorado,
       percent_no_tiene_servicio_electrico = 
         servicio_electrico_percent_no_tiene_servicio_electrico,
       percent_analfabeto = percent_poblacion_10_anos_y_mas_analfabeta,
       percent_sin_agua_segura = x_abast_agua2_percent_sin_agua_segura,
       poblacion_2019 = x_2019_poblacion_parroquial_total,
       personas_pobre_2019 = ham_2019_xx_poblacion_pobre_por_parroquia,
       densidad_ppl_km2 = densidad_poblacional_ppl_km2) %>% 
  
  rename(poblacion_2019 = x_2019_poblacion_parroquial_total,
         percent_pobre_ham_2019 = ham_2019_xx_pobreza_env_por_parroquia,
         percent_sin_saneamiento_mejorado = x_saneamiento_percent_sin_saneamiento_mejorado,
         percent_no_tiene_servicio_electrico = 
           servicio_electrico_percent_no_tiene_servicio_electrico,
         percent_analfabeto = percent_poblacion_10_anos_y_mas_analfabeta,
         percent_sin_agua_segura = x_abast_agua2_percent_sin_agua_segura,
         poblacion_2019 = x_2019_poblacion_parroquial_total,
         personas_pobre_2019 = ham_2019_xx_poblacion_pobre_por_parroquia,
         densidad_ppl_km2 = densidad_poblacional_ppl_km2) %>%
  
  act_parr <- act_ben %>% 
  filter(categoriadeactividad != "Vacunacion") %>% 
  group_by(estado, municipio, pcode3) %>% 
  summarise(beneficiarios = sum(beneficiarios)) %>% 
  ungroup() %>% 
  right_join(cen_ref %>% 
               select(estado, pcode1, municipio, pcode2, parroquia, pcode3, 
                      poblacion_2019, percent_pobre_ham_2019, poblacion_urbana_percent,
                      densidad_ppl_km2, razon_de_dependencia_total, percent_sin_agua_segura, 
                      percent_sin_saneamiento_mejorado, percent_analfabeto,
                      promedio_de_personas_por_vivienda, percent_no_tiene_servicio_electrico,
                      promedio_de_edad, poblacion_menor_de_18_percent, 
                      poblacion_18_y_mas_percent, ham_2019_ambitos_ge,
                      ham_2019_x_violencia_envelope), by = "pcode3") %>% 
  mutate(beneficiarios = ifelse(is.na(beneficiarios), 0, beneficiarios),
         not_covered_pobre = poblacion_2019 * percent_pobre_ham_2019 - beneficiarios,
         coverage_percent = beneficiarios / poblacion_2019,
         coverage_pobre   = beneficiarios / poblacion_2019 * percent_pobre_ham_2019,
         percent_total_ben = beneficiarios / sum(beneficiarios),
         org_present = ifelse(beneficiarios > 0, TRUE, FALSE)) %>% 
  select(-c(estado.x, municipio.x)) %>% 
  rename(estado = estado.y, 
         municipio = municipio.y) %>%
  # do you need this? maybe you should do one for each sector
  mutate(coverage_percent = beneficiarios / poblacion_2019,
         coverage_pobre   = beneficiarios / poblacion_2019 * percent_pobre_ham_2019)


#correlations
u_parr %>% 
  select(percent_pobre_ham_2019, poblacion_urbana_percent, 
         densidad_ppl_km2,  razon_de_dependencia_total, percent_sin_agua_segura,  
         percent_sin_saneamiento_mejorado,  percent_analfabeto, 
         promedio_de_personas_por_vivienda, percent_no_tiene_servicio_electrico, 
         ham_2019_x_violencia_envelope) %>% 
  cor() %>% 
  corrplot(tl.cex = 0.5)

# you've done all this -- this is just for reference
# I think maybe include all the parroquias in the tree
# mutate a new variable that is the scaled mean of pob_pobre and percent_pobre

my.scale = function(x,na.rm=TRUE) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

parr %>% 
  mutate(pob_pobre_score     = rescale(pob_pobre, to = c(0,1)), 
         percent_pobre_score = rescale(percent_pobre_2019, to = c(0,1)), 
         poverty_score = (pob_pobre_score + percent_pobre_score) / 2) %>% 
  select(pcode3, pob_pobre_score, percent_pobre_score, poverty_score) %>% 
  glimpse()

arrange(desc(poverty_score))

#
#
# for reporting -- FILTER then SUM
filter(mpg, manufacturer =="audi") %>% {sum(.$cty)}
#
#
#

# I think clustering is no longer necessary -- the tree performs pretty well
hc1 <- u_parr %>% 
  select(percent_pobre_ham_2019, poblacion_urbana_percent, 
         densidad_ppl_km2,  razon_de_dependencia_total, percent_sin_agua_segura,  
         percent_sin_saneamiento_mejorado,  percent_analfabeto, 
         promedio_de_personas_por_vivienda, percent_no_tiene_servicio_electrico, 
         ham_2019_x_violencia_envelope) %>% 
  scale() %>% dist() %>% hclust(method = "complete") 

u_parr %>% 
  mutate(cluster1 = cutree(hc1, k = 8)) %>%
  group_by(cluster1) %>% 
  summarise(poblacion = sum(poblacion_2019),
            pob_urbana = sum(poblacion_urbana_percent * poblacion_2019),
            pob_pobre = sum(percent_pobre_ham_2019 * poblacion_2019), 
            pob_sin_saneamiento_mejorado = sum(percent_sin_saneamiento_mejorado * poblacion_2019), 
            not_covered_pobre = sum(not_covered_pobre)) %>% 
  mutate(percent_urbana = pob_urbana / poblacion,
         percent_pobre = pob_pobre / poblacion, 
         percent_saneamiento = pob_sin_saneamiento_mejorado / poblacion, 
         percent_not_covered = not_covered_pobre / poblacion)

hc_u_parr <- hclust(u_parr_dist, method = "complete")

plot(hc_u_parr, type = "triangle")

relocate(`3`, .after = var_name) %>% 
  relocate(`11`, .after = `3`) %>% 
  rename(W = `3`, X = `11`, Y = `10`, Z = `4` ) %>% 
  
  # for finding the optimal number of nodes
  plotcp(tree3)

rpart.rules.table(tree3)
expand_limits(x = c(0, length(levels(parr0$estado)) + 1))


# can you try figuring out a function to do this? 
# you did figure it out

rbind(parr %>% 
        filter(not_covered_pobre <= 0) %>% 
        summarise(parroquias = n(),
                  beneficiarios = sum(beneficiarios),
                  avg_org_count = mean(org_count),
                  percent_pobre = (sum(pob_pobre)) / (sum(poblacion_2019)),
                  percent_urbana = (sum(pob_urbana)) / (sum(poblacion_2019)),
                  percent_sin_agua_segura = (sum(pob_sin_agua_segura)) / (sum(poblacion_2019)),
                  percent_sin_saneamiento_mejorado = (sum(pob_sin_saneamiento_mejorado, na.rm = TRUE)) /
                    (sum(poblacion_2019, na.rm = TRUE))) %>% 
        mutate_if(is.numeric, format, digits = 3), 
      
      parr %>% 
        filter(beneficiarios >= 1) %>% 
        summarise(parroquias = n(), 
                  beneficiarios = sum(beneficiarios),
                  avg_org_count = mean(org_count),
                  percent_pobre = (sum(pob_pobre)) / (sum(poblacion_2019)),
                  percent_urbana = (sum(pob_urbana)) / (sum(poblacion_2019)),
                  percent_sin_agua_segura = (sum(pob_sin_agua_segura)) / (sum(poblacion_2019)),
                  percent_sin_saneamiento_mejorado = (sum(pob_sin_saneamiento_mejorado, na.rm = TRUE)) /
                    (sum(poblacion_2019, na.rm = TRUE))) %>% 
        mutate_if(is.numeric, format, digits = 3), 
      
      parr %>% filter(beneficiarios < 1) %>% 
        summarise(parroquias = n(),
                  beneficiarios = sum(beneficiarios),
                  avg_org_count = mean(org_count),
                  percent_pobre = (sum(pob_pobre)) / (sum(poblacion_2019)),
                  percent_urbana = (sum(pob_urbana)) / (sum(poblacion_2019)),
                  percent_sin_agua_segura = (sum(pob_sin_agua_segura)) / (sum(poblacion_2019)),
                  percent_sin_saneamiento_mejorado = (sum(pob_sin_saneamiento_mejorado, na.rm = TRUE)) /
                    (sum(poblacion_2019, na.rm = TRUE))) %>% 
        mutate_if(is.numeric, format, digits = 3)) %>% 
  gather(key = var_name, value = value, 2:ncol(.)) %>% 
  spread_(key = names(.)[1], value = 'value') %>% 
  relocate(`641`, .after = `10`) %>% 
  mutate(`10` = as.numeric(`10`),
         `641` = as.numeric(`641`),
         `530` = as.numeric(`530`)) %>% 
  pander(big.mark = ",")



# currently evalled out and excluded. I have put all the code into the parr chunk. 
cen_ref <- read_excel("census data 20191122.xlsx", sheet = "data") %>% 
  clean_names() %>% 
  select(estado, pcode1, municipio, pcode2, parroquia, pcode3, 
         fo = field_office,
         poblacion_2019 = x_2019_poblacion_parroquial_total,
         hogares_2011 = numero_de_hogares, 
         ham_2019_ambitos_ge, 
         percent_pobre_2019 = ham_2019_xx_pobreza_env_por_parroquia, 
         pob_pobre = ham_2019_xx_poblacion_pobre_por_parroquia, 
         poblacion_total_2011,
         poblacion_infantil_menor_de_12_anos, poblacion_adolescentes_de_12_a_17_anos,
         poblacion_de_18_anos_y_mas, 
         percent_urbana = poblacion_urbana_percent, 
         area_km2, 
         densidad_ppl_km2 = densidad_poblacional_ppl_km2,
         matricula_2017_educacion_inicial, matricula_2017_educacion_primaria, 
         matricula_2017_educacion_media, razon_de_dependencia_total,
         razon_de_dependencia_de_menores_de_15_anos, 
         percent_sin_agua_segura = x_abast_agua2_percent_sin_agua_segura,
         percent_sin_saneamiento_mejorado = x_saneamiento_percent_sin_saneamiento_mejorado,
         percent_analfabeto = percent_poblacion_10_anos_y_mas_analfabeta,
         promedio_de_personas_por_vivienda,
         percent_hogares_jefatura_femenina = percent_de_hogares_con_jefatura_femenina,
         percent_sin_servicio_electrico = servicio_electrico_percent_no_tiene_servicio_electrico,
         ham_2019_x_violencia_envelope, ham_2019_x_mortalidad_y_salud_envelope, 
         ham_2019_x_pobreza_envelope, promedio_de_edad) %>% 
  mutate(estado    = rm_accent(str_to_upper(estado)), # just to make sure 
         municipio  = rm_accent(str_to_upper(municipio)),
         parroquia  = rm_accent(str_to_upper(parroquia))) %>% 
  mutate(pob_menor_de_18  = 
           (poblacion_infantil_menor_de_12_anos + poblacion_adolescentes_de_12_a_17_anos) /
           poblacion_total_2011 * poblacion_2019, 
         pob_18_y_mas    = poblacion_de_18_anos_y_mas / poblacion_total_2011 * poblacion_2019, 
         hogares_2019    = hogares_2011 * poblacion_2019 / poblacion_total_2011, 
         matricula_total = matricula_2017_educacion_inicial + matricula_2017_educacion_primaria + 
           matricula_2017_educacion_media) %>% 
  mutate_at(vars(percent_analfabeto, percent_sin_servicio_electrico, percent_sin_agua_segura,
                 percent_sin_saneamiento_mejorado,
                 percent_hogares_jefatura_femenina, percent_urbana,
                 razon_de_dependencia_total), ~(. / 100)) %>% 
  mutate(pob_analfabeto               =  percent_analfabeto * poblacion_2019,
         pob_sin_agua_segura          = percent_sin_agua_segura * poblacion_2019, 
         pob_sin_servicio_electrico   = percent_sin_servicio_electrico * poblacion_2019,
         pob_sin_saneamiento_mejorado = percent_sin_saneamiento_mejorado * poblacion_2019,
         pob_urbana                   = percent_urbana * poblacion_2019) %>% 
  select(-c(poblacion_infantil_menor_de_12_anos, poblacion_adolescentes_de_12_a_17_anos, 
            poblacion_de_18_anos_y_mas, hogares_2011, poblacion_total_2011))

# I don't really need this
<div class="fold s o">
  ```{r location-join-checking}
# there really are entirely duplicated rows, huh?
sum(adm_dirty$total_beneficiarios_alcanzados)

# just for reference, we are taking those out. 
adm_dirty %>% get_dupes()
```
</div>

# don't need this at the moment 
unique(act_ben[c("sector", "categoria")]) %>% arrange(sector)

# rpart.control is part of rpart
control = rpart.control(minsplit = 2, minbucket = 100))

estado_choices <- c("ALL" = "ALL", "DISTRITO CAPITAL" = "DISTRITO CAPITAL", 
                    "AMAZONAS" = "AMAZONAS", "ANZOATEGUI" = "ANZOATEGUI", "APURE" = "APURE",
                    "ARAGUA" = "ARAGUA", "BARINAS" = "BARINAS", "BOLIVAR" = "BOLIVAR", 
                    "CARABOBO" = "CARABOBO", "COJEDES" = "COJEDES", 
                    "DELTA AMACURO" = "DELTA AMACURO", "FALCON" = "FALCON", "GUARICO" = "GUARICO",
                    "LARA" = "LARA", "MERIDA" = "MERIDA", "MIRANDA" = "MIRANDA", 
                    "MONAGAS" = "MONAGAS", "NUEVA ESPARTA" = "NUEVA ESPARTA", 
                    "PORTUGUESA" = "PORTUGUESA", "SUCRE" = "SUCRE", "TACHIRA" = "TACHIRA", 
                    "TRUJILLO" = "TRUJILLO", "YARACUY" = "YARACUY", "ZULIA" = "ZULIA", 
                    "VARGAS" = "VARGAS")

# no longer needed
```{r location-cleaning}

locations <- read_excel("locations_20191111_1600.xlsx") %>%
  clean_names() %>% 
  rename(ubicacion = comunidad_o_centro) %>% 
  mutate(estado    = rm_accent(str_to_upper(estado)), # just to make sure 
         municipio = rm_accent(str_to_upper(municipio)),
         parroquia = rm_accent(str_to_upper(parroquia)),
         ubicacion = rm_accent(str_to_upper(ubicacion))) %>% 
  select(estado, pcode1, municipio, pcode2, parroquia, pcode3, ubicacion)

ven1 <- ven1 %>% 
  left_join(locations, by = "ubicacion") %>% 
  mutate(estado    = coalesce(estado.y, estado.x),
         pcode1    = coalesce(pcode1.y, pcode1.x),
         municipio = coalesce(municipio.y, municipio.x),
         pcode2    = coalesce(pcode2.y, pcode2.x),
         parroquia = coalesce(parroquia.y, parroquia.x),
         pcode3    = coalesce(pcode3.y, pcode3.x)) %>%
  select(-estado.x, -estado.y, -pcode1.x, -pcode1.y,
         -municipio.x, -municipio.y, -pcode2.x, -pcode2.y,
         -parroquia.x, -parroquia.y, -pcode3.x, -pcode3.y) %>% 
  distinct()

# next step is to resolve the admin locations
adm_dirty <- ven1 %>% 
  filter(is.na(estado) | is.na(pcode1) |
           is.na(municipio) | is.na(pcode2) |
           is.na(parroquia) | is.na(pcode3))

adm_clean <- ven1 %>% 
  filter(!is.na(estado) & !is.na(pcode1) &
           !is.na(municipio) & !is.na(pcode2) &
           !is.na(parroquia) & !is.na(pcode3)) 

# the distinct() actually clears out some duplicates in the data, 
#so it doesn't tally with the dataset from before 
# we'll get the rest of the duplicates in u_ben and act_ben
adm_dirty <- adm_dirty %>% 
  left_join(locations, by = "ubicacion") %>% 
  mutate(estado    = coalesce(estado.x, estado.y),
         pcode1    = coalesce(pcode1.x, pcode1.y),
         municipio = coalesce(municipio.x, municipio.y),
         pcode2    = coalesce(pcode2.x, pcode2.y),
         parroquia = coalesce(parroquia.x, parroquia.y),
         pcode3    = coalesce(pcode3.x, pcode3.y)) %>% 
  select(-estado.x, -estado.y, -pcode1.x, -pcode1.y,
         -municipio.x, -municipio.y, -pcode2.x, -pcode2.y,
         -parroquia.x, -parroquia.y, -pcode3.x, -pcode3.y) %>% 
  distinct() 

#overwriting ven1 to reduce environment objects
ven1 <-  bind_rows(adm_clean, adm_dirty)
```

# tree4 stuff
, cp = 0.021, minbucket = 120

# old state plot
parr0 %>% 
  group_by(estado) %>% 
  summarise(not_covered_pobre = sum(not_covered_pobre),
            coverage_percent = sum(beneficiarios) / sum(poblacion_2019) * 100) %>% 
  ggplot(aes(x = fct_reorder(estado, not_covered_pobre, .desc = TRUE), 
             y = not_covered_pobre, 
             fill = coverage_percent))+
  scale_fill_continuous(type = "gradient", trans = "reverse") +
  geom_col() + 
  geom_text(aes(label = scales::comma(not_covered_pobre, accuracy = 1)), 
            vjust = -0.25, hjust = 0.05, size = 2, angle = 60) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7)) +
  xlab("") + ylab("Poor persons not covered") +
  scale_y_continuous(labels = comma, limits = c(0, 2000000))

# renaming tree leaves

mutate(rule3a = recode(as.character(rule3),
                       `4` = "A", `11` = "C", `10` = "B", `6` = "D", `7` = "E"),
       rule1a = recode(as.character(rule1),
                       `3` = "W", `11` = "X", `10` = "Y", `4` = "Z"))

# plot not needed, but it's good reference

parr %>% 
  filter(beneficiarios != 0) %>% 
  ggplot(aes(x = densidad_ppl_km2, y = beneficiarios)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(trans = "log10", labels = comma) +
  scale_x_continuous(trans = "log10") +
  stat_regline_equation(label.y = 1e-01, aes(label = ..rr.label..))

# not a good solution
layout(legend = list(x = 0, 
                     y = "auto",
                     xanchor = "left", 
                     yanchor = "bottom",
                     orientation = "h")) %>% 
  
# ref for later  
  parrplot <- parr0 %>% 
  mutate_at(vars(pob_pobre, not_covered_pobre, org_count), ~(round(.))) %>% 
  mutate(percent_pobre = round(percent_pobre, digits = 2))%>% 
  ggplot(aes(x = not_covered_pobre, y = percent_pobre)) +
  geom_point(aes(alpha = estado)) +
  aes(size = not_covered_pobre, text = parroquia, colour = org_count) +
  scale_x_continuous(trans = "log10", labels = comma) +
  scale_size_continuous(range = c(0.3, 5)) +
  scale_alpha_discrete(range = c(0.75, 0.75)) +
  scale_colour_gradientn(
    colours = c("cornflowerblue", "tomato", "firebrick")) +
  xlab("Not covered poor") + ylab("Poverty incidence") +
  labs(size = "", alpha = "", colour = "estado") +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 5),
        legend.box = "vertical")

# discarded because there's a duplicate 
parr_plot <- parr0 %>% 
  mutate_at(vars(pob_pobre, not_covered_pobre, org_count), ~(round(.))) %>% 
  mutate(percent_pobre = round(percent_pobre, digits = 2))%>%
  ggplot(aes(x = not_covered_pobre, y = percent_pobre)) +
  geom_point(aes(alpha = estado)) +
  aes(size = not_covered_pobre, text = parroquia, colour = org_count) +
  scale_x_continuous(trans = "log10", labels = comma) + 
  scale_size_continuous(range = c(0.3, 5)) +
  scale_alpha_discrete(range = c(0.75, 0.75)) +
  scale_colour_gradientn(
    colours = c("cornflowerblue", "tomato", "firebrick")) +
  xlab("Not covered poor") + ylab("Poverty incidence") +
  labs(size = "") +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, "cm"))

ggplotly(parrplot, tooltip = c("y", "x", "text", "colour", "alpha")) %>%
  layout(showlegend = TRUE, legend = list(font = list(size = 7))) %>%
  config(displayModeBar = FALSE) %>% 
  hide_colorbar()

scroll down; \ndouble-click to toggle

# maybe act_ben shouldn't need to match unique beneficiaries...
# though perhaps it isn't such a bad idea -- how many beneficiaries have received
# support from more than one sector -- ok. it's settled. 
group_by(ubicacion, desagregacion, sector) %>% 
  slice(which.max(beneficiarios)) %>% 
  ungroup() %>% 
  
  compare_2nd_highest <- function(x) {
    #Sort the wages in descending order
    x1 <- sort(x, decreasing = TRUE)
    #Is the highest value more than double of second highest value
    x1[1] > (x1[2] * 2)
  }

# try this and see if it works
second_highest <- function(x) {
  x1 <- sort(x, decreasing = TRUE)
  x[2] * 1
}

educacion_ben = sum(Educacion, na.rm = TRUE),
nutricion_ben = sum(Nutricion, na.rm = TRUE),
proteccionGBV_ben = sum(Proteccion_GBV, na.rm = TRUE),
proteccionGeneral_ben = sum(Proteccion_General, na.rm = TRUE),
proteccionNNA_ben = sum(Proteccion_NNA, na.rm = TRUE),
salud_ben = sum(Salud, na.rm = TRUE),
seguridad_alimentaria_ben = sum(Seguridad_Alimentaria, na.rm = TRUE),
wash_ben = sum(WASH, na.rm = TRUE),

group_by(ubicacion, desagregacion, sector) %>% 
  slice(which.max(beneficiarios)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sector, values_from = beneficiarios) %>% 
  group_by(pcode3) %>% 
  # getting sector totals per pcode3
  
# Sector scatterplot is not longer needed
  
  ```{r}

sector_plot <- parr0 %>% 
  mutate_at(vars(pob_pobre, not_covered_pobre, org_count), ~(round(.))) %>% 
  mutate(percent_pobre = round(percent_pobre, digits = 2))%>% 
  ggplot(aes(x = not_covered_pobre, y = percent_pobre, 
             text = paste0(parroquia, ", ", estado), 
             size = not_covered_pobre)) +
  geom_point(aes(colour = sector_count), alpha = 0.75) +
  scale_x_continuous(trans = "log10", labels = comma) + 
  scale_size_continuous(range = c(0.3, 5)) +
  scale_colour_gradientn(
    colours = c("cornflowerblue", "tomato", "firebrick")) +
  xlab("Not covered poor") + ylab("Poverty incidence") +
  labs(colour = "Number of \nsectors", size = "") +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

ggplotly(sector_plot, tooltip = c("y", "x", "text", "colour")) %>% 
  layout(showlegend = TRUE, legend = list(font = list(size = 7))) %>%
  config(displayModeBar = FALSE)

```
_x-axis: number of poor persons; y-axis: poverty incidence; size: number of poor persons not covered; colour: number of sectors_
_mouse over for details_