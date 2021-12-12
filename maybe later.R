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

# options for act_ben joining for beneficiary frequencies 
mutate(sector2 = ifelse(categoriadeactividad == "Vacunacion", "Vacunacion", sector)) %>% 
  
  act_ben %>% 
  filter(categoriadeactividad != "Vacunacion") %>% 
  group_by(ubicacion, desagregacion, sector) %>% 
  slice(which.max(beneficiarios)) %>% 
  ungroup() %>% 
  group_by(ubicacion, sector, pcode3) %>% 
  pivot_wider(names_from = sector, values_from = beneficiarios) %>% 
  replace_na(list(Nutricion = 0, Educacion = 0, WASH = 0, Salud = 0,
                  Seguridad_Alimentaria = 0, Proteccion_NNA = 0,
                  Proteccion_General = 0, Proteccion_GBV = 0)) %>%
  group_by(pcode3) %>% 
  summarise(nutricion_ben = sum(Nutricion),
            proteccion_ben = sum(Proteccion_NNA + Proteccion_General + Proteccion_GBV),
            wash_ben = sum(WASH),
            salud_ben = sum(Salud),
            educacion_ben = sum(Educacion),
            seguridad_alimentaria_ben = sum(Seguridad_Alimentaria)) %>% 
  mutate(sector_count = rowSums(select(., -pcode3)!=0), 
         ben_freq = nutricion_ben + proteccion_ben + wash_ben + salud_ben + 
           educacion_ben + seguridad_alimentaria_ben,
         ben_max = pmax(nutricion_ben, proteccion_ben, wash_ben, salud_ben, 
                        educacion_ben, seguridad_alimentaria_ben),
         ms_max_ben = ifelse(ben_max >= ben_freq - ben_max, ben_freq - ben_max, ben_max))) %>%
  
  ```{r}
act_ben %>%
  # vaccination not filtered out  
  # filter(categoriadeactividad != "Vacunacion") %>% 
  group_by(ubicacion, desagregacion, sector) %>% 
  slice(which.max(beneficiarios)) %>% 
  ungroup() %>%
  group_by(ubicacion, sector, pcode3) %>% 
  pivot_wider(names_from = sector, values_from = beneficiarios) %>% 
  replace_na(list(Nutricion = 0, Educacion = 0, WASH = 0, Salud = 0,
                  Seguridad_Alimentaria = 0, Proteccion_NNA = 0,
                  Proteccion_General = 0, Proteccion_GBV = 0)) %>%
  group_by(pcode3, desagregacion) %>% 
  summarise(nutricion_ben   = sum(Nutricion),
            proteccion_ben  = sum(Proteccion_NNA + Proteccion_General + Proteccion_GBV),
            wash_ben        = sum(WASH),
            salud_ben       = sum(Salud),
            educacion_ben   = sum(Educacion),
            sa_ben          = sum(Seguridad_Alimentaria)) %>% 
  mutate(ben_freq   = nutricion_ben + proteccion_ben + wash_ben + salud_ben + 
           educacion_ben + sa_ben,
         ben_max    = pmax(nutricion_ben, proteccion_ben, wash_ben, salud_ben, 
                           educacion_ben, sa_ben),
         ms_ben_max = ifelse(ben_max >= ben_freq - ben_max, ben_freq - ben_max, ben_max)) %>% 
  group_by(pcode3) %>% 
  summarise(nutricion_ben  = sum(nutricion_ben),
            proteccion_ben = sum(proteccion_ben),
            wash_ben       = sum(wash_ben),
            salud_ben      = sum(salud_ben),
            educacion_ben  = sum(educacion_ben),
            sa_ben         = sum(sa_ben),
            ben_freq   = sum(ben_freq),
            ben_max    = sum(ben_max),
            ms_ben_max = sum(ms_ben_max)) %>% 
  mutate(sector_count = rowSums(select(., ends_with("_ben")) != 0)) 


ungroup() %>% 
  summarise(ms_ben_max = sum(ms_ben_max),
            ben_freq = sum(ben_freq),
            multisector_percent = sum(ms_ben_max) / sum(ben_freq))


# I think you have to summarise by pcode3 and disaggregation, 
# calculate the frequencies and ben_max,
# then only do you group by pcode3 and do the sector count
group_by(pcode3) %>% 
  summarise(nutricion_ben = sum(Nutricion),
            proteccion_ben = sum(Proteccion_NNA + Proteccion_General + Proteccion_GBV),
            wash_ben = sum(WASH),
            salud_ben = sum(Salud),
            educacion_ben = sum(Educacion),
            seguridad_alimentaria_ben = sum(Seguridad_Alimentaria)) %>% 
  mutate(sector_count = rowSums(select(., -pcode3)!=0), 
         ben_freq = nutricion_ben + proteccion_ben + wash_ben + salud_ben + 
           educacion_ben + seguridad_alimentaria_ben,
         ben_max = pmax(nutricion_ben, proteccion_ben, wash_ben, salud_ben, 
                        educacion_ben, seguridad_alimentaria_ben),
         ms_max_ben = ifelse(ben_max >= ben_freq - ben_max, ben_freq - ben_max, ben_max))


act_ben %>% 
  mutate(sector2 = ifelse(categoriadeactividad == "Vacunacion", "Vacunacion", sector)) %>% 
  group_by(sector2) %>% 
  summarise(beneficiarios = sum(beneficiarios))
```
# multisector barplot -- decided to do scatterplot instead

```{r}

parr0 %>% 
  group_by(estado) %>% 
  summarise(multisector_percent = sum(ms_ben_max) / sum(ben_freq)) %>% 
  arrange(desc(multisector_percent)) %>% 
  select(estado) %>% as.list(as.data.frame(t(.)))

state2_ord <- c("AMAZONAS", "BOLIVAR", "MIRANDA", "COJEDES", "ANZOATEGUI", "TACHIRA", "DELTA AMACURO",
                "ZULIA", "APURE", "DISTRITO CAPITAL", "GUARICO", "PORTUGUESA", "LARA", "MONAGAS", 
                "VARGAS","MERIDA", "SUCRE","YARACUY", "FALCON", "BARINAS", "ARAGUA", "CARABOBO", 
                "NUEVA ESPARTA", "TRUJILLO")

ms_state <- parr0 %>% 
  group_by(estado) %>% 
  summarise(multi_sector_ben = sum(ms_ben_max),
            one_sector_ben = sum(ben_freq) - sum(ms_ben_max),
            ben_freq = sum(ben_freq)) %>% 
  pivot_longer(multi_sector_ben:one_sector_ben, names_to = "ben_type", values_to = "ben_freqs") %>%
  mutate(multisector_percent = round(ben_freqs / ben_freq * 100), digits = 2) %>% 
  ggplot(aes(x = estado, y = multisector_percent, fill = ben_type)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(aes(x = estado, y = multisector_percent / 1.3, 
                label = comma(ben_freqs, accuracy = 1), 
                group = ben_type), size = 1.5) +
  scale_x_discrete(limits = state2_ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5),
        axis.title.y = element_text(size = 8)) +
  xlab("") + ylab("Percentage of multi-sector beneficiaries") + labs(fill = "") +
  scale_y_continuous(labels = comma)

ggplotly(ms_state) %>% 
  layout(legend = list(font = list(size = 6))) %>% 
  config(displayModeBar = FALSE)

```

# you were checking shapefile and census conflicts 

pcode3_shape %>% st_drop_geometry() %>% 
  select(pcode1,pcode2, pcode3) %>% distinct() %>% 
  write_csv(file = "shape_pcodes.csv")

parr %>% select(pcode1, pcode2, pcode3) %>% write_csv(file = "parr_pcodes.csv")

# does not work
style(hoverlabel = label, hoveron = "fill") 

# Dark2 -- we're using viridis instead
scale_fill_manual(values = c("#1B9E77", "#D95F02","#7570B3","#E7298A", "#FFFFFF"))

# shelving this -- i think we just need the parrmap of organisations
parrmap_nr <- parr %>% 
  left_join(parr0 %>% 
              select(pcode3, rule3), by = "pcode3") %>% 
  right_join(pcode3_shape, by = "pcode3") %>% 
  st_as_sf() %>% 
  mutate(not_reached = round(not_reached, digits = 0),
         tree_node = rule3, 
         med_nc = case_when(not_reached >= 5270 ~ "above_median",
                            not_reached < 5270 ~ "below_median")) %>% 
  mutate_at(vars(percent_pobre, percent_urbana), ~(round(., digits = 2))) %>% 
  ggplot() +
  geom_sf(size = 0.1, 
          aes(fill = tree_node,
              text = paste0(parroquia,",", "\n", 
                            municipio, ",", "\n",
                            estado, "\n", 
                            "not covered: ", not_reached, "\n",
                            "poverty incidence: ", percent_pobre, "\n",
                            "percent urban: ", percent_urbana, "\n",
                            "org present :", org_present),
              alpha = med_nc)) +
  theme_void() +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")) +
  scale_alpha_discrete(range = c(1, 0.7)) +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 9)) +
  guides(alpha = FALSE) +
  labs(fill = "Tree node",
       alpha = "",
       title = "") +
  ggtitle('Map of parrishes by decision tree node (colour) & poor persons not reached (alpha)')

ggplotly(parrmap_nr, tooltip = c("text", "fill")) %>%
  layout(title = list(text = paste0('Map of parrishes by decision tree node (colour) & poor persons not reached (alpha)',
                                    '<br>',
                                    '<sup>',
                                    'mouse over for details; drag and click to select and zoom; double-click legend select/deselect','</sup>')))

# to see matching combinations 
unique(act_ben[c("actividad_codigo", "actividad_desc")]) %>% 
  arrange(actividad_codigo)

# you have a better option for cluster combinations already
```{r}

cluster_comb <- parr %>% 
  filter(beneficiarios != 0) %>% 
  mutate(edu_nut = ifelse(educacion_ben > 0 & nutricion_ben > 0, 1, 0),
         edu_sal = ifelse(educacion_ben > 0 & salud_ben > 0, 1, 0),
         edu_wash = ifelse(educacion_ben > 0 & wash_ben > 0, 1, 0),
         edu_prot = ifelse(educacion_ben > 0 & proteccion_ben > 0, 1, 0), 
         nut_sal = ifelse(nutricion_ben > 0 & salud_ben > 0, 1, 0),
         nut_wash = ifelse(nutricion_ben > 0 & wash_ben > 0, 1, 0), 
         nut_prot = ifelse(nutricion_ben > 0 & proteccion_ben > 0, 1, 0),
         sal_wash = ifelse(wash_ben > 0 & salud_ben > 0, 1, 0),
         sal_prot = ifelse(proteccion_ben > 0 & salud_ben > 0, 1, 0),
         wash_prot = ifelse(wash_ben > 0 & proteccion_ben > 0, 1, 0),
         edu_only = ifelse(educacion_ben == ben_freq, 1, 0),
         nut_only = ifelse(nutricion_ben == ben_freq, 1, 0),
         sal_only = ifelse(salud_ben == ben_freq, 1, 0),
         wash_only = ifelse(wash_ben == ben_freq, 1, 0),
         prot_only = ifelse(proteccion_ben == ben_freq, 1, 0)) %>%
  summarise(across(c(edu_nut, edu_sal, edu_wash, edu_prot, nut_sal, nut_wash, nut_prot, sal_wash, 
                     sal_prot, wash_prot, edu_only, nut_only, sal_only, wash_only, prot_only), sum)) %>%
  pivot_longer(names_to = "combinations", values_to = "count", 1:ncol(.)) %>% 
  arrange(desc(count))

kable(list(cluster_comb %>% head(8), cluster_comb %>% tail(7)), 
      caption = "Cluster combinations by count of parrishes", 
      "html", table.attr = "style = 'width:50%;'") %>% 
  kable_styling(font_size = 15, bootstrap_options = "hover")

```

edu_all_freq = ifelse(educacion_ben > 0, ben_freq, 0),
nut_all_freq = ifelse(nutricion_ben > 0, ben_freq, 0),
sal_all_freq = ifelse(salud_ben > 0, ben_freq, 0),
wash_all_freq = ifelse(wash_ben > 0, ben_freq, 0),
prot_all_freq = ifelse(proteccion_ben > 0, ben_freq, 0)

# maybe not the best idea to have one giant table
# summary table of cluster combinations 
# rbind so they are all together in the same pander table
rbind(
  clust_com %>% 
    filter(cluster == "educacion" & clust_freq != 0) %>% 
    group_by(combination) %>% 
    summarise(parrishes = n(),
              clust_freq = sum(clust_freq),
              pair_freq = sum(pair_freq)) %>% 
    mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
           `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_freq)) / pair_freq * 100, digits = 1),
           `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`),
           `%_parrishes` = round(parrishes / sum(parrishes) * 100, digits =1)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>%
    relocate(`%_parrishes`, .after = parrishes) %>% 
    rbind(NA), 
  
  clust_com %>% 
    filter(cluster == "nutricion" & clust_freq != 0) %>% 
    group_by(combination) %>% 
    summarise(parrishes = n(),
              clust_freq = sum(clust_freq),
              pair_freq = sum(pair_freq)) %>% 
    mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
           `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_freq)) / pair_freq * 100, digits = 1),
           `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`),
           `%_parrishes` = round(parrishes / sum(parrishes) * 100, digits =1)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>%
    relocate(`%_parrishes`, .after = parrishes) %>% 
    rbind(NA),
  
  clust_com %>% 
    filter(cluster == "salud" & clust_freq != 0) %>% 
    group_by(combination) %>% 
    summarise(parrishes = n(),
              clust_freq = sum(clust_freq),
              pair_freq = sum(pair_freq)) %>% 
    mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
           `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_freq)) / pair_freq * 100, digits = 1),
           `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`),
           `%_parrishes` = round(parrishes / sum(parrishes) * 100, digits =1)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>%
    relocate(`%_parrishes`, .after = parrishes) %>% 
    rbind(NA),
  
  clust_com %>% 
    filter(cluster == "wash" & clust_freq != 0) %>% 
    group_by(combination) %>% 
    summarise(parrishes = n(),
              clust_freq = sum(clust_freq),
              pair_freq = sum(pair_freq)) %>% 
    mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
           `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_freq)) / pair_freq * 100, digits = 1),
           `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`),
           `%_parrishes` = round(parrishes / sum(parrishes) * 100, digits =1)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>%
    relocate(`%_parrishes`, .after = parrishes) %>% 
    rbind(NA),
  
  clust_com %>% 
    filter(cluster == "proteccion" & clust_freq != 0) %>% 
    group_by(combination) %>% 
    summarise(parrishes = n(),
              clust_freq = sum(clust_freq),
              pair_freq = sum(pair_freq)) %>% 
    mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
           `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_freq)) / pair_freq * 100, digits = 1),
           `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`),
           `%_parrishes` = round(parrishes / sum(parrishes) * 100, digits =1)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>%
    relocate(`%_parrishes`, .after = parrishes)
  
) %>% 
  
  pander(caption = "Cluster combinations", big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right", "right"), missing = "")

# the start of many, many, many failed attempts at category combinations

# this particular example collapses the values into a list vector -- very useful, 
# but I don't really know how to use it yet
```{r}
cat_list <- act_ben %>% 
  filter(categoria != "VACUNACION") %>% 
  select(pcode3, categoria) %>% 
  summarise_all(list) %>% 
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "vector")

expand(cat_list, 
       nesting(categoria, pcode3),
       nesting(var2 = var, pcode3_2 = pcode3))


act_ben %>%
  filter(categoria != "VACUNACION") %>% 
  select(pcode3, categoria) %>% 
  mutate(pcode3 = parse_number(pcode3)) %>% 
  pivot_wider(names_from = categoria, values_from = pcode3) %>%
  summarise_all(list) %>% 
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "vector") %>% 
  expand(nesting(var, vector),
         nesting(var2 = var, vector2 = vector)) %>% 
  filter(var != var2) %>% 
  arrange(var, var2) %>% 
  mutate(vars = paste0(var, ".", var2)) %>% 
  select(contains("var"), everything())
```

# it's not that this one doesn't work, but the way the combinations work is not good 
# this is the dataset that made me realise that pairwise calculations were much better

```{r}
cat <- act_ben %>% 
  filter(categoria != "VACUNACION" & categoria != "OTRO") %>% 
  group_by(pcode3, categoria) %>% 
  summarise(beneficiarios = sum(beneficiarios)) %>% 
  pivot_wider(names_from = categoria, values_from = beneficiarios) %>%
  replace(is.na(.), 0) %>% 
  group_by(pcode3) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  mutate(PREVENCION_DESNUTRICION_AGUDA = ifelse(PREVENCION_DESNUTRICION_AGUDA != 0,
                                                "PREVENCION_DESNUTRICION_AGUDA", NA_character_),
         CAPACITACIONES_PROTECCION = ifelse(CAPACITACIONES_PROTECCION != 0,
                                            "CAPACITACIONES_PROTECCION",NA_character_),
         TRATAMIENTO_DESNUTRICION_AGUDA = ifelse(TRATAMIENTO_DESNUTRICION_AGUDA != 0,
                                                 "TRATAMIENTO_DESNUTRICION_AGUDA", NA_character_),
         ASISTENCIA_ALIMENTARIA = ifelse(ASISTENCIA_ALIMENTARIA != 0, 
                                         "ASISTENCIA_ALIMENTARIA", NA_character_),
         RESILENCIA_EDUCACION = ifelse(RESILENCIA_EDUCACION != 0, "RESILENCIA_EDUCACION", NA_character_),
         FORTALECIMIENTO_INSTITUCIONAL = ifelse(FORTALECIMIENTO_INSTITUCIONAL != 0,
                                                "FORTALECIMIENTO_INSTITUCIONAL", NA_character_),
         WASH_EN_EDUCACION = ifelse(WASH_EN_EDUCACION != 0, 
                                    "WASH_EN_EDUCACION", NA_character_), 
         SEGURIDAD_ALIM_INSTITUCIONAL = ifelse(SEGURIDAD_ALIM_INSTITUCIONAL != 0,
                                               "SEGURIDAD_ALIM_INSTITUCIONAL", NA_character_),
         VIH = ifelse(VIH != 0, "VIH", NA_character_),
         SALUD_POBLACIONAL = ifelse(SALUD_POBLACIONAL != 0, 
                                    "SALUD_POBLACIONAL", NA_character_),
         PROVISION_DE_SERVICIO = ifelse(PROVISION_DE_SERVICIO != 0,
                                        "PROVISION_DE_SERVICIO_PG", NA_character_),
         INCIDENCIA_CON_AUTORIDADES = ifelse(INCIDENCIA_CON_AUTORIDADES != 0,
                                             "INCIDENCIA_CON_AUTORIDADES_PG", NA_character_),
         COMUNIDADES_SALUD = ifelse(COMUNIDADES_SALUD != 0, 
                                    "COMUNIDADES_SALUD", NA_character_),
         RED_INTEGRADA_SALUD = ifelse(RED_INTEGRADA_SALUD != 0, 
                                      "RED_INTEGRADA_SALUD", NA_character_),
         INFORMACION_RIESGOS = ifelse(INFORMACION_RIESGOS != 0, 
                                      "INFORMACION_RIESGOS_PG", NA_character_),
         PROVISION_DE_SERVICIOS = ifelse(PROVISION_DE_SERVICIOS != 0,
                                         "PROVISION_DE_SERVICIOS_PN", NA_character_),
         PROMOCION_HIGIENE = ifelse(PROMOCION_HIGIENE != 0, 
                                    "PROMOCION_HIGIENE", NA_character_),
         AGUA_EN_COMUNIDADES = ifelse(AGUA_EN_COMUNIDADES != 0, "AGUA_EN_COMUNIDADES", NA_character_),
         WASH_EN_SALUD_NUTRICION = ifelse(WASH_EN_SALUD_NUTRICION != 0, 
                                          "WASH_EN_SALUD_NUTRICION", NA_character_),
         FORTALECIMIENTO_CAPACIDAD_EDUCACION = ifelse(FORTALECIMIENTO_CAPACIDAD_EDUCACION != 0,
                                                      "FORTALECIMIENTO_CAPACIDAD_EDUCACION", NA_character_),
         ACCESO_PERMANENCIA_ESCOLAR = ifelse(ACCESO_PERMANENCIA_ESCOLAR != 0, 
                                             "ACCESO_PERMANENCIA_ESCOLAR", NA_character_),
         SALUD_MATERNA_INFANTIL = ifelse(SALUD_MATERNA_INFANTIL != 0, 
                                         "SALUD_MATERNA_INFANTIL", NA_character_),
         SUMINISTROS_MEDICAMENTOS_BASICOS = ifelse(SUMINISTROS_MEDICAMENTOS_BASICOS != 0, 
                                                   "SUMINISTROS_MEDICAMENTOS_BASICOS", NA_character_),
         SANEAMIENTO = ifelse(SANEAMIENTO != 0, 
                              "SANEAMIENTO", NA_character_),
         RED_DE_PROTECCION = ifelse(RED_DE_PROTECCION != 0, "RED_DE_PROTECCION", NA_character_),
         CAPACITACIONES_NUTRICION = ifelse(CAPACITACIONES_NUTRICION != 0, 
                                           "CAPACITACIONES_NUTRICION", NA_character_),
         ESTABLECIMIENTOS_SALUD = ifelse(ESTABLECIMIENTOS_SALUD != 0, 
                                         "ESTABLECIMIENTOS_SALUD", NA_character_))

names(cat) <- make.names(names(cat))

cat <- cat %>% 
  group_by(.dots = names(cat)) %>% 
  summarise(count = n())

cat$categories <- apply(cat[, c("ACCESO_PERMANENCIA_ESCOLAR", "FORTALECIMIENTO_CAPACIDAD_EDUCACION", 
                                "PREVENCION_DESNUTRICION_AGUDA", "PROMOCION_HIGIENE", "PROVISION_DE_SERVICIOS",
                                "RESILENCIA_EDUCACION", "WASH_EN_SALUD_NUTRICION", "AGUA_EN_COMUNIDADES",
                                "ASISTENCIA_ALIMENTARIA", "SEGURIDAD_ALIM_INSTITUCIONAL",
                                "TRATAMIENTO_DESNUTRICION_AGUDA", "FORTALECIMIENTO_INSTITUCIONAL",
                                "CAPACITACIONES_PROTECCION", "SALUD_MATERNA_INFANTIL", "VIH",
                                "COMUNIDADES_SALUD", "RED_INTEGRADA_SALUD",
                                "INFORMACION_RIESGOS", "PROVISION_DE_SERVICIO", "SALUD_POBLACIONAL",
                                "WASH_EN_EDUCACION", "CAPACITACIONES_NUTRICION", "RED_DE_PROTECCION",
                                "SUMINISTROS_MEDICAMENTOS_BASICOS", "ESTABLECIMIENTOS_SALUD",
                                "SANEAMIENTO", "INCIDENCIA_CON_AUTORIDADES")], 1,
                        function(i){paste(na.omit(i), collapse = ", ")})

cat %>% ungroup() %>% select(categories, count) %>% 
  arrange(desc(count))


```
# doesn't work 

```{r}
cat <- act_ben %>% 
  filter(categoria != "VACUNACION" & categoria != "OTRO") %>% 
  group_by(pcode3, categoria) %>% 
  summarise(beneficiarios = sum(beneficiarios)) %>% 
  mutate(beneficiarios2 = beneficiarios) %>% 
  pivot_wider(names_from = categoria, values_from = beneficiarios2) %>%
  replace(is.na(.), 0) %>% 
  group_by(pcode3) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  mutate(PREVENCION_DESNUTRICION_AGUDA = ifelse(PREVENCION_DESNUTRICION_AGUDA != 0,
                                                "PREVENCION_DESNUTRICION_AGUDA", NA_character_),
         CAPACITACIONES_PROTECCION = ifelse(CAPACITACIONES_PROTECCION != 0,
                                            "CAPACITACIONES_PROTECCION",NA_character_),
         TRATAMIENTO_DESNUTRICION_AGUDA = ifelse(TRATAMIENTO_DESNUTRICION_AGUDA != 0,
                                                 "TRATAMIENTO_DESNUTRICION_AGUDA", NA_character_),
         ASISTENCIA_ALIMENTARIA = ifelse(ASISTENCIA_ALIMENTARIA != 0, 
                                         "ASISTENCIA_ALIMENTARIA", NA_character_),
         RESILENCIA_EDUCACION = ifelse(RESILENCIA_EDUCACION != 0, "RESILENCIA_EDUCACION", NA_character_),
         FORTALECIMIENTO_INSTITUCIONAL = ifelse(FORTALECIMIENTO_INSTITUCIONAL != 0,
                                                "FORTALECIMIENTO_INSTITUCIONAL", NA_character_),
         WASH_EN_EDUCACION = ifelse(WASH_EN_EDUCACION != 0, 
                                    "WASH_EN_EDUCACION", NA_character_), 
         SEGURIDAD_ALIM_INSTITUCIONAL = ifelse(SEGURIDAD_ALIM_INSTITUCIONAL != 0,
                                               "SEGURIDAD_ALIM_INSTITUCIONAL", NA_character_),
         VIH = ifelse(VIH != 0, "VIH", NA_character_),
         SALUD_POBLACIONAL = ifelse(SALUD_POBLACIONAL != 0, 
                                    "SALUD_POBLACIONAL", NA_character_),
         PROVISION_DE_SERVICIO = ifelse(PROVISION_DE_SERVICIO != 0,
                                        "PROVISION_DE_SERVICIO_PG", NA_character_),
         INCIDENCIA_CON_AUTORIDADES = ifelse(INCIDENCIA_CON_AUTORIDADES != 0,
                                             "INCIDENCIA_CON_AUTORIDADES_PG", NA_character_),
         COMUNIDADES_SALUD = ifelse(COMUNIDADES_SALUD != 0, 
                                    "COMUNIDADES_SALUD", NA_character_),
         RED_INTEGRADA_SALUD = ifelse(RED_INTEGRADA_SALUD != 0, 
                                      "RED_INTEGRADA_SALUD", NA_character_),
         INFORMACION_RIESGOS = ifelse(INFORMACION_RIESGOS != 0, 
                                      "INFORMACION_RIESGOS_PG", NA_character_),
         PROVISION_DE_SERVICIOS = ifelse(PROVISION_DE_SERVICIOS != 0,
                                         "PROVISION_DE_SERVICIOS_PN", NA_character_),
         PROMOCION_HIGIENE = ifelse(PROMOCION_HIGIENE != 0, 
                                    "PROMOCION_HIGIENE", NA_character_),
         AGUA_EN_COMUNIDADES = ifelse(AGUA_EN_COMUNIDADES != 0, "AGUA_EN_COMUNIDADES", NA_character_),
         WASH_EN_SALUD_NUTRICION = ifelse(WASH_EN_SALUD_NUTRICION != 0, 
                                          "WASH_EN_SALUD_NUTRICION", NA_character_),
         FORTALECIMIENTO_CAPACIDAD_EDUCACION = ifelse(FORTALECIMIENTO_CAPACIDAD_EDUCACION != 0,
                                                      "FORTALECIMIENTO_CAPACIDAD_EDUCACION", NA_character_),
         ACCESO_PERMANENCIA_ESCOLAR = ifelse(ACCESO_PERMANENCIA_ESCOLAR != 0, 
                                             "ACCESO_PERMANENCIA_ESCOLAR", NA_character_),
         SALUD_MATERNA_INFANTIL = ifelse(SALUD_MATERNA_INFANTIL != 0, 
                                         "SALUD_MATERNA_INFANTIL", NA_character_),
         SUMINISTROS_MEDICAMENTOS_BASICOS = ifelse(SUMINISTROS_MEDICAMENTOS_BASICOS != 0, 
                                                   "SUMINISTROS_MEDICAMENTOS_BASICOS", NA_character_),
         SANEAMIENTO = ifelse(SANEAMIENTO != 0, 
                              "SANEAMIENTO", NA_character_),
         RED_DE_PROTECCION = ifelse(RED_DE_PROTECCION != 0, "RED_DE_PROTECCION", NA_character_),
         CAPACITACIONES_NUTRICION = ifelse(CAPACITACIONES_NUTRICION != 0, 
                                           "CAPACITACIONES_NUTRICION", NA_character_),
         ESTABLECIMIENTOS_SALUD = ifelse(ESTABLECIMIENTOS_SALUD != 0, 
                                         "ESTABLECIMIENTOS_SALUD", NA_character_)) %>% 
  select(-c(beneficiarios))

plyr::count(cat[,-1]) %>% arrange(desc(freq))

plyr::count(cat, vars = c("ACCESO_PERMANENCIA_ESCOLAR", "FORTALECIMIENTO_CAPACIDAD_EDUCACION", 
                          "PREVENCION_DESNUTRICION_AGUDA", "PROMOCION_HIGIENE", "PROVISION_DE_SERVICIOS",
                          "RESILENCIA_EDUCACION", "WASH_EN_SALUD_NUTRICION", "AGUA_EN_COMUNIDADES",
                          "ASISTENCIA_ALIMENTARIA", "SEGURIDAD_ALIM_INSTITUCIONAL",
                          "TRATAMIENTO_DESNUTRICION_AGUDA", "FORTALECIMIENTO_INSTITUCIONAL",
                          "CAPACITACIONES_PROTECCION", "SALUD_MATERNA_INFANTIL", "VIH",
                          "COMUNIDADES_SALUD", "RED_INTEGRADA_SALUD",
                          "INFORMACION_RIESGOS", "PROVISION_DE_SERVICIO", "SALUD_POBLACIONAL",
                          "WASH_EN_EDUCACION", "CAPACITACIONES_NUTRICION", "RED_DE_PROTECCION",
                          "SUMINISTROS_MEDICAMENTOS_BASICOS", "ESTABLECIMIENTOS_SALUD",
                          "SANEAMIENTO", "INCIDENCIA_CON_AUTORIDADES"))

```

As a note, the number of organisations present in a parrish is highly correlated with the number of sectors present there (a correlation coefficient of `r round(cor(parr0$org_count, parr0$sector_count, method = c("pearson")), digits = 3)`). A scatterplot would be repetitive and very similar to the one in section 3b and not deepen our understanding of the actual coverage.

# the scatterplot will be at parrish level

```{r ms-state-scatter-PLOT, fig.height=5}

ms_scatter <- parr0 %>% 
  group_by(estado) %>% 
  summarise(multi_sector_ben = sum(ms_ben_max),
            one_sector_ben = round(sum(ben_freq) - sum(ms_ben_max), digits = 0),
            ben_freq = round(sum(ben_freq), digits = 0)) %>% 
  mutate(multi_sector_percent = round(multi_sector_ben / ben_freq * 100, digits = 1),
         one_sector_percent = round(one_sector_ben / ben_freq * 100, digits = 1)) %>% 
  ggplot(aes(x = ben_freq, 
             y = multi_sector_percent)) +
  geom_point(aes(size = one_sector_ben), 
             alpha = 0.8, colour = "coral") +
  geom_text(aes(label = estado), size = 1.5) +
  scale_x_continuous(trans = "log10", labels = comma) +
  scale_size_continuous(range = c(0.3, 10)) +
  xlab("Beneficiary frequencies") + ylab("Percentage received multi-sector support") +
  labs(title = "Scatterplot of states by beneficiary frequencies and multi-sector coverage") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 8.5))

ggplotly(ms_scatter, tooltip = c("x", "y", "text", "size")) %>% 
  layout(legend = list(font = list(size = 6))) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0(
    "Scatterplot of states by beneficiary frequencies and multi-sector coverage",
    "<br>",
    "<sup>",
    "size: beneficiaries who only received support from one sector; mouse over for details","</sup>")))


```

pander(caption = "Most common inter-cluster activity category combinations", style = "rmarkdown",  
       justify = c("left", "right"))

pander(caption = "Cluster combinations, sorted by pair_sum", big.mark = ",", missing = "",
       justify = c("left", "right", "right", "right", "right", "right"), style = "rmarkdown")

pander(big.mark = ",", caption = "Summary table of the terminal nodes of tree3")

```{r parr0-state-PLOT, fig.height=5}
# ref for printing state_ord
# parr0 %>% 
#   group_by(estado) %>% 
#   summarise(not_reached = sum(not_reached)) %>% 
#   arrange(desc(not_reached)) %>% 
#   select(estado) %>% as.list(as.data.frame(t(.))) %>% print()

state_ord <- c("ZULIA", "LARA", "CARABOBO", "MIRANDA", "ANZOATEGUI", "ARAGUA", "BOLIVAR",
               "PORTUGUESA", "SUCRE", "GUARICO", "FALCON", "MONAGAS", "BARINAS", "MERIDA",
               "TACHIRA", "TRUJILLO", "YARACUY", "APURE", "DISTRITO CAPITAL", "NUEVA ESPARTA",
               "COJEDES", "VARGAS", "DELTA AMACURO", "AMAZONAS")

stack_text <- parr0 %>% 
  group_by(estado) %>% 
  summarise(beneficiarios = sum(beneficiarios),
            total = sum(pob_pobre)) %>% 
  mutate(percent_reached = round(beneficiarios / total * 100, digits = 1)) %>% 
  arrange(desc(total)) 

state_stack <- parr0 %>% 
  select(estado, beneficiarios, not_reached) %>% 
  group_by(estado) %>%
  summarise(beneficiarios = round(sum(beneficiarios), digits = 0), 
            not_reached = round(sum(not_reached), digits = 0)) %>% 
  pivot_longer(c(beneficiarios, not_reached),
               names_to = "pob_type", values_to = "total") %>% 
  
  ggplot(aes(x = estado, y = total)) +
  geom_col(aes(fill = pob_type)) +
  scale_y_continuous(label = comma) +
  scale_x_discrete(limits = state_ord) +
  scale_fill_manual(values = c("coral", "royalblue")) +
  geom_text(data = stack_text, aes(y = 20000,
                                   label = percent_reached), 
            size = 2.5, fontface = "bold", colour = "white") +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 6.5),
        axis.text.y  = element_text(size = 5),
        axis.title.y = element_text(size = 9),
        plot.title   = element_text(size = 11)) +
  xlab("") + ylab("Number of poor persons") + 
  labs(fill = "",
       title = "Barplot of poor persons by state by reached/not reached")

ggplotly(state_stack) %>% 
  layout(legend = list(font = list(size = 7))) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0(
    "Barplot of poor persons by state by reached/not reached",
    "<br>",
    "<sup>",
    "mouse over for details","</sup>")))

```