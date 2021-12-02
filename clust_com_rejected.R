

```{r cc-edu-TABLE}
clust_com %>% 
  filter(cluster == "educacion" & clust_freq != 0) %>% 
  group_by(combination) %>% 
  summarise(parrishes = n(),
            clust_freq = sum(clust_freq),
            pair_sum = sum(pair_freq)) %>% 
  mutate(ms_max  = round(pmin(clust_freq, abs(clust_freq - pair_sum)), digits = 1),
         `%_ms_max` = round(ms_max / pair_sum * 100, digits = 1),
         `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`)) %>% 
  
  pander(caption = "Education cluster combinations", big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right"))
```


The most common pairing for Education was with the Nutrition cluster -- they coincide in `r clust_com %>% filter(combination == "edu_nut" & clust_freq != 0) %>% nrow()` parrishes. However, there are no concrete programmatic links programmatically between the two, with Nutrition focusing on persons older or younger than the majority of Education's beneficiaries. However, Protection and WASH both have explicit programmatic links (in the logframe) with Education and `r clust_com %>% filter(combination == "edu_prot" & clust_freq != 0) %>% nrow()` and `r clust_com %>% filter(combination == "edu_wash" & clust_freq != 0) %>% nrow()` parrishes respectively. A fruitful avenue of investigation would be how many beneficiaries of Education also benefitted from Protection interventions and how close it is to the 37.9% theoretical maximum. 


<br>

```{r cc-nutrition-TABLE}
clust_com %>% 
  filter(cluster == "nutricion" & clust_freq != 0) %>% 
  group_by(combination) %>% 
  summarise(parrishes = n(),
            clust_freq = sum(clust_freq),
            pair_sum = sum(pair_freq)) %>% 
  mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
         `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_sum)) / pair_sum * 100, digits = 1),
         `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>% 

  pander(caption = "Nutrition cluster combinations", big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right"))
```

Nutrition operates alone in 282 parrishes out of the `r act_ben %>% filter(sector == "Nutricion") %>% distinct(pcode3) %>% nrow()` that it operates in, this is the most out of any of the other clusters -- it is necessary to evaluate the extent to which other clusters can make use of the footholds established by Nutrition. In this light, it seems that its high level of coincidence with Education was due more to its wide operational presence rather than due to concerted coordination. The highest possibility of overlap is with Health, due to 1) their respective beneficiary frequencies not differing as much as with the other clusters and 2) the numerous programmatic links between the two. However, has the lowest number of beneficiaries in parrishes wherre health is also present. 

<br>

```{r cc-health-TABLE}
clust_com %>% 
  filter(cluster == "salud" & clust_freq != 0) %>% 
  group_by(combination) %>% 
  summarise(parrishes = n(),
            clust_freq = sum(clust_freq),
            pair_sum = sum(pair_freq)) %>% 
  mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
         `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_sum)) / pair_sum * 100, digits = 1),
         `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>% 

  pander(caption = "Health cluster combinations, excluding vaccinations", 
         big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right"))
```

Without vaccination interventions, health has the smallest footprint of any of the clusters, covering `r act_ben %>% filter(sector == "Salud" & categoria != "VACUNACION") %>% distinct(pcode3) %>% nrow()` parrishes and `r act_ben %>% filter(sector == "Salud" & categoria != "VACUNACION") %>% {sum(.$beneficiarios)}` beneficiary frequencies. It coincides the most with Nutrition and Protection and has numerous programmatic opportunities to coordinate with both at the intervention level with its focus on obstetric, antenatal and neonatal care as well as with HIV diagnosis and treatment. 

<br> 

```{r cc-wash-TABLE}
clust_com %>% 
  filter(cluster == "wash" & clust_freq != 0) %>% 
  group_by(combination) %>% 
  summarise(parrishes = n(),
            clust_freq = sum(clust_freq),
            pair_sum = sum(pair_freq)) %>% 
  mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
         `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_sum)) / pair_sum * 100, digits = 1),
         `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`)) %>% 
    relocate(`%_subtotal`, .after = clust_freq) %>% 

  pander(caption = "WASH cluster combinations", big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right"))
```

Almost none of WASH's beneficiary frequencies occurred in parrishes where no other clusters were present. Its great reach and blanket coverage (especially water supply and other community-level activities) mean that other clusters operating in the same areas as WASH are "guaranteed" to reach beneficiaries with multi-sector programming -- the challenges being 1. the intentionality of the multi-sector coverage and 2. matching the scale of WASH activities. WASH has excellent programmatic overlap with all other clusters. 

<br>
  
  ```{r cc-protection-TABLE}
clust_com %>% 
  filter(cluster == "proteccion" & clust_freq != 0) %>% 
  group_by(combination) %>% 
  summarise(parrishes = n(),
            clust_freq = sum(clust_freq),
            pair_sum = sum(pair_freq)) %>% 
  mutate(`%_subtotal` = round(clust_freq / sum(clust_freq) * 100, digits = 1),
         `%_ms_max` = round(pmin(clust_freq, abs(clust_freq - pair_sum)) / pair_sum * 100, digits = 1),
         `%_ms_max` = ifelse(is.infinite(`%_ms_max`), 0, `%_ms_max`)) %>% 
  relocate(`%_subtotal`, .after = clust_freq) %>% 
  
  pander(caption = "Protection cluster combinations", big.mark = ",", style = "rmarkdown", 
         justify = c("left", "right", "right", "right", "right", "right"))
```

Like WASH and health, Protection has very limited beneficiary frequencies in parrishes where it operates alone. Protection coincides the most with Nutrition -- this should serve as an impulse for the creation of referral pathways between the two since both carry out screening activities; additionally, both manage some form of beneficiary-level database. This cluster has the most explicit progammatic links to Education. 

```{r clust-com-REF}
# creation of reference df for the cluster combinations 
clust_com <- parr %>% 
  filter(ben_freq != 0) %>% 
  select(pcode3, ben_freq, educacion_ben, nutricion_ben, salud_ben, wash_ben, proteccion_ben) %>%
  # mutate a new column for each combination of sectors -- if edu is the first cluster in the combination, 
  # only education beneficiaries will be used to fill values in the column 
  mutate(edu_alone = ifelse(educacion_ben == ben_freq, educacion_ben, 0),
         edu_nut = ifelse(educacion_ben > 0 & nutricion_ben > 0, educacion_ben, 0),
         edu_sal = ifelse(educacion_ben > 0 & salud_ben > 0, educacion_ben, 0),
         edu_wash = ifelse(educacion_ben > 0 & wash_ben > 0, educacion_ben, 0),
         edu_prot = ifelse(educacion_ben > 0 & proteccion_ben > 0, educacion_ben, 0),
         nut_alone = ifelse(nutricion_ben == ben_freq, nutricion_ben, 0),
         nut_edu = ifelse(educacion_ben > 0 & nutricion_ben > 0, nutricion_ben, 0),
         nut_sal = ifelse(nutricion_ben > 0 & salud_ben > 0, nutricion_ben, 0), 
         nut_wash = ifelse(nutricion_ben > 0 & wash_ben > 0, nutricion_ben, 0), 
         nut_prot = ifelse(nutricion_ben > 0 & proteccion_ben > 0, nutricion_ben, 0),
         sal_alone = ifelse(salud_ben == ben_freq, salud_ben, 0),
         sal_edu = ifelse(educacion_ben > 0 & salud_ben > 0, salud_ben, 0),
         sal_nut = ifelse(nutricion_ben > 0 & salud_ben > 0, salud_ben, 0),
         sal_wash = ifelse(salud_ben > 0 & wash_ben > 0, salud_ben, 0),
         sal_prot = ifelse(proteccion_ben > 0 & salud_ben > 0, salud_ben, 0),
         wash_alone = ifelse(wash_ben == ben_freq, wash_ben, 0),
         wash_edu = ifelse(educacion_ben > 0 & wash_ben > 0, wash_ben, 0),
         wash_nut = ifelse(nutricion_ben > 0 & wash_ben > 0, wash_ben, 0),
         wash_sal = ifelse(salud_ben > 0 & wash_ben > 0, wash_ben, 0),
         wash_prot = ifelse(wash_ben > 0 & proteccion_ben > 0, wash_ben, 0),
         prot_alone = ifelse(proteccion_ben == ben_freq, proteccion_ben, 0), 
         prot_edu = ifelse(educacion_ben > 0 & proteccion_ben > 0, proteccion_ben, 0),
         prot_nut = ifelse(nutricion_ben > 0 & proteccion_ben > 0, proteccion_ben, 0),
         prot_sal = ifelse(proteccion_ben > 0 & salud_ben > 0, proteccion_ben, 0),
         prot_wash = ifelse(wash_ben > 0 & proteccion_ben > 0, proteccion_ben, 0)) %>%
  # pivot_longer to the clust_freq column 
  pivot_longer(names_to = "combination", values_to = "clust_freq", 8:32) %>%
  # mutating new cluster column for filtering later
  mutate(cluster = case_when(str_detect(combination, "^edu") ~ "educacion",
                             str_detect(combination, "^nut") ~ "nutricion",
                             str_detect(combination, "^sal") ~ "salud",
                             str_detect(combination, "^wash") ~ "wash",
                             str_detect(combination, "^prot") ~ "proteccion")) %>%
  # calculating the sum of frequencies in each pair
  mutate(pair_freq = 
           case_when(
             str_detect(combination, "edu_nut|nut_edu") ~ educacion_ben + nutricion_ben,
             str_detect(combination, "edu_sal|sal_edu") ~ educacion_ben + salud_ben,
             str_detect(combination, "edu_wash|wash_edu") ~ educacion_ben + wash_ben,
             str_detect(combination, "edu_prot|prot_edu") ~ educacion_ben + proteccion_ben,
             str_detect(combination, "nut_sal|sal_nut") ~ nutricion_ben + salud_ben,
             str_detect(combination, "nut_wash|wash_nut") ~ nutricion_ben + wash_ben,
             str_detect(combination, "nut_prot|prot_nut") ~ nutricion_ben + proteccion_ben,
             str_detect(combination, "sal_wash|wash_sal") ~ salud_ben + wash_ben,
             str_detect(combination, "sal_prot|prot_sal") ~ salud_ben + proteccion_ben,
             str_detect(combination, "prot_wash|wash_prot") ~ proteccion_ben + wash_ben,
             TRUE ~ NA_real_)) %>% 
  mutate(pair_freq = ifelse(str_detect(combination, "alone$"), 0, pair_freq)) %>% 
  select(pcode3, ben_freq, combination, cluster, clust_freq, pair_freq)

```
