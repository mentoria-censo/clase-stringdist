library(tidyverse)
library(stringdist)
library(arrow)

library(glue)
ruta_pueblos = 'C:/Users/Jeconchao/Instituto Nacional de Estadisticas/ciencia de datos - pueblos'
pueblos = arrow::read_feather('{ruta_pueblos}/data_pueblos_codificacion.feather' %>% glue, as_data_frame = T)


pueblos_otro = pueblos %>% filter(p16a == 10)

afro = pueblos_otro %>% count(estr_p16glospueblo ) %>% filter(str_detect(estr_p16glospueblo, 'DIENTE$'))
afro %>% mutate(distancia_lv = stringdist(estr_p16glospueblo, 'AFRODESCENDIENTE', method = 'lv'),
                distancia_osa = stringdist(estr_p16glospueblo, 'AFRODESCENDIENTE', method = 'osa'),
                distancia_jac = stringdist(estr_p16glospueblo, 'AFRODESCENDIENTE', method = 'jaccard'),
                distancia_cos = stringdist(estr_p16glospueblo, 'AFRODESCENDIENTE', method = 'cosine')
                ) %>% view

afro_o_mapuche = pueblos_otro %>% count(estr_p16glospueblo ) %>% filter(str_detect(estr_p16glospueblo, 'MAPU|(DIENTE$)'))
afro_o_mapuche = pueblos_otro %>% count(estr_p16glospueblo ) %>% filter(str_detect(estr_p16glospueblo, 'TOD.S'))

expand.grid(afro_o_mapuche$estr_p16glospueblo, c('AFRODESCENDIENTE', 'MAPUCHE')) %>%
  mutate(distancia_cos = stringdist(Var1, Var2 , method = 'cosine')) %>%
  group_by(Var1) %>% mutate(group_min = Var2[which.min(distancia_cos)]) %>%
  pivot_wider(id_cols = c('Var1', 'group_min'), names_from = 'Var2', values_from = 'distancia_cos') %>% view





stringdist::afind(afro$estr_p16glospueblo, pattern = 'AFRODESCENDIENTE') %>% view


