library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(openxlsx)

rm(list=ls()) #===EXCLUINDO TODOS OS DATASETS DO ENVIRONMENT

#===LENDO OS DADOS
channels <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/channels.csv") #CANAIS DE VENDA
deliveries <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/deliveries.csv") #ENTREGAS
drivers <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/drivers.csv") #ENTREGADORES
hubs <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/hubsM.csv") #CENTRO DE DISTRIBUICAO
orders <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/orders.csv") #VENDAS
payments <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/payments.csv") #PAGAMENTOS
stores <- read.csv("C:/Users/AquilaEstevao/Documents/DataLab do Delivery Center/datasets/storesM.csv") #LOJISTAS

hubs$AREA_KM2 <- NULL
hubs$hubs_cd_mun <- hubs$CD_MUN
hubs$CD_MUN <- NULL
hubs$hubs_nm_mun <- hubs$NM_MUN
hubs$NM_MUN <- NULL
hubs$hubs_uf <- hubs$SIGLA_UF
hubs$SIGLA_UF <- NULL
hubs$hub_state <- NULL

stores$AREA_KM2 <- NULL
stores$store_cd_mun <- stores$CD_MUN
stores$CD_MUN <- NULL
stores$stores_nm_mun <- stores$NM_MUN
stores$NM_MUN <- NULL
stores$stores_uf <- stores$SIGLA_UF
stores$SIGLA_UF <- NULL

#===DESCRITIVO DOS DADOS
summary(channels)
summary(deliveries)
summary(drivers)
summary(hubs)
summary(orders)
summary(payments)
summary(stores)

#===VERIFICANDO AUSÊNCIA DE DADOS
#===RETORNA "TRUE" SE FOR "NA"
#===RETORNA "FALSE" SE NÃO FOR "NA"
is.na(channels)
is.na(deliveries)
is.na(drivers)
is.na(hubs)
is.na(orders)
is.na(payments)
is.na(stores)

#===JUNTANDO OS DADOS
storhub <- left_join(stores, hubs, by = "hub_id")
deldriv <- left_join(deliveries, drivers, by = "driver_id")
orders <- left_join(orders, payments, by = "payment_order_id")
orders <- left_join(orders, channels, by = "channel_id")
orders <- left_join(orders, deldriv, by = "delivery_order_id")
orders <- left_join(orders, storhub, by = "store_id")

rm(storhub)
rm(deldriv)
rm(channels)
rm(deliveries)
rm(drivers)
rm(hubs)
rm(payments)
rm(stores)

orders_c <- orders

#===ESTRUTURA DA DATASET
names(orders_c)
str(orders_c)
summary(orders_c)

#===ALGUNS FILTROS
rnorm(orders_c$order_amount) #Funcao estatisitca para calcular densidade e funcao de distribuicao normal
max(orders_c$order_created_hour) #Obtendo o valor maximo de um objeto
unique(orders_c$order_status) #Obtendo valores unicos de order_status
unique(orders_c$payment_method) #Obtendo valores unicos de payment_method
unique(orders_c$payment_status) #Obtendo valores unicos de payment_status
unique(orders_c$channel_name) #Obtendo valores unicos de channel_name
unique(orders_c$channel_type) #Obtendo valores unicos de channel_type
unique(orders_c$delivery_status) #Obtendo valores unicos de delivery_status
unique(orders_c$driver_modal) #Obtendo valores unicos de driver_modal
unique(orders_c$driver_type) #Obtendo valores unicos de driver_type
unique(orders_c$store_name) #Obtendo valores unicos de store_name
unique(orders_c$store_segment) #Obtendo valores unicos de store_segment
unique(orders_c$hub_name) #Obtendo valores unicos de hub_name
unique(orders_c$stores_uf)
unique(orders_c$stores_nm_mun)
unique(orders_c$hubs_uf)
unique(orders_c$hubs_nm_mun)

orders_c$order_moment_created <- NULL #Excluindo objeto de segundos
#==================================================================================
#======SEPARANDO A VARIAVEL "order_moment_accepted" EM DIA, MES, ANO, HORA E MINUTO
#==================================================================================

orders_c <- orders_c %>%
  separate(order_moment_accepted, into = c("accepted_month", "accepted_day", "accepted_year"), sep = '/')

orders_c <- orders_c %>%
  separate(accepted_year, into = c("accepted_year", "accepted_hour", "accepted_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(accepted_hour, into = c("accepted_hour", "accepted_minute", "accepted_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(accepted_hour, accepted_AMPM) %>%
  #filter(accepted_AMPM == 'PM') %>%
  distinct() %>%
  arrange(accepted_AMPM, accepted_hour)
orders_read

#Ajustando a hora
orders_c$accepted_hour[orders_c$accepted_AMPM == "AM" & orders_c$accepted_hour == 12] <- 0
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 1] <- 13
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 2] <- 14
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 3] <- 15
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 4] <- 16
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 5] <- 17
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 6] <- 18
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 7] <- 19
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 8] <- 20
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 9] <- 21
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 10] <- 22
orders_c$accepted_hour[orders_c$accepted_AMPM == "PM" & orders_c$accepted_hour == 11] <- 23
#Transformando valores para inteiros
orders_c$accepted_month <- as.integer(orders_c$accepted_month)
orders_c$accepted_day <- as.integer(orders_c$accepted_day)
orders_c$accepted_year <- as.integer(orders_c$accepted_year)
orders_c$accepted_hour <- as.integer(orders_c$accepted_hour)
orders_c$accepted_minute <- as.integer(orders_c$accepted_minute)

#Verificando os valores de horas
orders_read <- orders_c %>%
  select(accepted_hour, accepted_AMPM) %>%
  #filter(accepted_AMPM == 'AM') %>%
  distinct() %>%
  arrange(accepted_AMPM, accepted_hour)
orders_read

rm(orders_read)

orders_c$order_moment_accepted <- NULL
#orders_c$accepted_AMPM <- NULL
orders_c$accepted_seconds <- NULL #Excluindo objeto de segundos
#============================================================================

#===============================================================================
#======SEPARANDO A VARIAVEL "order_moment_ready" EM DIA, MES, ANO, HORA E MINUTO
#===============================================================================

orders_c <- orders_c %>%
  separate(order_moment_ready, into = c("ready_month", "ready_day", "ready_year"), sep = '/')

orders_c <- orders_c %>%
  separate(ready_year, into = c("ready_year", "ready_hour", "ready_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(ready_hour, into = c("ready_hour", "ready_minute", "ready_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(ready_hour, ready_AMPM) %>%
  #filter(ready_AMPM == 'PM') %>%
  distinct() %>%
  arrange(ready_AMPM, ready_hour)
orders_read

orders_c$ready_hour[orders_c$ready_AMPM == "AM" & orders_c$ready_hour == 12] <- 0
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 1] <- 13
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 2] <- 14
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 3] <- 15
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 4] <- 16
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 5] <- 17
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 6] <- 18
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 7] <- 19
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 8] <- 20
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 9] <- 21
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 10] <- 22
orders_c$ready_hour[orders_c$ready_AMPM == "PM" & orders_c$ready_hour == 11] <- 23

orders_c$ready_month <- as.integer(orders_c$ready_month)
orders_c$ready_day <- as.integer(orders_c$ready_day)
orders_c$ready_year <- as.integer(orders_c$ready_year)
orders_c$ready_hour <- as.integer(orders_c$ready_hour)
orders_c$ready_minute <- as.integer(orders_c$ready_minute)

orders_read <- orders_c %>%
  select(ready_hour, ready_AMPM) %>%
  #filter(ready_AMPM == 'AM') %>%
  distinct() %>%
  arrange(ready_AMPM, ready_hour)
orders_read

rm(orders_read)

#orders_c$ready_AMPM <- NULL
orders_c$ready_seconds <- NULL #Excluindo objeto de segundos
#============================================================================

#===================================================================================
#======SEPARANDO A VARIAVEL "order_moment_collected" EM DIA, MES, ANO, HORA E MINUTO
#==================================================================================

orders_c <- orders_c %>%
  separate(order_moment_collected, into = c("collected_month", "collected_day", "collected_year"), sep = '/')

orders_c <- orders_c %>%
  separate(collected_year, into = c("collected_year", "collected_hour", "collected_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(collected_hour, into = c("collected_hour", "collected_minute", "collected_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(collected_hour, collected_AMPM) %>%
  #filter(collected_AMPM == 'AM') %>%
  distinct() %>%
  arrange(collected_AMPM, collected_hour)
orders_read

orders_c$collected_hour[orders_c$collected_AMPM == "AM" & orders_c$collected_hour == 12] <- 0
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 1] <- 13
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 2] <- 14
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 3] <- 15
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 4] <- 16
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 5] <- 17
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 6] <- 18
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 7] <- 19
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 8] <- 20
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 9] <- 21
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 10] <- 22
orders_c$collected_hour[orders_c$collected_AMPM == "PM" & orders_c$collected_hour == 11] <- 23

orders_c$collected_month <- as.integer(orders_c$collected_month)
orders_c$collected_day <- as.integer(orders_c$collected_day)
orders_c$collected_year <- as.integer(orders_c$collected_year)
orders_c$collected_hour <- as.integer(orders_c$collected_hour)
orders_c$collected_minute <- as.integer(orders_c$collected_minute)

orders_read <- orders_c %>%
  select(collected_hour, collected_AMPM) %>%
  #filter(collected_AMPM == 'PM') %>%
  distinct() %>%
  arrange(collected_hour)
orders_read

rm(orders_read)

#orders_c$collected_AMPM <- NULL
orders_c$collected_seconds <- NULL
#=============================================================================

#============================================================================
#SEPARANDO A VARIAVEL "order_moment_in_expedition" EM DIA, MES, ANO, HORA E MINUTO
#============================================================================

orders_c <- orders_c %>%
  separate(order_moment_in_expedition, into = c("expedition_month", "expedition_day", "expedition_year"), sep = '/')

orders_c <- orders_c %>%
  separate(expedition_year, into = c("expedition_year", "expedition_hour", "expedition_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(expedition_hour, into = c("expedition_hour", "expedition_minute", "expedition_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(expedition_hour, expedition_AMPM) %>%
  #filter(expedition_AMPM == 'PM') %>%
  distinct() %>%
  arrange(expedition_AMPM, expedition_hour)
orders_read

orders_c$expedition_hour[orders_c$expedition_AMPM == "AM" & orders_c$expedition_hour == 12] <- 0
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 1] <- 13
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 2] <- 14
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 3] <- 15
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 4] <- 16
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 5] <- 17
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 6] <- 18
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 7] <- 19
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 8] <- 20
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 9] <- 21
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 10] <- 22
orders_c$expedition_hour[orders_c$expedition_AMPM == "PM" & orders_c$expedition_hour == 11] <- 23

orders_c$expedition_month <- as.integer(orders_c$expedition_month)
orders_c$expedition_day <- as.integer(orders_c$expedition_day)
orders_c$expedition_year <- as.integer(orders_c$expedition_year)
orders_c$expedition_hour <- as.integer(orders_c$expedition_hour)
orders_c$expedition_minute <- as.integer(orders_c$expedition_minute)

orders_read <- orders_c %>%
  select(expedition_hour, expedition_AMPM) %>%
  #filter(expedition_AMPM == 'PM') %>%
  distinct() %>%
  arrange(expedition_AMPM, expedition_hour)
orders_read

rm(orders_read)

#orders_c$finished_AMPM <- NULL
orders_c$expedition_seconds <- NULL
#============================================================================


#=======================================================================================
#======SEPARANDO A VARIAVEL "order_moment_in_expedition" EM DIA, MES, ANO, HORA E MINUTO
#=======================================================================================

orders_c <- orders_c %>%
  separate(order_moment_delivering, into = c("delivering_month", "delivering_day", "delivering_year"), sep = '/')

orders_c <- orders_c %>%
  separate(delivering_year, into = c("delivering_year", "delivering_hour", "delivering_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(delivering_hour, into = c("delivering_hour", "delivering_minute", "delivering_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(delivering_hour, delivering_AMPM) %>%
  #filter(delivering_AMPM == 'PM') %>%
  distinct() %>%
  arrange(delivering_AMPM, delivering_hour)
orders_read

orders_c$delivering_hour[orders_c$delivering_AMPM == "AM" & orders_c$delivering_hour == 12] <- 0
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 1] <- 13
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 2] <- 14
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 3] <- 15
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 4] <- 16
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 5] <- 17
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 6] <- 18
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 7] <- 19
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 8] <- 20
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 9] <- 21
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 10] <- 22
orders_c$delivering_hour[orders_c$delivering_AMPM == "PM" & orders_c$delivering_hour == 11] <- 23

orders_c$delivering_month <- as.integer(orders_c$delivering_month)
orders_c$delivering_day <- as.integer(orders_c$delivering_day)
orders_c$delivering_year <- as.integer(orders_c$delivering_year)
orders_c$delivering_hour <- as.integer(orders_c$delivering_hour)
orders_c$delivering_minute <- as.integer(orders_c$delivering_minute)

orders_read <- orders_c %>%
  select(delivering_hour, delivering_AMPM) %>%
  #filter(delivering_AMPM == 'PM') %>%
  distinct() %>%
  arrange(delivering_AMPM, delivering_hour)
orders_read

rm(orders_read)

#orders_c$delivering_AMPM <- NULL
orders_c$delivering_seconds <- NULL
#===================================================================================

#===================================================================================
#======SEPARANDO A VARIAVEL "order_moment_delivered" EM DIA, MES, ANO, HORA E MINUTO
#===================================================================================

orders_c <- orders_c %>%
  separate(order_moment_delivered, into = c("delivered_month", "delivered_day", "delivered_year"), sep = '/')

orders_c <- orders_c %>%
  separate(delivered_year, into = c("delivered_year", "delivered_hour", "delivered_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(delivered_hour, into = c("delivered_hour", "delivered_minute", "delivered_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(delivered_hour, delivered_AMPM) %>%
  #filter(delivered_AMPM == 'AM') %>%
  distinct() %>%
  arrange(delivered_AMPM, delivered_hour)
orders_read

orders_c$delivered_hour[orders_c$delivered_AMPM == "AM" & orders_c$delivered_hour == 12] <- 0
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 1] <- 13
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 2] <- 14
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 3] <- 15
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 4] <- 16
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 5] <- 17
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 6] <- 18
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 7] <- 19
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 8] <- 20
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 9] <- 21
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 10] <- 22
orders_c$delivered_hour[orders_c$delivered_AMPM == "PM" & orders_c$delivered_hour == 11] <- 23

orders_c$delivered_month <- as.integer(orders_c$delivered_month)
orders_c$delivered_day <- as.integer(orders_c$delivered_day)
orders_c$delivered_year <- as.integer(orders_c$delivered_year)
orders_c$delivered_hour <- as.integer(orders_c$delivered_hour)
orders_c$delivered_minute <- as.integer(orders_c$delivered_minute)

orders_read <- orders_c %>%
  select(delivered_hour, delivered_AMPM) %>%
  #filter(AM_PM == 'AM') %>%
  distinct() %>%
  arrange(delivered_AMPM, delivered_hour)
orders_read

rm(orders_read)

#orders_c$delivered_AMPM <- NULL
orders_c$delivered_seconds <- NULL
#===================================================================================

#============================================================================
#SEPARANDO A VARIAVEL "order_moment_finished" EM DIA, MES, ANO, HORA E MINUTO
#============================================================================

orders_c <- orders_c %>%
  separate(order_moment_finished, into = c("finished_month", "finished_day", "finished_year"), sep = '/')

orders_c <- orders_c %>%
  separate(finished_year, into = c("finished_year", "finished_hour", "finished_AMPM"), sep = ' ')

orders_c <- orders_c %>%
  separate(finished_hour, into = c("finished_hour", "finished_minute", "finished_seconds"), sep = ':')

orders_read <- orders_c %>%
  select(finished_hour, finished_AMPM) %>%
  #filter(finished_AMPM == 'PM') %>%
  distinct() %>%
  arrange(finished_AMPM, finished_hour)
orders_read

orders_c$finished_hour[orders_c$finished_AMPM == "AM" & orders_c$finished_hour == 12] <- 0
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 1] <- 13
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 2] <- 14
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 3] <- 15
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 4] <- 16
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 5] <- 17
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 6] <- 18
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 7] <- 19
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 8] <- 20
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 9] <- 21
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 10] <- 22
orders_c$finished_hour[orders_c$finished_AMPM == "PM" & orders_c$finished_hour == 11] <- 23

orders_c$finished_month <- as.integer(orders_c$finished_month)
orders_c$finished_day <- as.integer(orders_c$finished_day)
orders_c$finished_year <- as.integer(orders_c$finished_year)
orders_c$finished_hour <- as.integer(orders_c$finished_hour)
orders_c$finished_minute <- as.integer(orders_c$finished_minute)


summary(orders_c)

orders_read <- orders_c %>%
  select(finished_hour, finished_AMPM) %>%
  #filter(finished_AMPM == 'PM') %>%
  distinct() %>%
  arrange(finished_AMPM, finished_hour)
orders_read

rm(orders_read)

#orders_c$finished_AMPM <- NULL
orders_c$finished_seconds <- NULL
#===================================================================================

names(orders_c)

#===EXPORTANDO O DATASET
write.xlsx(orders_c, 'C:/Users/AquilaEstevao/Downloads/ordersJunto.xlsx')
write.csv(orders_c, 'C:/Users/AquilaEstevao/Downloads/ordersJunto.csv')

#SELECIONANDO AS VARIAVEIS (KPI)

orders_v <- orders_c %>% select(order_id, order_amount, order_created_hour, order_created_month, accepted_month, accepted_hour, ready_month, ready_hour, collected_month, collected_hour, expedition_month, expedition_hour, delivering_month, delivering_hour, delivering_AMPM, delivered_AMPM, finished_AMPM, order_metric_transit_time, payment_amount, payment_status, delivery_status, store_plan_price, hub_name, hubs_nm_mun, accepted_day, ready_day, collected_day, expedition_day, delivering_day, delivered_month, finished_month, channel_name, driver_modal, store_name, stores_nm_mun, hubs_uf, order_status, order_delivery_cost, order_created_day, accepted_AMPM, ready_year, ready_AMPM, collected_year, collected_AMPM, expedition_year, expedition_AMPM, delivering_year, delivered_day, finished_day, payment_method, channel_type, driver_type, store_segment, stores_uf, delivered_hour, finished_hour)

#===SELECTS PARA VISUALIZACAO
# QUANTIDADE DE ORDENS POR CANAL
channels <- orders_v %>%
  group_by(channel_name) %>%
  summarise(QntOrdens = n_distinct(order_id, na.rm = FALSE)) %>%
  arrange(-QntOrdens)

orders_c$channel_name

created_month <- orders_v %>%
  group_by(order_created_month) %>%
  summarise(QntOrdenMes = n_distinct(order_id, na.rm = FALSE)) %>%
  arrange(-QntOrdenMes)


munic <- orders_v %>%
  group_by(stores_nm_mun) %>%
  summarise(QntPedidos = n_distinct(order_id, na.rm = FALSE)) %>%
  arrange(-QntPedidos)

munic 

canais <- orders_v %>%
  group_by(channel_type) %>%
  summarise(QntPedidos = n_distinct(order_id, na.rm = FALSE)) %>%
  arrange(-QntPedidos)

canais             

valPedido <- orders_v %>%
  group_by(channel_type) %>%
  summarise(ValPed = n_distinct(order_amount, na.rm = FALSE)) %>%
  arrange(-ValPed)

valPedido

storPlan <- orders_v %>%
  group_by(channel_type) %>%
  summarise(QntStorPlan = n_distinct(order_amount, na.rm = FALSE)) %>%
  arrange(-ValPed)


#VISUALIZAÇÃO DE DADOS

ggplot(orders_v, aes(x=channel_type, y=store_plan_price)) + 
  geom_boxplot(aes(fill=channel_type))

ggplot(orders_v, aes(x=channel_type, y=order_amount)) + 
  geom_boxplot(aes(fill=channel_type))


ggplot(orders_v, aes(x=stores_nm_mun, y=order_amount)) + 
  geom_boxplot(aes(fill=stores_nm_mun))



