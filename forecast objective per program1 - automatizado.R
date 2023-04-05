library(plotly)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(tibbletime)
library(forecast)
library(prophet)
library(openxlsx)

path = "C:/Users/mario.vidana/Documents/dashboards/"
#Funcion que agarra el modelo con menor MAPE y lo ejecuta para cada programa.

MAPE <- function(programa,nombre_prog){
  programa = programa %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == nombre_prog) %>% mutate(Year = year(Fecha),Month = month(Fecha)) %>% 
    group_by(Year,Month) %>% summarise(Fecha = Fecha[1],Rating_miles = mean(Rating_miles))
  Inicio = programa$Year %>% min()
  Inicio_sem = programa$Month[programa$Year == Inicio] %>% min()
  Inicio_year = programa %>% tail(6) %>% .$Year %>% head(1)
  Fecha_sep = programa$Fecha[programa$Month == programa$Month %>% tail(6) %>% head(1) & programa$Year == Inicio_year]
  Inicio_week = programa$Month[programa$Fecha == Fecha_sep]
  test_df = programa %>% tail(6)
  
  
  train_data = ts(programa %>% filter(Fecha < Fecha_sep) %>% select(Rating_miles) ,start=c(Inicio,Inicio_sem),frequency = 12)
  test_data = ts(programa %>% filter(!Fecha < Fecha_sep) ,start=c(Inicio_year,Inicio_week),frequency = 12)
  prop = programa %>% filter(Fecha < Fecha_sep)  %>% select(Fecha,Rating_miles) #train data
  colnames(prop) = c("ano","ds","y")
  
  arima = auto.arima(train_data[,2],stepwise = TRUE,D=1)
  ets = ets(train_data[,2])
  #stlf for seasonal forecasts
  # stlm = stlm(train_data[,2],method = "arima")
  # tbats = tbats(train_data[,2])
  # arfima = arfima(train_data[,2],estim = "ls")
  
  ##PRUEBA DEL MODELO ARIMA
  fcast1 <- forecast(arima,h = 6,level = c(99.5))
  fcast2 <- as.numeric(fcast1$mean)
  real1 <- as.numeric(test_data[,4])
  
  df1 <- rbind(data.frame(tipo = "fcast",
                          valor = fcast2, semana = test_df$Month),
               data.frame(tipo = "real", valor = real1, semana = test_df$Month))
  
  MAPE_ARIMA = df1 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  
  #PRUEBA DEL MODELO ETS
  fcast3 <- forecast(ets,h = 6,level = c(99.5))
  fcast4 <- as.numeric(fcast3$mean)
  
  df2 <- rbind(data.frame(tipo = "fcast",
                          valor = fcast4, mes = test_df$Month),
               data.frame(tipo = "real", valor = real1, mes = test_df$Month))
  
  MAPE_ETS = df2 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  
  ##PRUEBA DEL MODELO STLM
  # fcast5 <- forecast(stlm,h = 6,level = c(99.5))
  # fcast6 <- as.numeric(fcast5$mean)
  # 
  # df3 <- rbind(data.frame(tipo = "fcast",
  #                         valor = fcast6, mes = Min_month:Max_month),
  #              data.frame(tipo = "real", valor = real1, mes = Min_month:Max_month))
  # 
  # MAPE_STLM = df3 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  
  
  # ##PRUEBA DEL MODELO TBATS
  # fcast7 <- forecast(tbats,h = 6,level = c(99.5))
  # fcast8 <- as.numeric(fcast7$mean)
  # 
  # df4 <- rbind(data.frame(tipo = "fcast",
  #                         valor = fcast8, mes = Min_month:Max_month),
  #              data.frame(tipo = "real", valor = real1, mes = Min_month:Max_month))
  # 
  # MAPE_TBATS = df4 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  # 
  # ##PRUEBA DEL MODELO ARFIMA
  # fcast9 <- forecast(arfima,h = 6,level = c(99.5))
  # fcast10 <- as.numeric(fcast9$mean)
  # 
  # df5 <- rbind(data.frame(tipo = "fcast",
  #                         valor = fcast10, mes = Min_month:Max_month),
  #              data.frame(tipo = "real", valor = real1, mes = Min_month:Max_month))
  # 
  # MAPE_ARFIMA = df5 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  
  ##PRUEBA MODELO PROPHET
  m = prophet(prop[,2:3])
  futuro = make_future_dataframe(m,periods = 6,freq = "month")
  
  prediccion = predict(m,futuro)
  
  fcastlast=tail(prediccion[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) %>% .$yhat
  
  df6 <- rbind(data.frame(tipo = "fcast",
                          valor = fcastlast, mes = test_df$Month),
               data.frame(tipo = "real", valor = real1, mes = test_df$Month))
  
  MAPE_PROP = df6 %>% spread(tipo,valor,0) %>% mutate(diff = abs((real-fcast)/real)) %>% .$diff %>% mean()
  
  #modelo ganador = menor mape
  # mapes = c(MAPE_ARIMA,MAPE_ETS,MAPE_STLM,MAPE_TBATS,MAPE_ARFIMA,MAPE_PROP)
  # modelos = c("ARIMA","ETS","STLM","TBATS","ARFIMA","PROPHET")
  mapes = c(MAPE_ARIMA,MAPE_ETS,MAPE_PROP)
  modelos = c("ARIMA","ETS","PROPHET")
  best_mape = which.min(mapes)
  best_model = modelos[best_mape]
  
  #prediccion si prophet gana
  if(best_mape == 3){
    prop = programa %>% head(-1) %>% select(Fecha,Rating_miles)
    colnames(prop) = c("ano","ds","y")
    
    m = prophet(prop[,2:3])
    futuro = make_future_dataframe(m,periods = 3,freq = "month")
    
    prediccion = predict(m,futuro) %>% select(ds,yhat_lower,yhat,yhat_upper)
    
    
    set.seed(50)
    scenarios <- data.frame(matrix(NA, nrow = 100, ncol = 3))
    colnames(scenarios) <- paste0("h.", 1:3)
    prediccion2 = prediccion %>% tail(3)
    for (i in 1:3) {
      scenarios[, i] <- round(runif(100, prediccion2$yhat_lower[i], prediccion2$yhat_upper[i]), 2)
    }
    
    prediction = prediccion %>% tail(3) %>% select(ds,yhat)
    prediction = prop[(nrow(prop)-11):nrow(prop),]$y %>% append(prediction[[2]])
    
    means = c()
    
    for(i in 1:100){
      means = means %>% append(scenarios[i,] %>% as.numeric() %>% mean())
    }
    
    histograma = hist(means,xlab = "Rating Promedio",ylab = "Frecuencia",breaks = 15,main = "Distribucion simulaciones del rating promedio de 3 semanas")
    cuartiles = quantile(means)
    
    
    ##Grafica prediccion prophet
    grafica = plot(m,prediccion)
    
  }else if(best_mape == 1 | best_mape == 2){
    train_data = ts(programa %>% head(-1) %>% select(Rating_miles) ,start=c(Inicio,Inicio_sem),frequency = 12)
    
    if(best_mape == 1){
      model = auto.arima(train_data[,2],stepwise = TRUE,D=1)
    }else if(best_mape == 2){
      model = ets(train_data[,2])
    }
    simulations = c()
    S1 = simulate(model,nsim = 3,seed = 100)
    simulations[[1]] = S1
    seed = S1
    seed2 = 101
    for(i in 2:100){
      .Random.seed <- attr(seed, "seed")
      identical(seed,simulate(model,nsim=3))
      
      seed = simulate(model,nsim=3,seed = seed2)
      
      seed2 = seed2+1
      
      simulations[[i]] = c(seed)
    }
    
    grafica = plot(forecast(model, h = 3),type = "l")
    rat_reales = train_data[(nrow(train_data)-11):nrow(train_data),2]
    prediction = forecast(model,3)$mean[1:3]
    prediction = c(rat_reales,prediction)
    means = c()
    for(i in 1:100){
      means = means %>% append(simulations[[i]] %>% mean())
    }
    
    histograma = hist(means,xlab = "Rating Promedio",ylab = "Frecuencia",breaks = 15,main = "Distribucion simulaciones del rating promedio de 3 meses")
    cuartiles = means %>% quantile()
  }
  return(list(mapes,modelos,paste0("El mejor modelo fue el: ",best_model),grafica,prediction,cuartiles))
  
  
}
ATS_Format = function(df){
  df$ATS[df$ATS == "n.a"] = "0.00"
  df$ATS = as.double(df$ATS,6)
  df$Horas = df$ATS * 24
  splits  = str_split(as.character(df$Horas),pattern = "[.]")
  horas = c()
  minutos = c()
  
  for(i in 1:length(splits)){
    horas = horas %>% append(splits[[i]][1])
    minutos = minutos %>% append(ifelse(splits[[i]][2] %>% is.na(),"0.00",paste0("0.",splits[[i]][2])))
  }
  minutos = as.double(minutos,6)*60
  
  splits = str_split(as.character(minutos),pattern = "[.]")
  
  minutes = c()
  seconds = c()
  for(i in 1:length(splits)){
    minutes = minutes %>% append(ifelse(splits[[i]][1] %>% nchar() == 1,paste0("0",splits[[i]][1]),splits[[i]][1]))
    seconds = seconds %>% append(ifelse(splits[[i]][2] %>% is.na(),"0.00",paste0("0.",splits[[i]][2])))
  }
  
  seconds = as.character(ifelse(round(as.double(seconds)*60,0) %>% nchar == 1,paste0("0",round(as.double(seconds)*60,0)),
                                round(as.double(seconds)*60,0)))
  
  df$ATS = paste0(paste0("0",horas),":",minutes,":",seconds)
  df$ATS[df$ATS == "n.a"] = "00:00:00"
  tomin = hms(df$ATS)
  df$ATS = ifelse(!hour(tomin) == 0,hour(tomin)*60 + minute(tomin) + second(tomin)/60,minute(tomin) + second(tomin)/60)
  
  return(df)
}


archivos = file.info(list.files(path,full.names = T))
lv = archivos%>%filter(str_detect(row.names(archivos),"Dashboard LV"))
lv_actual  = rownames(lv)[which.max(lv$ctime)]

fines = archivos%>%filter(str_detect(row.names(archivos),"Dashboard Fines"))
fines_actual  = rownames(fines)[which.max(fines$ctime)]

cdmx = read_xlsx(lv_actual ,range = "A1:L3170")

cdmx = cdmx[,-1]
cdmx = cdmx[-1,]
colnames(cdmx) = c("Fecha","Programa","Vehiculo","Rating_miles","Rating_porc","ATS","Reach","Rat_hog","Rat_p_Hog","ATS_HOG","Rch_hog")
cdmx$Fecha = cdmx$Fecha %>% as.Date("%d/%m/%Y")
cdmx$Rating_miles = as.double(gsub(",","",cdmx$Rating_miles),2)

cdmx = ATS_Format(cdmx)

fines = read_xlsx(fines_actual,range = "A1:L1082")
fines = fines[,-1]
fines = fines[-1,]
colnames(fines) = c("Fecha","Programa","Vehiculo","Rating_miles","Rating_porc","ATS","Reach","Rat_hog","Rat_p_Hog","ATS_HOG","Rch_hog")
fines$Fecha = fines$Fecha %>% as.Date("%d/%m/%Y")
fines$Rating_miles = as.double(gsub(",","",fines$Rating_miles),2)
fines = ATS_Format(fines)


fines_personas = fines %>% select(Fecha,Programa,Vehiculo,Rating_miles,ATS) %>% data.frame()
fines_hogares = fines %>% select(Fecha,Programa,Vehiculo,Rat_p_Hog,ATS_HOG,Rch_hog) %>% data.frame()



cdmx_personas = cdmx %>% select(Fecha,Programa,Vehiculo,Rating_miles,ATS) %>% data.frame() %>% rbind(fines_personas)
cdmx_hogares = cdmx %>% select(Fecha,Programa,Vehiculo,Rat_p_Hog,ATS_HOG,Rch_hog) %>% data.frame() %>% rbind(fines_hogares)
colnames(cdmx_hogares) = c("Fecha","Programa","Vehiculo","Rating_miles","ATS","Reach")
cdmx_hogares$Rating_miles = as.double(gsub(",","",cdmx_hogares$Rating_miles),3)
cdmx_hogares = ATS_Format(cdmx_hogares)
cdmx_personas$Rating_miles = as.double(gsub(",","",cdmx_personas$Rating_miles),2)


programas = cdmx_personas$Programa %>% unique()
rat_hog_ayer = c()
rat_hog_semana = c()
rat_hog_mes = c()
rat_ayer = c()
rat_semana = c()
rat_mes = c()
share_ayer = c()
share_semana = c()
share_mes = c()
ats_ayer = c()
ats_semana = c()
ats_mes = c()

for(i in programas){
  rat_hog_ayer = rat_hog_ayer %>% append(cdmx_hogares %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% filter(Fecha == Fecha %>% max()) %>% .$Rating_miles)
  rat_hog_semana = rat_hog_semana %>% append(cdmx_hogares %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% 
                                               filter(Fecha >= Fecha %>% max()%m-%days(7)) %>% .$Rating_miles %>% mean() %>% round(2))
  rat_hog_mes = rat_hog_mes %>% append(cdmx_hogares %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% 
                                         filter(Fecha >= Fecha %>% max()%m-%months(1)) %>% .$Rating_miles %>% mean() %>% round(2))
  rat_ayer = rat_ayer %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% filter(Fecha == Fecha %>% max()) %>% .$Rating_miles %>% round(2))
  rat_semana = rat_semana %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% 
                                       filter(Fecha >= Fecha %>% max() %m-% days(7)) %>% .$Rating_miles %>% mean() %>% round(2))
  rat_mes = rat_mes %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% 
                                 filter(Fecha >= Fecha %>% max() %m-% months(1)) %>% .$Rating_miles %>% mean() %>% round(2))
  share_ayer = share_ayer %>% append(cdmx_personas %>% select(Fecha,Programa,Vehiculo,Rating_miles) %>% filter(Vehiculo %in% c( "MULTIMEDIOS TOTAL","TOTAL ENCENDIDOS"),Programa == i) %>% 
                                filter(Fecha == Fecha %>% max()) %>% spread(Vehiculo,Rating_miles,0) %>% 
                                  mutate(Share = `MULTIMEDIOS TOTAL`/ `TOTAL ENCENDIDOS`) %>% .$Share %>% round(4))
  share_semana = share_semana %>% append(cdmx_personas %>% select(Fecha,Programa,Vehiculo,Rating_miles) %>% filter(Vehiculo %in% c( "MULTIMEDIOS TOTAL","TOTAL ENCENDIDOS"),Programa == i) %>%
                                  filter(Fecha >= Fecha %>% max() %m-% days(7)) %>% spread(Vehiculo,Rating_miles,0) %>% 
                                    mutate(Share = `MULTIMEDIOS TOTAL`/ `TOTAL ENCENDIDOS`) %>% .$Share %>% mean()%>% round(4))
  share_mes = share_mes %>% append(cdmx_personas %>% select(Fecha,Programa,Vehiculo,Rating_miles) %>% filter(Vehiculo %in% c( "MULTIMEDIOS TOTAL","TOTAL ENCENDIDOS"),Programa == i) %>%
                                  filter(Fecha >= Fecha %>% max() %m-% months(1)) %>% spread(Vehiculo,Rating_miles,0) %>% 
                                    mutate(Share = `MULTIMEDIOS TOTAL`/ `TOTAL ENCENDIDOS`) %>% .$Share %>% mean()%>% round(4))
  ats_ayer = ats_ayer %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% filter(Fecha == Fecha %>% max()) %>% .$ATS %>% round(2))
  ats_semana = ats_semana %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% filter(Fecha >= Fecha %>% max() %m-% days(7)) %>%
                                       .$ATS %>% mean() %>% round(2))
  ats_mes = ats_mes %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% filter(Fecha >= Fecha %>% max() %m-% months(1)) %>%
                                 .$ATS %>% mean() %>% round(2))
}



resumen = data.frame(Programas = programas,Rat_Ayer = rat_ayer,Rat_Semana = rat_semana,Rat_Mes = rat_mes,Share_ayer = share_ayer,Share_semana = share_semana,Share_mes = share_mes,
                     Ats_ayer = ats_ayer,Ats_semana = ats_semana,Ats_mes = ats_mes,Rat_Hog_ayer = rat_hog_ayer,Rat_Hog_semana = rat_hog_semana,Rat_hog_Mes = rat_hog_mes)




#Share genero de programas
competencia = read_xlsx("C:/Users/mario.vidana/Documents/competidores share.xlsx",range = "A1:AI4")
share_genero_ayer = c()
share_genero_semana = c()
share_genero_mes = c()
prog2 = c()

for(i in programas){
  prog2 = prog2 %>% append(i)
  competidores = competencia[[i]] %>% append("MULTIMEDIOS TOTAL")
  share_genero_ayer = share_genero_ayer %>% append(cdmx_personas %>% filter(Programa == i ,Vehiculo %in% competidores) %>% filter(Fecha == Fecha %>% max()) %>% 
    mutate(Rat_sum = sum(Rating_miles)) %>% group_by(Vehiculo) %>% summarise(Share = Rating_miles/Rat_sum) %>% filter(Vehiculo == "MULTIMEDIOS TOTAL") %>% 
      .$Share %>% round(4))
  share_genero_semana = share_genero_semana %>% append(cdmx_personas %>% filter(Programa == i ,Vehiculo %in% competidores) %>% filter(Fecha >= Fecha %>% max()%m-%days(7)) %>% group_by(Vehiculo) %>% 
    summarise(Rating_miles = mean(Rating_miles)) %>% 
    mutate(Rat_sum = sum(Rating_miles)) %>% group_by(Vehiculo) %>% summarise(Share = Rating_miles/Rat_sum) %>% filter(Vehiculo == "MULTIMEDIOS TOTAL") %>% 
      .$Share)
  share_genero_mes = share_genero_mes %>% append(cdmx_personas %>% filter(Programa == i ,Vehiculo %in% competidores) %>% filter(Fecha >= Fecha %>% max()%m-%months(1)) %>% group_by(Vehiculo) %>% 
    summarise(Rating_miles = mean(Rating_miles)) %>% 
    mutate(Rat_sum = sum(Rating_miles)) %>% group_by(Vehiculo) %>% summarise(Share = Rating_miles/Rat_sum)%>% filter(Vehiculo == "MULTIMEDIOS TOTAL") %>% 
      .$Share)
}


resumen_share = data.frame(Programa = prog2,Share_ayer = share_genero_ayer,Share_semana = share_genero_semana,Share_mes = share_genero_mes)


#info de competidores IMAGEN, Azteca 1 y televisa
rat_comp_ayer = c()
rat_comp_semana = c()
rat_comp_mes = c()
ats_comp = c()
ats_comp_ayer = c()
ats_comp_semana = c()
ats_comp_mes = c()
canales = c()
comp_directos = c("AZTECA UNO","IMAGEN TV","LAS ESTRELLAS","FORO TV","AZTECA 7")
prog = c()
for(i in programas){
  for(y in comp_directos){
    prog = prog %>% append(i)
    canales = canales %>% append(y)
    rat_comp_ayer = rat_comp_ayer %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha == Fecha %>% max()) %>% 
      .$Rating_miles)
    rat_comp_semana = rat_comp_semana %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%days(7)) %>%
      .$Rating_miles %>% mean() %>% round(2))
    rat_comp_mes = rat_comp_mes %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%months(1)) %>%
      .$Rating_miles %>% mean() %>% round(2))  
    ats_comp_ayer = ats_comp_ayer %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha == Fecha %>% max()) %>%
                             .$ATS)
    ats_comp_semana = ats_comp_semana %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%days(7)) %>%
                                                   .$ATS %>% mean())
    ats_comp_mes = ats_comp_mes %>% append(cdmx_personas %>% filter(Vehiculo == y,Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%months(1)) %>%
                                                   .$ATS %>% mean())
  }
  
  
}

resumen_comp = data.frame(Programas = prog,Canal = canales,Rating_ayer = rat_comp_ayer,Rating_semana = rat_comp_semana,
                          Rating_mes = rat_comp_mes,ATS_ayer = ats_comp_ayer,ATS_Semana = ats_comp_semana,ATS_Mes = ats_comp_mes)
        

#medias moviles para objetivo de share ats y carry over
#MA(4) de la semana
media_movil = 10
prog = c()
mm_sharesem = c()
mm_atssem = c()
mm_sharemes = c()
mm_atsmes = c()
for(i in programas){
  prog = prog %>% append(i)
  #semanal
  mm_sharesem = mm_sharesem %>% append(cdmx_personas %>% filter(Vehiculo %in% c( "MULTIMEDIOS TOTAL","TOTAL ENCENDIDOS"),Programa == i) %>% mutate(semana = week(as.Date(Fecha,"%Y-%m-%d")),year = year(Fecha)) %>% 
    select(Fecha,semana,year,Programa,Vehiculo,Rating_miles) %>% group_by(year,semana,Vehiculo) %>% summarise(Rating_miles = mean(Rating_miles)) %>% 
    spread(Vehiculo,Rating_miles,0) %>% mutate(Share = `MULTIMEDIOS TOTAL`/`TOTAL ENCENDIDOS`) %>% tail(media_movil) %>% .$Share %>% mean() %>% round(4))
  
  mm_atssem = mm_atssem %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% mutate(semana = week(as.Date(Fecha,"%Y-%m-%d")),year = year(Fecha)) %>% 
    group_by(year,semana) %>% summarise(Rating_miles = mean(Rating_miles),ATS = mean(ATS)) %>% tail(media_movil) %>% .$ATS %>% mean())
  
  #mensual
  mm_sharemes = mm_sharemes %>% append(cdmx_personas %>% filter(Vehiculo %in% c( "MULTIMEDIOS TOTAL","TOTAL ENCENDIDOS"),Programa == i) %>% mutate(mes = month(as.Date(Fecha,"%Y-%m-%d")),year = year(Fecha)) %>% 
    select(Fecha,mes,year,Programa,Vehiculo,Rating_miles) %>% group_by(year,mes,Vehiculo) %>% summarise(Rating_miles = mean(Rating_miles))%>% 
    spread(Vehiculo,Rating_miles,0) %>% mutate(Share = `MULTIMEDIOS TOTAL`/`TOTAL ENCENDIDOS`) %>% tail(media_movil) %>% .$Share %>% mean() %>% round(4))
  
  mm_atsmes = mm_atsmes %>% append(cdmx_personas %>% filter(Vehiculo == "MULTIMEDIOS TOTAL",Programa == i) %>% mutate(mes = month(Fecha),year = year(Fecha)) %>% group_by(year,mes) %>% 
    summarise(Rating_miles = mean(Rating_miles),ATS = mean(ATS)) %>% tail(media_movil) %>% .$ATS %>% mean())
}

resumen_mas = data.frame(Programa = prog,Share_semanal = mm_sharesem,ATS_semanal = mm_atssem,Share_mensual = mm_sharemes,ATS_mes = mm_atsmes)


#carry over primera descarga
# carry = read.csv("C:/Users/mario.vidana/Documents/dashboards/carry over cdmx2.txt",sep = ";")
# carry = carry[-1,]
# carry = carry[,-1]
# colnames(carry) = c("Fecha","Horario","Vehiculo","Rating_miles")
# carry$Fecha = as.Date(carry$Fecha,"%d/%m/%Y")
# carry$Rating_miles = as.double(gsub(",","",carry$Rating_miles),2)
# saveRDS(carry,paste0(path,"Carry.Rda"))

#carry over descargas nuevas-----------
carry_historic = readRDS(paste0(path,"Carry.Rda"))

archivos = file.info(list.files(path,full.names = T))
CO = archivos%>%filter(str_detect(row.names(archivos),"Carry Over"))
CO_actual  = rownames(CO)[which.max(CO$ctime)]
carry_actual = read_xlsx(CO_actual ,range = "A1:E30002")
carry_actual = carry_actual[-1,]
carry_actual = carry_actual[,-1]
colnames(carry_actual) = c("Fecha","Horario","Vehiculo","Rating_miles")
carry_actual$Fecha = as.Date(carry_actual$Fecha,"%Y-%m-%d")
carry_actual$Rating_miles = as.double(gsub(",","",carry_actual$Rating_miles),2)

#pegar las filas de los dias superiores al maximo de carry_historic
carry_max = carry_historic$Fecha %>% max()
carry_actual_max = carry_actual$Fecha %>% max()

if(carry_max < carry_actual_max){
  add_rows = carry_actual %>% filter(Fecha > carry_max)
  carry_historic = carry_historic %>% rbind(add_rows)
  saveRDS(carry_historic,paste0(path,"Carry.Rda"))
}


#eliminar los primeros dias para que solo queden 35 dias como maximo.----
if(difftime(carry_historic$Fecha %>% max,carry_historic$Fecha %>% min(),units = "day") %>% as.numeric() > 35){
  elementos_delete = difftime(carry_historic$Fecha %>% max,carry_historic$Fecha %>% min(),units = "day") %>% 
    as.numeric() - 35
  carry_historic = carry_historic %>% filter(!Fecha <= Fecha %>% min() %m+% days(elementos_delete))
  rownames(carry_historic) = NULL
  saveRDS(carry_historic,paste0(path,"Carry.Rda"))
}

#procesamiento del carry para sacar el df-----
carry = carry_historic

carry = carry %>% mutate(hour = substr(Horario,1,2)) %>% 
  mutate(Hora =ymd_hms(paste0("1899-12-31 ",ifelse(hour == "24",
                                                   paste0("00:",substr(Horario,4,8)),ifelse(hour == "25",
                                                                                            paste0("01:",substr(Horario,4,8)),
                                                                                            substr(Horario,1,8))))),dia_sem = weekdays(Fecha))
horarios = read_xlsx("C:/Users/mario.vidana/Documents/horarios cdmx.xlsx",sheet = "Horario")

canal_id = c()
prog4 = c()
co_ayer = c()
co_semana = c()
co_mes = c()
canales = carry$Vehiculo %>% unique()
colnames(horarios) = c("dia_sem","Hora","Fin","Programa")

for(r in canales ){
  carry_mm = carry %>% filter(Vehiculo == r)
  #este ultimo lo que hace sera que haremos un join ya por hora de inicio y fin en cada programa y calcularemos el carry over
  carry_mm$rating_promedio <- sapply(1:nrow(carry_mm), function(i) {
    if (i <= 5) {
      NA
    } else {
      mean(carry_mm$Rating_miles[(i - 5):(i - 1)])
    }
  })

  carry_mm = carry_mm %>% left_join(horarios,by = c("dia_sem","Hora")) %>% na.omit()
  co = c()
  for(i in 1:nrow(carry_mm)){
    coaux = carry_mm$rating_promedio[i+1]/carry_mm$rating_promedio[i]-1
    co = co %>% append(ifelse((coaux %>% is.infinite()) | (coaux %>% is.na()),0,coaux))
  }
  
  carry_mm = carry_mm %>% cbind(CO = co)
  
  
  programas = carry_mm$Programa %>% unique()
  for(i in programas){
    canal_id = canal_id %>% append(r)
    prog4 = prog4 %>% append(i)
    co_ayer = co_ayer %>% append(carry_mm %>% filter(Programa == i) %>% filter(Fecha == Fecha %>% max()) %>% .$CO) 
    co_semana = co_semana %>% append(carry_mm %>% filter(Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%days(7)) %>% .$CO %>% mean())
    co_mes = co_mes %>% append(carry_mm %>% filter(Programa == i) %>% filter(Fecha >= Fecha %>% max()%m-%months(1)) %>% .$CO %>% mean() )
    
  }
  
  if(r == "MULTIMEDIOS TOTAL"){
    carry_MMT = carry_mm
  }
}


resumen_carry = data.frame(Vehiculo = canal_id,Programa = prog4,ayer = co_ayer,semana = co_semana,mes = co_mes) %>% mutate(concat = paste0(Programa,Vehiculo)) %>% 
  select(Programa,Vehiculo,concat,ayer,semana,mes)


#MA para carry over
programas = horarios$Programa %>% unique()
ma_carry1 = c()
ma_carry2 = c()

for(i in programas){
  ma_carry1 = ma_carry1 %>% append(carry_MMT %>% filter(Programa == i) %>% mutate(semana = week(Fecha),year = year(Fecha)) %>% group_by(year,semana) %>% summarise(CO = mean(CO)) %>% 
    tail(media_movil) %>% .$CO %>% mean())
  
  ma_carry2 = ma_carry2 %>% append(carry_MMT %>% filter(Programa == i) %>% mutate(mes = month(Fecha),year = year(Fecha)) %>% group_by(year,mes) %>% summarise(CO = mean(CO)) %>% 
    tail(media_movil) %>% .$CO %>% mean())
}

resumen_ma_carry = data.frame(Programa = programas,MA1 = ma_carry1,MA2 = ma_carry2)

#correr las predicciones solo si ya pasaron 3 meses
#cambiar de vuelta a 1:20
#leer la info historica

path_historico = "C:/Users/mario.vidana/Documents/dashboards/"

#Ejemplo----
# historicolv = read.csv(paste0(path_historico,"LV_Historico.txt"),sep = ";")
# 
# historicosd = read.csv(paste0(path_historico,"SD_Historico.txt"),sep = ";")
# historicolv = historicolv[-1,]
# historicolv = historicolv[,-1]
# colnames(historicolv) = c("Fecha","Programa","Vehiculo","Rating_miles","Rating_porc","ATS","Reach","na1","na2","na3","na4")
# historicolv = historicolv %>% select("Fecha","Programa","Vehiculo","Rating_miles")
# historicolv$Fecha = historicolv$Fecha %>% as.Date("%d/%m/%Y")
# historicolv$Rating_miles = as.double(gsub(",","",historicolv$Rating_miles),2)
# 
# historicosd = historicosd[-1,]
# historicosd = historicosd[,-1]
# colnames(historicosd) = c("Fecha","Programa","Vehiculo","Rating_miles","Rating_porc","ATS","Reach","na1","na2","na3","na4")
# historicosd = historicosd %>% select("Fecha","Programa","Vehiculo","Rating_miles")
# historicosd$Fecha = historicosd$Fecha %>% as.Date("%d/%m/%Y")
# historicosd$Rating_miles = as.double(gsub(",","",historicosd$Rating_miles),2)
# 
# historico = historicolv %>% rbind(historicosd) %>% data.frame() %>% arrange(Fecha) %>% filter(Vehiculo == "MULTIMEDIOS TOTAL")
# rownames(historico) = NULL
# 
# saveRDS(historico,paste0(path_historico,"HISTORICO_MM_CDMX.Rda"))

#Descarga historico------

historico = readRDS(paste0(path_historico,"HISTORICO_MM_CDMX.Rda"))
last_historic = historico$Fecha %>% max()
last_info = cdmx_personas$Fecha %>% max()

if(last_historic < last_info){
  add_rows = cdmx_personas %>% filter(Fecha > last_historic,Vehiculo  == "MULTIMEDIOS TOTAL") %>% select(Fecha,Programa,Vehiculo,Rating_miles) %>% arrange(Fecha)
  historico = historico %>% select(Fecha,Programa,Vehiculo,Rating_miles) %>% rbind(add_rows)
  saveRDS(historico,paste0(path_historico,"HISTORICO_MM_CDMX.Rda"))
  
}

hoy = Sys.Date()
proxima_consulta = readRDS(paste0(path_historico,"proxima_consulta.Rda"))

if(hoy >= proxima_consulta){
  
  proxima_consulta = hoy %m+% months(3)
  saveRDS(proxima_consulta,paste0(path_historico,"proxima_consulta.Rda"))
  df = data.frame(id = 1)
  
  for(i in programas){
    print(i)
    x = MAPE(historico,i)
    df2 = data.frame(i = x)
    colnames(df2) = c(i)
    df = cbind(df,df2)
    
    print("Termina prediccion")
  }
   prediction_enable = T
   resumen_pred = df
   saveRDS(resumen_pred,paste0(path,"Pronosticos.Rda"))
   saveRDS(resumen_pred,paste0(path,"Pronosticos_beta.Rda"))
}else{
  prediction_enable = F
}




#ultimos 30 dias
treintadias = data.frame(id = 1:30)
acumulado_mes = data.frame(id = 1:12)

for(i in programas){
  treintadias = treintadias %>% cbind(historico %>% filter(Programa == i,Vehiculo == "MULTIMEDIOS TOTAL") %>% tail(30) %>% .$Rating_miles)
  acumulado_mes = acumulado_mes %>% cbind(historico %>% filter(Programa == i,Vehiculo == "MULTIMEDIOS TOTAL") %>% 
    mutate(Mes = month(Fecha), year = year(Fecha)) %>% group_by(year,Mes) %>% summarise(Rating_miles = mean(Rating_miles)) %>% head(-1) %>% tail(12) %>% 
      .$Rating_miles)
    
}
colnames(treintadias) = c("ID",programas)
meses_graph = historico  %>% mutate(Mes = month(Fecha), year = year(Fecha)) %>% group_by(year,Mes) %>% summarise(Rating_miles = mean(Rating_miles)) %>% 
  head(-1) %>% tail(12) %>% .$Mes
acumulado_mes = acumulado_mes %>% cbind(meses_graph)
colnames(acumulado_mes) = c("ID",programas,"Mes")
acumulado_mes = acumulado_mes %>% select(ID,Mes,programas)
#dar formato a cada df------
resumen_comp = resumen_comp %>% mutate(Pro_canal = paste0(Programas,Canal)) %>% select(Programas,Canal,Pro_canal,Rating_ayer,Rating_semana,Rating_mes,
                                                                                       ATS_ayer,ATS_Semana,ATS_Mes)

#poner los ultimas 12 meses en la pestaña de predicciones.
nombre_programa = c()
nombre_programa = c("id")
eightweeks = data.frame(id = 1:12)
for(i in programas){
  nombre_programa = nombre_programa %>% append(i)
  eightweeks = eightweeks %>% cbind(historico %>% mutate(mes = month(Fecha),year = year(Fecha)) %>% group_by(year,mes,Programa) %>% summarise(Rating_miles = mean(Rating_miles)) %>% 
                                      filter(Programa == i) %>% head(-1) %>% tail(12) %>% .$Rating_miles)
  
}
colnames(eightweeks) = nombre_programa


#Mover la semana de la prediccion correspondiente al prediction.
pronosticos = readRDS(paste0(path,"Pronosticos.Rda"))

if(prediction_enable){
  pronosticos = pronosticos[13:nrow(pronosticos),]
}else{
  rownames(pronosticos) = NULL
  ultimo_mes = historico %>% mutate(mes = month(Fecha),year = year(Fecha)) %>% group_by(year,mes,Programa) %>% 
    summarise(Rating_miles = mean(Rating_miles)) %>% ungroup(year,mes,Programa) %>% filter(year == max(year)) %>% 
    .$mes %>% max()
  mes_registro = readRDS(paste0(path,"ultimo mes.Rda")) 
  saveRDS(ultimo_mes,paste0(path,"ultimo mes.Rda"))
  if(mes_registro < ultimo_mes){
    pronosticos = pronosticos[-13,]
    saveRDS(pronosticos,paste0(path,"Pronosticos.Rda"))
  }
  pronosticos = pronosticos[13:nrow(pronosticos),]
}


#comparar ultimo historico vs pronostico si es menor que pronostico usar pronostico y viceversa
primer_pronostico = pronosticos[1,] %>% gather("Programa","Rating_miles",2:ncol(pronosticos[1,]))
last_month = historico %>% mutate(mes = month(Fecha),year = year(Fecha)) %>% group_by(mes,year,Programa) %>% 
  summarise(Rating_miles = mean(Rating_miles)) %>% ungroup(mes,year,Programa) %>% 
  filter(year == max(year)) %>% filter(mes == ifelse(max(mes)!=1,max(mes)-1,52))

maximos = primer_pronostico %>% left_join(last_month %>% select("Programa","Rating_miles"),by = c("Programa")) %>% 
  group_by(Programa) %>% 
  mutate(Mayor = max(Rating_miles.x,Rating_miles.y)) %>% select(Programa,Mayor) %>% 
  spread(Programa,Mayor)

max2 = maximos %>% rbind(pronosticos[,!names(pronosticos) %in% c("id")] %>% tail(-1))

max2 = max2 %>% mutate(id = 1:nrow(max2)) %>% select(id,programas) %>% data.frame()
colnames(max2) = c("id",programas)


#juntar resumen_pred
resumen_pred = eightweeks %>% rbind(max2)
rownames(resumen_pred)  = NULL



#correr dependiendo si se corre la amcro de predicciones y solo la de los otros datos


dfs = list(resumen,resumen_pred,resumen_share,resumen_comp,resumen_carry,resumen_mas,resumen_ma_carry,treintadias,acumulado_mes)
openxlsx::write.xlsx(dfs,"C:/Users/mario.vidana/Documents/tablas_dash.xlsx")
shell.exec("C:/Users/mario.vidana/Documents/MACROS/dashboard/Dashboard.vbs")
Sys.sleep(7)



