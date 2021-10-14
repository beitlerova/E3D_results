########################## vyhodnoceni pp_data.csv #########################

#vytvoreno pro automaticke vyhodnoceni a srovnani tabelarnich vysledku E3D modelovanych variant v ramci jedne erozni studie
#vyhodnocuje max prùtok (m3/s), celkový objem odtoku (m3) a celkovou hmotnost sedimentu (t)

###baliky

library(dplyr)

### nacteni pp_data ktera chceme zpracovat - MENI UZIVATEL

N10 <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N10_base/pp_data.csv")
N10_udol <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N10_udolnice/pp_data.csv")
N10_svod <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N10_svod/pp_data.csv")
N10_venp <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N10_VENP/pp_data.csv")
N50 <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N50_base/pp_data.csv")
N50_udol <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N50_udolnice/pp_data.csv")
N50_svod <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N50_svod/pp_data.csv")
N50_venp <- read.csv("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/N50_VENP/pp_data.csv")

### nazvy variant - MENI UZIVATEL

varianty <- list(N10, N10_udol, N10_svod, N10_venp, N50, N50_udol, N50_svod, N50_venp )
names(varianty) <- c("N10", "N10_udol", "N10_svod", "N10_venp", "N50", "N50_udol", "N50_svod", "N50_venp")
 
### udaje o simulaci - MENI UZIVATEL

pp <- 3 # max pocet pour pointu v pp datech
rozliseni <- 3 #rozliseni rastru
interval <- 5 #casovy krok simulace

### priprava df pro zapsani vysledku

dfSumQ <- data.frame(matrix(NA, nrow = pp, ncol = length(varianty)))
names(dfSumQ) <- c(names(varianty))

dfMaxQ <- data.frame(matrix(NA, nrow = pp, ncol = length(varianty)))
names(dfMaxQ) <- c(names(varianty))

dfSumS <- data.frame(matrix(NA, nrow = pp, ncol = length(varianty)))
names(dfSumS) <- c(names(varianty))

### vypocet relevantnich udaju pro vsechny varianty - max Q, sum Q, sum Sed

for (i in 1:length(varianty)) {
  sumQ <- varianty[[i]] %>% group_by(ID) %>% count(max(Runoff))
  sumQ <- sumQ$`max(Runoff)`
  sumQ <- sumQ * rozliseni #prepocet ma m3
  dfSumQ[1:length(sumQ),i] <- sumQ
  
  maxQ <- varianty[[i]] %>% group_by(ID) %>% mutate(Q = lead(Runoff,1) - Runoff) %>% count(max(Q,na.rm=TRUE))
  maxQ <- maxQ$`max(Q, na.rm = TRUE)`
  maxQ <- maxQ * rozliseni / 5 / 60 #prepocet ma m3/s
  dfMaxQ[1:length(maxQ),i] <- maxQ
  
  sumS <- varianty[[i]] %>% group_by(ID) %>% count(max(Sedvol))
  sumS <- sumS$`max(Sedvol)`
  sumS <- sumS * rozliseni / 1000 #prepocet ma t
  dfSumS[1:length(sumS),i] <- sumS
    }

### ulozeni vysledku - MENI UZIVATEL

write.csv(dfSumQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumQ2.csv"))
write.csv(dfMaxQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_maxQ2.csv"))
write.csv(dfSumS,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumSed2.csv"))
