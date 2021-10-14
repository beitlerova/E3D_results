########################## vyhodnoceni pp_data.csv #########################

#vytvoreno pro automaticke vyhodnoceni a srovnani tabelarnich vysledku E3D modelovanych variant v ramci jedne erozni studie
#vyhodnocuje max pr?tok (m3/s), celkov? objem odtoku (m3) a celkovou hmotnost sedimentu (t)

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

varianty <- list(N10, N10_venp, N10_udol, N10_svod, N50, N50_venp, N50_udol, N50_svod )
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
  sumS <- sumS * rozliseni / 1000 #prepocet na t
  dfSumS[1:length(sumS),i] <- sumS
    }

### ulozeni vysledku - MENI UZIVATEL

write.csv(dfSumQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumQ2.csv"))
write.csv(dfMaxQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_maxQ2.csv"))
write.csv(dfSumS,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumSed2.csv"))

### vytvoří graf průtoků

for (i in 1:length(varianty)) {
  maxQ <- varianty[[i]] %>% group_by(ID) %>% mutate(Q = lead(Runoff,1) - Runoff) %>% count(max(Q,na.rm=TRUE))
  maxQ <- maxQ$`max(Q, na.rm = TRUE)`
  maxQ <- maxQ * rozliseni / 5 / 60 #prepocet ma m3/s
  dfMaxQ[1:length(maxQ),i] <- maxQ
    }

N10_sum <- N10 %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                        Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                        ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                        ChClay=sum(ChClay),ChSilt=sum(ChSilt),Q_Cell=sum(Q_Cell)) #sečte pro každý krok hodnotu jednoho ID (je-li UP definován více pixely jednotného ID)


Q <- N10_sum %>% group_by(ID) %>% mutate(Q = lag(lead(Runoff,1) - Runoff), Q_ms = Q * rozliseni / 5 / 60) #výpočet Runoff pro každý krok ne kumulativně

Qpp <- Q[Q$ID==1,]
plot(Qpp$Time, Qpp$Q_ms,col="white")

for (i in 1:pp) {
  Qpp <- Q[Q$ID==i,]
  lines(Qpp$Time, Qpp$Q_ms, col=i)
}




