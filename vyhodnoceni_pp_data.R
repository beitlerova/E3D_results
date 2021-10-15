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

#sečte pro každý krok hodnotu jednoho ID (je-li UP definován více pixely jednotného ID) - - MENI UZIVATEL nazev varianty
N10 <- N10 %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                          Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                          ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                          ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N10_udol <- N10_udol %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N10_svod <- N10_svod %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N10_venp <- N10_venp %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N50 <- N50 %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                          Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                          ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                          ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N50_udol <- N50_udol %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N50_svod <- N50_svod %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 
N50_venp <- N50_venp %>% ungroup() %>% group_by(Time, ID) %>% count(Sedbudget=sum(Sedbudget),Runoff=sum(Runoff),Sedvol=sum(Sedvol),Sedconc=sum(Sedconc),
                                                                    Clay=sum(Clay),Silt=sum(Silt),Totero=sum(Totero),Totdep=sum(Totdep),Netero=sum(Netero),
                                                                    ChRunoff=sum(ChRunoff),ChSedvol=sum(ChSedvol),ChNetEro=sum(ChNetEro),
                                                                    ChClay=sum(ChClay),ChSilt=sum(ChSilt)) 

### nazvy variant - MENI UZIVATEL

varianty <- list(N10, N10_venp, N10_udol, N10_svod, N50, N50_venp, N50_udol, N50_svod )
names(varianty) <- c("N10", "N10_venp", "N10_udol", "N10_svod", "N50", "N50_venp", "N50_udol", "N50_svod")
 
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

write.csv(dfSumQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumQ3.csv"))
write.csv(dfMaxQ,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_maxQ3.csv"))
write.csv(dfSumS,paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/LUBY_priradovka/E3D/Vystupy/results/", "vysledky_pp_sumSed3.csv"))

### vytvoří grafy průtoků
#v jednom grafu je 1 varianta a všechny UP

for (i in 1:length(varianty)) {
  png(file=paste0("u:/Vyzkum/17318_NAZV_E3D/3_zpracovani/METODIKA/obrazky/LUBY_graf_prutoku",i,".png"))
  #generovani prutoku - rada prutoku v m3*s-1 za kazdy krok (nekumulativne)
  Prutoky <- varianty[[i]] %>% 
    group_by(ID) %>% mutate(Q = lag(lead(Runoff,1) - Runoff), Q_ms = Q * rozliseni / 5 / 60) %>% 
    select(Time, ID, Q_ms)
  
  #definice grafu
  Prutokova_rada <- Prutoky[Prutoky$ID==1,]
  plot(seq(0,(length(Prutokova_rada$Time)*5)-5,5), as.vector(Prutokova_rada$Q_ms),col="white", xlab="čas [min]", type="l", ylab=expression('průtok (m'^3*'s'^-1*')'), ylim=c(0,ceiling(max(dfMaxQ[,i]))))
  
  #generovani lajn pro jednotliva ID
  for (j in 1:pp) {
    Prutokova_rada <- Prutoky[Prutoky$ID==j,] #rada prutoku jen pro jedno ID
    lines(seq(0,(length(Prutokova_rada$Time)*5)-5,5), Prutokova_rada$Q_ms, col=j) #pridani rady do grafu
  }
  legend("topright",legend=c("UP1 - kukuřice/tráva", "UP2 - tráva/intravilán", "UP3 - průleh"), col = c("black","red","green"), lty=c(1,1,1),)
  #ulozeni grafu
  dev.off()
}



