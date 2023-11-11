```{r warning=FALSE}
library(bipartite)
library(readxl)
library(dplyr)
library(data.table)

```

#Load data
```{r message=FALSE, warning=FALSE}
setwd("~/Desktop/Rko/Diplomka")
Data_Kobe_08_04_2021 <- read_excel("Data Kobe 08-04-2021.xlsx", sheet = "Combined")
attach(Data_Kobe_08_04_2021)
combined <- Data_Kobe_08_04_2021
attach(combined)
levels(as.factor(`Functional group`))
Traits <- read_excel("Data Kobe 08-04-2021.xlsx", 
                     sheet = "Pivot traits", range = "A4:L210")
Nectar <- read_excel("Data Kobe 08-04-2021.xlsx", 
                     sheet = "Combined", col_types = c("skip", 
                                                       "skip", "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", "skip", 
                                                       "skip", "skip", "skip", "skip", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"))
x <- combined
x[colnames(Nectar[,c(seq(3,8))])] <- Nectar[,c(seq(3,8))]
combined2 <- x #Use with caution
#Change character columns to factors
combined[sapply(combined, is.character)] <- lapply(combined[sapply(combined, is.character)], as.factor)
Traits[sapply(Traits, is.character)] <- lapply(Traits[sapply(Traits, is.character)], as.factor)
attach(Traits)
#Check columns for NAs - to be safe in turning binary to logical
apply(combined, 2, function(x) any(is.na(x)))
#Turn binary data to logical
combined$`include in network analyses` <- as.logical(combined$`include in network analyses`)
combined$`Any contact` <- as.logical(combined$`Any contact`)
combined$`Contact with anthers` <- as.logical(combined$`Contact with anthers`)
combined$`Contact with stigma` <- as.logical(combined$`Contact with stigma`)
combined$Robbing <- as.logical(combined$Robbing)

#Get nectar per species into the traits dataset
nec <- read_excel("Data Kobe 08-04-2021.xlsx", sheet = "Pivot Nectar")
nec <- nec[seq(1:177),]
nec <- nec[,c(1,3,4,5,6,7,8)]
nec[sapply(nec, is.character)] <- lapply(nec[sapply(nec, is.character)], as.factor)
Traits_and_nec <- Traits
for (i in 1:nrow(nec)) {
  spname <- nec$`Row Labels`[i]
  if(any(apply(Traits, 1, function(r) any(r %in% c(spname))))){
    rnumber <- which(apply(Traits, 1, function(r) any(r %in% c(spname))))
    print(rnumber)
    for (i2 in 1:(ncol(nec)-1)){
      Traits_and_nec[rnumber,(12+i2)] <- nec[i,(i2+1)]
    }
  }else{
    print(paste("No species found:", as.character(spname), sep=" "))
  }
}
print("All values from nec dataset were appended to the respective rows in the Traits dataset -> new dataset is Traits_and_nec")

#Create identificator of individual webs - elevation and season
combined$WebID <- paste(substr(combined$Elev,1,4),substr(combined$Exp,1,1), sep="_")

##Create dataset with pollinators that touch any reproductive organ and are important ollinator group
dataset <- combined[`Any contact`==TRUE & `include in network analyses`==TRUE,]
```

##Turn visit data into bipartite network format
```{r}
web<-frame2webs(dataset, varnames = c("SpCode", "Functional group", "WebID"), type.out = "list", emptylist = TRUE)
websp<-frame2webs(dataset, varnames = c("SpCode", "Final Visitor", "WebID"), type.out = "list", emptylist = TRUE)
many <- length(web)
```

##Plot of web
```{r}
par(mfrow=c(2,2))
for(i in 1:many) {
  plotweb(web[[i]], method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
          y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
          col.interaction="yellow", col.high = "grey10", col.low="grey10", bor.col.interaction ="grey20", 
          bor.col.high="black", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
          low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
          bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
          adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
          high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
  title(paste(names(web)[[i]], "- interaction plot", sep=" "))
}
```
##Calculate network indices for individual webs
##Insect order/functional group level
```{r}

for(i in 1:many) {
  assign(paste("indices", levels(as.factor(combined$WebID))[[i]], sep = "_"), specieslevel(web[[i]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                                                                                           PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE))
}

##Insect species level
for(i in 1:many) {
  assign(paste("indices.sp", levels(as.factor(combined$WebID))[[i]], sep = "_"), specieslevel(websp[[i]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                                                                                              PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE))
}

##Add respective columns from indices dataframe to the traits dataset

Append_Dataframe <- function(add_df, app_df, spname_col, create_cols, add_after) {
  targ_df <- app_df
  orig_df <- add_df
  last_col <- add_after
  
  if(create_cols){ 
    for(i in 1:ncol(orig_df)) {
      targ_df$col <- NA
      names(targ_df)[names(targ_df) == 'col'] <- colnames(orig_df)[i]
    }
  }else{
  }
  for(i in 1:nrow(orig_df)) {
    spname <- rownames(orig_df[i,])
    rnumber <- which(targ_df[,spname_col] == spname)
    for(i2 in 1:ncol(orig_df)) {
      targ_df[rnumber,(last_col+i2)] <- orig_df[i,i2]
    }
  }
  return(targ_df)
}
##Append trait data with indices of plant species in individual webs

##alternative to append traits with nectar data with indeces considering insect on species level
Traits_and_indices <- data.frame()
base_df <- Traits_and_nec
web_level <- "indices.sp"
complete_cols <- c(10,15,38)

##alternative to append traits with nectar data
Traits_and_indices <- data.frame()
base_df <- Traits_and_nec
web_level <- "indices"
complete_cols <- c(10,15,38)

for(i in 1:many) {
  assign(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"), Append_Dataframe(get(paste(web_level, levels(as.factor(combined$WebID))[[i]], sep = "_")), base_df, 1, TRUE, ncol(base_df)))
  assign(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"), get(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"))[complete.cases(get(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"))[ ,complete_cols]),])
  ds <- get(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"))
  if (substr(levels(as.factor(combined$WebID))[[i]], 1,4) %in% c("650m")) {
    ds$elev <- 650
  } else {
    ds$elev <- as.numeric(substr(levels(as.factor(combined$WebID))[[i]], 1,4))
  }
  if (substr(levels(as.factor(combined$WebID))[[i]], 6,6) %in% c("D")) {
    ds$dry <- 1
  } else {
    ds$dry <- 0
  }
  assign(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"), ds)
  ds <- get(paste("Ts_and_Is", levels(as.factor(combined$WebID))[[i]], sep = "_"))
  Traits_and_indices <- rbind(Traits_and_indices, ds)
}
attach(Traits_and_indices)
TaI <- Traits_and_indices
attach(TaI)

## Prepare data
exclude_cols <- c("Average of Volume/ per flower", 
                  "Average of concentration (w/w%) Refractometer",
                  "Average of fructose (%)", 
                  "Average of glucose (%)", 
                  "Average of saccharose (%)", 
                  "degree",                                       
                  "normalised.degree",                          
                  "species.strength",                             
                  "interaction.push.pull",                        
                  "nestedrank",                                   
                  "PDI",                                         
                  "species.specificity.index",                    
                  "resource.range",                              
                  "PSI",                                        
                  "node.specialisation.index.NSI",               
                  "betweenness",                                  
                  "weighted.betweenness",                      
                  "weighted.closeness",                          
                  "Fisher.alpha",                                
                  "partner.diversity",                            
                  "effective.partners",                           
                  "proportional.similarity",                      
                  "proportional.generality")
TaI <- TaI[,!colnames(TaI) %in% exclude_cols]
#TaI <- TaI[,c(-13,-14,-16:-29, -30, -32:-37)]
names(TaI)
summary(TaI)

#Open flowers - 1 = Open, dish
levels(TaI$`Shape-upd`)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,3]]  %in% c("Dish", "Open", "Bowl", "Stellate")){
    TaI[i,"openess"]<-1
    print("open flower")
  }
  else{
    TaI[i,"openess"]<-0
    print("closed flower")
  }
}
print("magic over")

##Symetry acti? zygomorph
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,4]]  %in% c("Zygomorphic")){
    TaI[i,"zygomorph"]<-1
    print("zygo flower")
  }
  else{
    TaI[i,"zygomorph"]<-0
    print("actino flower")
  }
}
print("magic over")  

##pendant position
levels(TaI$`Flower position`)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,5]]  %in% c("Upright")){
    TaI[i,"position"]<-0.5
    print("upright flower")
  }
  else{
    if (TaI[[i,5]]  %in% c("Horizontal")){
      TaI[i,"position"]<-1
      print("horizontal flower")
    }
    else{
      TaI[i,"position"]<-0
      print("pendant flower")
    }
  }
}
print("magic over")

##anthers
levels(TaI$`Anther position`)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,6]]  %in% c("Partially exposed")){
    TaI[i,"anthers"]<-0.5
    print("partially exposed anthers")
  }
  else{
    if (TaI[[i,6]]  %in% c("Exposed")){
      TaI[i,"anthers"]<-1
      print("exposed anthers")
    }
    else{
      TaI[i,"anthers"]<-0
      print("hidden anthers")
    }
  }
}
print("magic over")

##odour
levels(TaI$`Odour strength`)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,7]]  %in% c("Moderate")){
    TaI[i,"odour"]<-0.5
    print("moderate smell")
  }
  else{
    if (TaI[[i,7]]  %in% c("Strong")){
      TaI[i,"odour"]<-1
      print("strong smell")
    }
    else{
      TaI[i,"odour"]<-0
      print("weak/no smell")
    }
  }
}
print("magic over")

##Brightness
levels(TaI$Brightness)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,8]]  %in% c("Vivid")){
    TaI[i,"bright"]<-1
    print("bright")
  }
  else{
    TaI[i,"bright"]<-0
    print("drab")
  }
}
print("magic over")

##guides
levels(TaI$`Nectar guides`)
for (i in 1:(nrow(TaI))) {
  if (TaI[[i,10]]  %in% c("Present")){
    TaI[i,"guides"]<-1
    print("guides yes")
  }
  else{
    TaI[i,"guides"]<-0
    print("guides no")
  }
}
print("magic over")

##colour
fcols <- transpose(as.data.frame(col2rgb(c("blue","brown","green","orange", "pink", "purple", "red", "white", "yellow"))))
fcols <- as.data.frame(fcols)
colnames(fcols) <- rownames(col2rgb(c("blue","brown","green","orange", "pink", "purple", "red", "white", "yellow")))
vcols <- c("blue","brown","green","orange", "pink", "purple", "red", "white", "yellow")
for (i in 1:(nrow(TaI))) {
  if (tolower(TaI[[i,9]])  %in% vcols){
    ind <- which(vcols %in% c(tolower(TaI[[i,9]])))
    TaI[i,"R"]<-fcols[[ind,1]]
    TaI[i,"G"]<-fcols[[ind,2]]
    TaI[i,"B"]<-fcols[[ind,3]]
    print(paste(TaI[[i,9]]))
  }
  else{
    print("no color")
  }
}
print("magic over")

## Remove original trait columns
names(TaI)
exclude_cols <- c("Species",
                  "Shape-upd",                                
                  "Symmetry",                                
                  "Flower position",                          
                  "Anther position",                         
                  "Odour strength",                           
                  "Brightness",                              
                  "Colour",                                  
                  "Nectar guides")
TaI <- TaI[,!colnames(TaI) %in% exclude_cols] 
#TaI <- TaI[,-2:-10]
TaI <- as.data.frame(TaI)
rownames(TaI) <- paste(TaI$SPCODE, TaI$elev, TaI$dry, sep="_")
TaI <- TaI[,!colnames(TaI) %in% c("SPCODE")] 
TaI
colnames(TaI)[which(colnames(TaI)=="Average of Final size")] <- "size"
colnames(TaI)[which(colnames(TaI)=="Average of Tube length (CM)")] <- "length"
colnames(TaI)[which(colnames(TaI)=="Average of Sugar amount per flower (mg)2")] <- "sugar"
```
### Multivariátní analýza

```{r}
## Standardizace přes sloupce bez identifikátorů
Tstand = decostand(TaI[,c(-6:-7)],method="stand") 
attach(Tstand)
## Odstranění prediktorů z datasetu
TS <- Tstand[,c(-4:-5)]
## Opětovné přidání identifikátorů
TS$elev <- TaI$elev
TS$dry <- TaI$dry
## Výběr hierarchické kategorie (polinační sítě)
towork <- TS[,!colnames(TS) %in% c("elev","dry")]
towork <- towork[,!colnames(towork) %in% c("hue")]
```
## Nepřímá analýza
## PCA všech kytek ve všech elevacích a sezónách
## Osa 1 a 2
    ## Je vidět, že s první ordinační osou, že koreluje jak délka květní trubky a
    ## celková veliksot květu, tak i živost jeho barev. Na druhé straně je vidět
    ## přístupnost tyčinek a celková otevřenost květu. Tyto klastry jsou kopírovány
    ## i zbarvením květů. Je vidět, že ty mdlé, drobné květy, otevřené stavby jsou
    ## zelené či bílé narozdíl od výrazných velkých květů v pravé části.
  
    ## S druhou ordinační osou pak koreluje orientace květu (vzpřímenost) a
    ## symetrie. Symetrie je také zobrazena v rámci tvaru symbolů. Trojúhelník =
    ## zygomorfní, kolečko = aktinomorfní. Na opačném konci je pak síla vůně květu.
```{r}
par(mfrow=c(1,1))
rdout=rda(towork)
plot(rdout, choices = c(1, 2), type="n")
text(rdout, choices = c(1, 2), disp="species", cex=0.7)
points(rdout, choices = c(1, 2), disp="sites",pch=zygomorph+16,cex=1.3, col = "black")
points(rdout, choices = c(1, 2), disp="sites",pch=zygomorph+16,cex=1.0, col = rgb(TaI$R/255,TaI$G/255,TaI$B/255))
arrows(0,0,scores(rdout)$species["length",1],scores(rdout)$species["length",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["anthers",1],scores(rdout)$species["anthers",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["position",1],scores(rdout)$species["position",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["odour",1],scores(rdout)$species["odour",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["zygomorph",1],scores(rdout)$species["zygomorph",2],lwd=1,length=0.15)

barplot(as.numeric(eigenvals(rdout) / sum(eigenvals(rdout))))
```

## Osa 1 a 3
## S třetí ordinační osou pak koreluje modrá a červená složka barev - to znamená bílou. Zelená je 
```{r}
rdout=rda(towork)
plot(rdout, choices = c(1, 3), type="n")
text(rdout, choices = c(1, 3), disp="species", cex=0.7)
points(rdout, choices = c(1, 3), disp="sites",pch=zygomorph+16,cex=1.3, col = "black")
points(rdout, choices = c(1, 3), disp="sites",pch=zygomorph+16,cex=1.0, col = rgb(TaI$R/255,TaI$G/255,TaI$B/255))
arrows(0,0,scores(rdout, choices = c(1, 3))$species["length",1],scores(rdout, choices = c(1, 3))$species["length",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout, choices = c(1, 3))$species["anthers",1],scores(rdout, choices = c(1, 3))$species["anthers",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout, choices = c(1, 3))$species["B",1],scores(rdout, choices = c(1, 3))$species["B",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout, choices = c(1, 3))$species["R",1],scores(rdout, choices = c(1, 3))$species["R",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout, choices = c(1, 3))$species["G",1],scores(rdout, choices = c(1, 3))$species["G",2],lwd=1,length=0.15)


##PCA jen RGB
RGB <- towork[,c(11,12,13)]
rdout =rda(RGB)
plot(rdout, choices = c(1, 2), type="none")
text(rdout, disp="species", cex=0.7)
points(rdout, disp="sites",pch=16,cex=1.2, col = "black")
points(rdout, disp="sites",pch=16,cex=1, col = rgb(TaI$R/255,TaI$G/255,TaI$B/255))
arrows(0,0,scores(rdout)$species["R",1],scores(rdout)$species["R",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["G",1],scores(rdout)$species["G",2],lwd=1,length=0.15)
arrows(0,0,scores(rdout)$species["B",1],scores(rdout)$species["B",2],lwd=1,length=0.15)

barplot(as.numeric(eigenvals(rdout) / sum(eigenvals(rdout))))
```

## Přímá analýza
## RDA všech kytek ve všech elevacích a sezónách
## Test vlivu ůrovně specializace (lisi se květní traits s úrovní specializace polinačního systému?)
#    - elevations je třeba ponechat jako celky pohromadě
#    - seasons je potřeba také jako celky pohromadě 
#    - Volně permutovat unvitř elevations a sezóny. Tzn., že je 8 samostatných celků.
```{r}
fE <- as.factor(TaI$elev)
fD <- as.factor(TaI$dry)
rdout=rda(towork~Tstand$d+Condition(fE+fD))
plot(rdout,type="n",display=c("sp"),col="white", xlim=c(-3,5), ylim=c(-3,3))
points(rdout, disp="sites",pch=1,cex=1+TaI$d*1.2, col = rgb(0,0,0, alpha = 0.2))
points(rdout, disp="sites",pch=16,cex=0.8+TaI$d*1.2, col = rgb(TaI$R/255,TaI$G/255,TaI$B/255, alpha = 0.4))
text(rdout, disp="species", cex=0.7, bg="white")
text(scores(rdout)$biplot[1]+0.5, 0, "specialization", disp="cn", cex=0.7)
arrows(0,0,scores(rdout, choices = c(1, 2))$biplot[1],scores(rdout, choices = c(1, 2))$biplot[2],lwd=1, length=0.15)
scores(rdout)

```
## Statistický test
```{r}
rdout=rda(towork~Tstand$d+Condition(fE+fD))
perm <- how(nperm = 500, blocks=fE, plots=Plots(strata=fD,type="none"),within=Within(type="free"))
anova(rdout,by="terms",permutations=perm) 
```

## Vysvětlená variabilita
```{r}
par(mfrow=c(1,2))
barplot(as.numeric(eigenvals(rdout)))
xx=varpart(towork,~Tstand$d,~fE+fD,data=towork, transfo="stand")
xx
plot(xx)
```
##Automatic prinitng
```{r}
elevs <- c(650, 650, 1100, 1100, 1450, 1450, 2250, 2250)
seans <- c(1,0,1,0,1,0,1,0)
par(mfrow=c(2,2))
for (i in 1:length(elevs)) {
  ##Data prep
  towork <- TS[which(TS$elev==elevs[[i]] & TS$dry == seans[[i]]),!colnames(TS) %in% c("elev","dry")]
  towork <- towork[,!colnames(towork) %in% c("hue")]
  ##PCA
  rdout=rda(towork)
  plot(rdout, type="n")
  text(rdout, disp="species", cex=0.7)
  points(rdout, disp="sites",pch=zygomorph+16,cex=1.2, col = "black")
  points(rdout, disp="sites",pch=zygomorph+16,cex=1.0, col = rgb(TaI$R/255,TaI$G/255,TaI$B/255))
  arrows(0,0,scores(rdout)$species["length",1],scores(rdout)$species["length",2],lwd=1,length=0.15)
  arrows(0,0,scores(rdout)$species["anthers",1],scores(rdout)$species["anthers",2],lwd=1,length=0.15)
  title(main = "PCA", sub = paste(elevs[[i]], "meters a.s.l.", c("Dry", "Wet","Dry", "Wet","Dry", "Wet","Dry", "Wet")[(2-i)], "Season", sep=" "))
  barplot(as.numeric(eigenvals(rdout) / sum(eigenvals(rdout))))
  title(main = "Explained variability per axis", sub = paste(elevs[[i]], "meters a.s.l.", c("Dry", "Wet","Dry", "Wet","Dry", "Wet","Dry", "Wet")[i], "Season", sep=" "))
}
elevs <- c(650, 650, 1100, 1100, 1450, 1450, 2250, 2250)
seans <- c(1,0,1,0,1,0,1,0)
par(mfrow=c(2,2))
for (i in 1:length(elevs)) {
  #Data prep
  towork <- TS[which(TS$elev==elevs[[i]] & TS$dry == seans[[i]]),!colnames(TS) %in% c("elev","dry")]
  towork <- towork[,!colnames(towork) %in% c("hue")]
  #RDA
  rdout = rda(towork~Tstand$d[which(TS$elev==elevs[[i]] & TS$dry == seans[[i]])])
  RsquareAdj(rdout)
  plot(rdout,display=c("sp","cn"))
  title(main = "RDA", sub = paste(elevs[[i]], "meters a.s.l.", c("Dry", "Wet","Dry", "Wet","Dry", "Wet","Dry", "Wet")[i], "Season", sep=" "))
  barplot(as.numeric(eigenvals(rdout)))
  title(main = "Explained variability", sub = paste(elevs[[i]], "meters a.s.l.", c("Dry", "Wet","Dry", "Wet","Dry", "Wet","Dry", "Wet")[i], "Season", sep=" "))
  #ANOVA test
  anova(rdout,by="terms")
}
```



