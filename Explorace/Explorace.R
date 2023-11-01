library(bipartite)
library(readxl)
library(dplyr)
library(rstudioapi)

# Setup a current working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#Load data
path_to_data <- "./Data/Data Kobe 08-04-2021.xlsx"
Data_Kobe_08_04_2021 <- read_excel(path_to_data, sheet = "Combined")
attach(Data_Kobe_08_04_2021)
combined <- Data_Kobe_08_04_2021
attach(combined)

Traits <- read_excel(path_to_data, 
                     sheet = "Pivot traits", range = "A4:L210")
View(Traits)

## Read a new data set and add some column from it to the original dataset
Nectar <- read_excel(path_to_data, 
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
summary(combined)

Traits[sapply(Traits, is.character)] <- lapply(Traits[sapply(Traits, is.character)], as.factor)
summary(Traits)

#Check columns for NAs - to be safe in turning binary to logical
apply(combined, 2, function(x) any(is.na(x)))


#Turn binary data to logical
combined$`include in network analyses` <- as.logical(combined$`include in network analyses`)
combined$`Any contact` <- as.logical(combined$`Any contact`)


combined$`Contact with anthers` <- as.logical(combined$`Contact with anthers`)
combined$`Contact with stigma` <- as.logical(combined$`Contact with stigma`)
combined$Robbing <- as.logical(combined$Robbing)

######
levels(SpCode)[c(1:20)]

sub <- combined %>% filter(SpCode %in% levels(SpCode)[c(1:20)] )
plot(sub$`Insect order`,sub$`Tube length (CM)`, las=2)

dataset <- combined

##TUrn visit data into bipartite network format
web<-frame2webs(dataset, varnames = c("SpCode", "Insect order", "Elev"), type.out = "list", emptylist = TRUE)
websp<-frame2webs(dataset, varnames = c("SpCode", "Final Visitor", "Elev"), type.out = "list", emptylist = TRUE)

##Grid visualization of web
for(i in 1:4) {
  visweb(web[[i]], type="none", prednames=TRUE, preynames=TRUE, labsize=8, plotsize = 12, 
         square="interactions", text="interactions", frame=NULL, textsize=4, textcol="red", 
         pred.lablength=NULL, prey.lablength=NULL, clear=TRUE, xlabel="Insect Order", ylabel="Plant species", 
         boxes=TRUE, circles=FALSE, circle.col="black", circle.min=0.2, circle.max=2, 
         outerbox.border="white", outerbox.col="white", box.border="black", box.col="black", 
         def.col="blue", max.digits=4, NA.col="red")
}

for(i in 1:4) {
  visweb(websp[[i]], type="none", prednames=TRUE, preynames=TRUE, labsize=8, plotsize = 12, 
         square="interactions", text="interactions", frame=NULL, textsize=4, textcol="red", 
         pred.lablength=NULL, prey.lablength=NULL, clear=TRUE, xlabel="Insect Species", ylabel="Plant species", 
         boxes=TRUE, circles=FALSE, circle.col="black", circle.min=0.2, circle.max=2, 
         outerbox.border="white", outerbox.col="white", box.border="black", box.col="black", 
         def.col="blue", max.digits=4, NA.col="red")
}

##Plot of web
for(i in 1:4) {
  plotweb(web[[i]], method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
          y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
          col.interaction="yellow", col.high = "grey10", col.low="grey10", bor.col.interaction ="grey20", 
          bor.col.high="black", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
          low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
          bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
          adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
          high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
}

for(i in 1:4) {
  plotweb(websp[[i]], method = "cca", empty = TRUE, labsize = 1, ybig = 1, y.width.low = 0.1, 
          y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", 
          col.interaction="yellow", col.high = "grey10", col.low="grey10", bor.col.interaction ="grey20", 
          bor.col.high="black", bor.col.low="black", high.lablength = NULL, low.lablength = NULL, sequence=NULL, 
          low.abun=NULL, low.abun.col="green", bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red", 
          bor.high.abun.col="black", text.rot=90, text.high.col="black", text.low.col="black", adj.high=NULL, 
          adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE, 
          high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="additional")
}


##Calculate network indices for individual webs
##Insect order level
indices_w1<-specieslevel(web[[1]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                         PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices_w2<-specieslevel(web[[2]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                         PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices_w3<-specieslevel(web[[3]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                         PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices_w4<-specieslevel(web[[4]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                         PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
##Insect species level
indices.sp_w1<-specieslevel(websp[[1]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                            PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices.sp_w2<-specieslevel(websp[[2]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                            PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices.sp_w3<-specieslevel(websp[[3]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                            PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

indices.sp_w4<-specieslevel(websp[[4]], index="ALLBUTD", level="lower", logbase=exp(1), low.abun=NULL, high.abun=NULL, PDI.normalise=TRUE, 
                            PSI.beta=c(1,0), nested.method="NODF", nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)


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

##func execution
Traits_and_indices<-Append_Dataframe(indices_w1, Traits, 1, TRUE, 12)
Traits_and_indices<-Append_Dataframe(indices_w2, Traits_and_indices, 1, FALSE, 12)
Traits_and_indices<-Append_Dataframe(indices_w3, Traits_and_indices, 1, FALSE, 12)
Traits_and_indices<-Append_Dataframe(indices_w4, Traits_and_indices, 1, FALSE, 12)

attach(Traits_and_indices)
summary(Traits_and_indices)

TaI<-Traits_and_indices[complete.cases(Traits_and_indices[ , c(11,32)]),]
attach(TaI)

`Average of Final size`

plot(`Average of Final size`, TaI$d)
abline(lm(TaI$d~TaI$`Average of Final size`))

plot(`Average of Tube length (CM)`, TaI$d)
abline(lm(TaI$d~`Average of Tube length (CM)`))

plot(Symmetry, TaI$d)
plot(`Flower position`, TaI$d)

summary(lm(TaI$d~`Anther position`))

pairs(TaI)
pairs(Traits)
