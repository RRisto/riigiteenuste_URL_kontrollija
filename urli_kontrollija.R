library(httr)
library(riigiteenused)
library(dplyr)

andmedLai=andmedSisse()
andmedPikk=andmedPikaks(andmedLai)

#kontrollib urle üskhaaval
urliKontorllija=function(urlid) {
  idx <- 1
  vastused<-sapply(urlid, function(x) 
  {cat("working on url nr",idx, "\n"); idx<<-idx+1;
  tryCatch( http_status(GET(x))$category, 
            error=function(e) 'error')}) %>% as.data.frame
  vastused$url=urlid
  colnames(vastused)[1]="response"

  return(vastused)
}

#kontrolli unikaalseid urle ja siis jagab tulemused algsete peale laiali
#selleks, et töötaks, peab olema veerg, kus urlid on nimeks url
efektiivneUrliKontrollija=function(andmed){
  unikUrlid=unique(andmed$url)
  unikTulemused=urliKontorllija(unikUrlid)
  tulem=merge(andmed, unikTulemused, by.x="url", by.y="url", all=T)
  tulem$algne=NULL
  return(tulem)
}

urlid=efektiivneUrliKontrollija(andmedLai)
erroritega=urlid[grep('error', urlid$response),]

#sama asi õigusaktidega
seadused=setNames(andmedLai$legalBasisUrl, andmedLai$identifier)
seadused <- mapply(`[<-`, seadused, 'service_id', value = names(seadused), SIMPLIFY = FALSE)
oigusaktid=dplyr::rbind_all(seadused)

oigusaktid_urlid=efektiivneUrliKontrollija(oigusaktid)
cols=c("identifier", "name", "provider.name", "provider.memberOf.name")
oigusaktid_urlid=merge(oigusaktid_urlid, 
                       andmedLai[,cols], by.x="service_id", by.y="identifier")

#table(oigusaktid_urlid$response)
erroridSeadused=oigusaktid_urlid[grep('error', oigusaktid_urlid$response),]

#paneme kahest kokku raporti
vigasedKoond=erroridSeadused[, c("service_id", "name", "url", "provider.name", "provider.memberOf.name","title")]
vigasedKoond$veaKirjeldus="vigane viide regulatsioonile"

vigasedKanalid=erroritega[, c("identifier", "name", "url", "provider.name", "provider.memberOf.name")]
vigasedKanalid$title="pole"
vigasedKanalid$veaKirjeldus="vigane viide kanalile"
column_names=c("service_id", "name", "url", "provider.name", "provider.memberOf.name","title", "veaKirjeldus")
colnames(vigasedKanalid)=column_names

#muudame col nimed ja salvestame
vigasedKoond=rbind(vigasedKanalid, vigasedKoond)
names(vigasedKoond)=c("teenused_id", "teenuse_nimi", "vigane_url", "asutus", 
                      "haldusala","õigusakti_pealkiri (olemasolul)", "vea_kirjeldus")
write.table(vigasedKoond, "vigased_koond.csv", sep=";", row.names = F)

#salvestame iga haldusala lõikes, esmalt teeme funktsiooni
salvestaja=function(andmed, grupeerija, kaust) {
  if(!kaust%in%dir()) {
    dir.create(kaust) 
  }
  invisible(lapply(unique(grupeerija), 
                   function(i) write.table(andmed[grupeerija==i,], 
                                           file = paste0("./",kaust, "/vigased_",gsub(" ","_",i), ".csv"), 
                                           sep=";", row.names = FALSE)))
  cat("salvestatud")
}
#salvestame
salvestaja(vigasedKoond, vigasedKoond$haldusala,"haldusalade_loikes")
#asutuste lõikes
salvestaja(vigasedKoond, vigasedKoond$asutus,"asutuste_loikes")



