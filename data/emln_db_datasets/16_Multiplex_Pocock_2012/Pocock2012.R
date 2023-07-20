source('~/Dropbox/RESEARCH/PROJECTS/PRP_Modularity_ECMLN/Datasets/rmangal_setup.R')

# API contact
#api=mangalapi(usr='shainova',key = '109b754be067292d7b9e0998de9cb16aaef1699c')
api=mangalapi(url = 'http://localhost:8000', usr='test',key = '9d00823baa5be60d788d079143d9785a4ffd3eec')
api=mangalapi(url = '45.33.87.103', usr='Shai',key = 'c03963694b247923e032a6776c1b3189c98307b0')

setwd('~/Dropbox/RESEARCH/EMLN_Datasets/Pocock2012/')

################### Pocock2012 ################### 
dataurl=list(addReference(api,list(doi='10.5061/dryad.3s36r118',url="http://dx.doi.org/10.5061/dryad.3s36r118")))

D <- read.csv("./Pocock2012_norwood.csv", h=T, stringsAsFactors = F)
latitude=51.3128
longitude=2.3206

#  Upload taxa
taxa <- unique(c(D$lower.taxon, D$upper.taxon))
taxa <- prepareTaxaNames(taxa)
taxa <- unique(taxa)
obj_taxa <- upload_taxa(taxa)

# Define interaction types
linktypes=unique(D[,c('lower.guild','upper.guild')])
linktypes$link_type=c('PLN_FVS','PLN_APH','PLN_PAP','PLN_SAP','PLN_LMP','PLN_SFI','PLN_SFB','PLN_SFR','PLN_BFL','PLN_SFP','PLN_REP','APH_PAP','APH_SAP','SFI_SFP','SFR_REP')


interactions <- data.frame(from=as.character(D$upper.guild),
                           to=as.character(D$lower.guild),
                           strength_f=D$estimated.interaction.strength,
                           link_type="mutualistic symbiosis",
                           obs_type = "observation",
                           latitude=latitude,
                           longitude=longitude,
                           description=with(D,paste('interaction between',lower.guild,'and',upper.guild)),
                           date=as.Date(D$Date, "%m/%d/%Y"))
#  Upload taxa
taxa <- unique(taxa)
taxa <- prepareTaxaNames(taxa)
taxa <- unique(taxa)
obj_taxa <- upload_taxa(taxa)


#  Taxa NCBI info
# uid = get_uid(taxa)
# names(uid) = taxa
# for(i in 1:length(uid)) {
#   print(i)
#   if(!is.na(uid[i])) {
#     obj_taxa[[names(uid)[i]]]$ncbi <- uid[i]
#     obj_taxa[[names(uid)[i]]] <- patchTaxa(api, obj_taxa[[names(uid)[i]]])
#   }
# }


for (l in 1:nrow(sites)){
  D <- read.csv(sites$file[l])
  rownames(D) <- prepareTaxaNames(rownames(D))
  colnames(D) <- prepareTaxaNames(colnames(D))
  all(colnames(D) %in% taxa)
  print(paste(l,':',nrow(D),'plants',ncol(D),'pollinators','taxa:',all(rownames(D) %in% taxa),all(colnames(D) %in% taxa)))
  
  from <- to <- strength_f <- strength_t <-  NULL
  for (i in 1:nrow(D)) {
    for (j in 1:ncol(D)) {
      if(D[i,j] > 0) {
        from <- c(from, colnames(D)[j])
        to <- c(to, rownames(D)[i])
        strength_f <- c(strength_f, D[i,j])
        strength_t <- c(strength_t, D[i,j])
      }
    }
  }
  latitude=sites$latitude[l]
  longitude=sites$longitude[l]
  # Make a list of interactions
  interactions <- data.frame(from=from,
                             to=to,
                             strength_f=strength_f,
                             strength_t=strength_t,
                             link_type="mutualistic symbiosis",
                             obs_type = "observation",
                             latitude=latitude,
                             longitude=longitude,
                             date=as.Date('2010-02-01','%Y-%m-%d'),
                             description='Sampling was done between 16 January and 28 March in 2010, so I just set the date somewhere in the middle'
                             )
  
  # Upload interactions
  # whatIs(api,'interaction')
  obj_int <- list()
  for(i in 1:nrow(interactions)) {
    int <- interactions[i,]
    print(paste(i,'of',nrow(interactions),'interactions added'))
    temp_int <- list(
      description=int$description,
      date=int$date,
      data=dataurl,
      # papers=papers,
      taxa_from = obj_taxa[[as.character(int$from)]]$id,
      taxa_to = obj_taxa[[as.character(int$to)]]$id,
      strength_f=int$strength_f,
      strength_t=int$strength_t,
      # units_f='',
      # units_t='',
      obs_type = as.character(int$obs_type),
      link_type = as.character(int$link_type),
      latitude=as.character(int$latitude),
      longitude=as.character(int$longitude),
      public=F
    )
    obj_int[[i]] <- addInteraction(api, temp_int)
    cat(obj_int[[i]]$taxa_from, "\t", obj_int[[i]]$taxa_to, "\n")
  }
  
  # Create network
  assign(str_replace_all(paste('Trojelsgaard2015_network',sites$sites[l],sep='_'),' ','_'), addNetwork(api, list(
    name=paste('Trojelsgaard2015',sites$sites[l],sep='_'),
    interactions=obj_int,
    latitude=latitude,
    longitude=longitude,
    public=F))
  )
}


# Create a data set

networks=list()
for (l in ls(pattern='Trojelsgaard2015_network')){
  network=get(l)
  networks[[l]]=network
}

Trojelsgaard2015_DS <- addDataset(api, list(
  name="Trojelsgaard2015",
  description='',
  networks=networks,
  data=dataurl)
)

listDataset(api)

