# Functions to be sourced from uberon mapping



# To lower and remove space at beginning od strings
embellecer <- function(vector_values) {
        tolower(gsub(pattern = "^ ", replacement ="", x = vector_values))
}



# Look up for loosely mathcing values
loose_lookup <- function(values_and, exact=TRUE, syn=TRUE) {
        values_and <- str_split(values_and, " ")[[1]]
        values_and <- embellecer(values_and)
        
        ##### IF SYN = TRUE
       if (syn==TRUE) {
               values_pool <- c(df$label, df$synonym)
       } else if  (syn==FALSE) {
               values_pool <-df$label
       }
        
        ##### IF EXACCT = FALSE
if (exact==FALSE) {
        # one value
        if (length(values_and)==1) {   
                
                res <- values_pool[agrep(pattern = values_and, x = values_pool)]
        }   
        # two values 
        if (length(values_and)==2) {   

        res <- intersect(values_pool[agrep(pattern = values_and[1], x = values_pool)],
                         values_pool[agrep(pattern = values_and[2], x = values_pool)])
      
        }            
       
        # three values
        if (length(values_and)==3) {   # two values only
        # intersect bt 2
        res1 <- intersect(values_pool[agrep(pattern = values_and[1], x = values_pool)],
                          values_pool[agrep(pattern = values_and[2], x = values_pool)])
        res2 <- intersect(values_pool[agrep(pattern = values_and[1], x = values_pool)],
                          values_pool[agrep(pattern = values_and[3], x = values_pool)])
        res3 <- intersect(values_pool[agrep(pattern = values_and[2], x = values_pool)],
                          values_pool[agrep(pattern = values_and[3], x = values_pool)])
        dos <- unique(c(res1, res2, res3))
        # intersect among all 3
        res4 <- Reduce(intersect, list(values_pool[agrep(pattern = values_and[1], x = values_pool)],
                                       values_pool[agrep(pattern = values_and[2], x = values_pool)],
                                       values_pool[agrep(pattern = values_and[3], x = values_pool)])
                                 )
        
        res <- list(tres=res4[!(is.na(res4))], dos=res1[!(is.na(res1))], dos=res2[!(is.na(res2))], dos=res3[!(is.na(res3))])
        
        }
   
        
}     
        
        ## IF EXACT = TRUE
if (exact==TRUE) {
        # one value
        if (length(values_and)==1) {   
                
                res <- df$label[str_detect(df$label, values_and)]
        }   
        # two values 
        if (length(values_and)==2) {   
                
                res <- intersect(values_pool[str_detect(values_pool, values_and[1])],
                                 values_pool[str_detect(df$label, values_and[2])])
                
        }            
        
        # three values
        if (length(values_and)==3) {   # two values only
                # intersect bt 2
                res1 <- intersect(values_pool[str_detect(values_pool, values_and[1])],
                                  values_pool[str_detect(values_pool, values_and[2])])
                res2 <- intersect(values_pool[str_detect(values_pool, values_and[1])],
                                  values_pool[str_detect(values_pool, values_and[3])])
                res3 <- intersect(values_pool[str_detect(values_pool, values_and[2])],
                                  values_pool[str_detect(values_pool, values_and[3])])
                dos <- unique(c(res1, res2, res3))
                # intersect among all 3
                res4 <- Reduce(intersect, list(values_pool[str_detect(values_pool, values_and[1])],
                                               values_pool[str_detect(values_pool, values_and[2])],
                                               values_pool[str_detect(values_pool, values_and[3])]
                ))
                
                res <- list(tres=res4[!(is.na(res4))], dos=res1[!(is.na(res1))], dos=res2[!(is.na(res2))], dos=res3[!(is.na(res3))])
                
        }
        res <- res[!(is.na(res))]
        
}    
        res
}


"\"medial antecubital vein\" RELATED [http://en.wikipedia.org/wiki/Median_cubital_vein]"

syn_to_label <- function(syn) {
  x<- df[which(df$synonym==as.character(syn)),]    
  x
}

# Harmonize values per categories funs


# missing: asign to global environment
harmonize_blood() <- function() {
        ## BLOOD AND DERIVATES
map_all$curated[which(map_all$original %in% pbmc | map_all$curated %in% pbmc)] <-  "peripheral blood mononuclear cell"
map_all$curated[which(map_all$original %in% peripheral_blood | map_all$curated %in% peripheral_blood)] <-  "peripheral blood" # here
map_all$curated[which(map_all$original %in% whole_blood | map_all$curated %in% whole_blood)] <-  "whole blood"
map_all$curated[which(map_all$original %in% plasma | map_all$curated %in% plasma)] <-  "blood plasma"
map_all$curated[which(map_all$original %in% cord_blood | map_all$curated %in% cord_blood)] <-  "umbilical cord blood"
map_all$curated[which(map_all$original %in% serum | map_all$curated %in% serum)] <- "blood serum"
# # unmmapped        
# "peripheral blood"   
#  "dna from blood" 
# "whole blood" 
# "blood granulocytes"
# "human blood"                       
# "nf1-blood"                         
# "blood cd19+" 
}

harmonize_brain() <- function() {
        ## BRAIN 
        map_all$curated[which(map_all$curated=="posterior fossa")] <-   "posterior cranial fossa"
        map_all$curated[which(map_all$curated== "hippocampus")] <- "amygdalohippocampal area"  # here
        # map_all$curated[which(map_all$curated=="thalamus")] <-  "ventral thalamus" # here or "epithalamus"
        map_all$curated[which(map_all$curated=="bithalamic - right thalamus" )] <-  "right dorsal thalamus"
        #map_all$curated[which(map_all$curated=="right cortex")] <-   #  here see paper "right auditory cortex" or "temporal cortex" (syn "cortex of temporal lobe" )
        map_all$curated[which(map_all$curated=="temporo-parietal lobe")] <-  c("temporal lobe", "parietal lobe")
        map_all$curated[which(map_all$curated=="left fronto-parietal lobe")] <-   c("left frontal lobe", "left parietal lobe")
        map_all$curated[which(map_all$curated=="brain-frontal lobe-left" )] <-   "left frontal lobe"
        map_all$curated[which(map_all$curated=="brain-frontal lobe-right" )] <-   "right frontal lobe"
        map_all$curated[which(map_all$curated=="brain-cerebellum"  )] <-   "cerebellum" 
        map_all$curated[which(map_all$curated=="hemispheric")] <-   "obsolete hemispheric parts of the cerebellar cortex" # ("EGAD00001003762 " "EGAD00001004036 " "EGAD00001004070 " "EGAD00001004116 " "EGAD00001005131 ")
        map_all$curated[which(map_all$curated=="pineal")] <-   "obsolete pineal complex"
        map_all$curated[which(map_all$curated=="fronto-temporal lobe")] <-  c("frontal lobe", "temporal lobe") 
        map_all$curated[which(map_all$curated=="right parietal")] <-  "right parietal lobe"
        map_all$curated[which(map_all$curated=="right temporal")] <- "right temporal lobe"
        map_all$curated[which(map_all$curated=="brain-temporal lobe" )] <- "temporal lobe" 
        map_all$curated[which(map_all$curated=="brain-right temporal")] <- "right temporal lobe"
        # map_all$curated[which(map_all$curated=="left cortex")] <-  #  here see paper "left auditory cortex" or "temporal cortex" (syn "cortex of temporal lobe" )
        map_all$curated[which(map_all$original %in% spinal_cord | map_all$curated %in% spinal_cord)] <-  "spinal cord"
        map_all$curated[which(map_all$curated=="brainstem (mesencephalon)")] <- "brainstem" 
        map_all$curated[which(map_all$curated== "right thalamus" )] <- "right dorsal thalamus" 
        map_all$curated[which(map_all$curated=="cerebrum")] <- "obsolete cerebrum" 
        map_all$curated[which(map_all$curated=="left temporo-parietal lobe")] <- c("left temporal lobe", "left parietal lobe")
        map_all$curated[which(map_all$curated=="left tempo-parietal lobe")] <- c("left temporal lobe", "left parietal lobe")
        map_all$curated[which(map_all$curated=="right parietal")] <- "right parietal lobe"
        map_all$curated[which(map_all$curated=="left parietal" )] <- "left parietal lobe"
        # map_all$curated[which(map_all$curated=="hypothalamic chiasm" )] <- "optic chiasm"
        map_all$curated[which(map_all$curated== "thalamus/fornix" )] <- "fornix of brain"
        map_all$curated[which(map_all$curated== "frontal right cortex"  )] <-   "right frontal lobe" #here "frontal cortex" or "right auditory cortex"  
        map_all$curated[which(map_all$curated== "left thalamus"  )] <- "left dorsal thalamus" 
        map_all$curated[which(map_all$curated=="right occipital")] <- "right occipital lobe"
        map_all$curated[which(map_all$curated=="right occipetal lobe")] <- "right occipital lobe"
        #map_all$curated[which(map_all$curated== "brain glioblastoma" )]  # here meninges? <- "brain"
        map_all$curated[which(map_all$curated=="brain-forebrain" )] <-  "forebrain"
        map_all$curated[which(map_all$curated=="vermis")] <-  "cerebellar vermis"
        map_all$curated[which(map_all$curated=="whole cortex frontal")] <-  "frontal cortex"
        map_all$curated[which(map_all$curated=="whole cortex occipital")] <-  "occipital cortex"
        map_all$curated[which(map_all$curated=="whole brain" )] <-  "brain"
        map_all$curated[which(map_all$curated=="whole cortex" )] <-  "cortex"
        map_all$curated[which(map_all$curated=="whole cortex parietal")] <-  "parietal cortex"
        map_all$curated[which(map_all$curated=="whole cortex temporal"  )] <-  "temporal cortex"
        map_all$curated[which(map_all$curated=="brain-temporal lobe-left")] <-  "left temporal lobe"
        map_all$curated[which(map_all$curated=="brain-occipetal lobe-right")] <-  "right occipital lobe" 
        map_all$curated[which(map_all$curated=="external capsule")] <-   "external capsule of telencephalon"
        map_all$curated[which(map_all$curated=="interior pons")] <-   "pons" 
        #map_all$curated[which(map_all$curated== "non cortical plate" )] <-  "brain: other" 
        #map_all$curated[which(map_all$curated=="whole head" )] <-  "brain:other"
        #map_all$curated[which(map_all$curated %in% c("human brain",   "homo sapien brain"))] <-  "brain:other"

}
