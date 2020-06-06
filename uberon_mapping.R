source("/Users/claudiavasallovega/repolab/work/uberon_ontology.R")
source("/Users/claudiavasallovega/repolab/work/sample_types.R")
source("/Users/claudiavasallovega/repolab/work/curating_funs.R")
# ////////////////////////////////////////////////////////////////////////////////////////////
# CURATING AND MAPPING ANATOMIC VALUES

main_organs <- c(
        "saliva", "blood", "brain", "spinal_cord",
        "placenta", "lymph_node", "pituitary_gland", 
        "spleen", "thymus", "liver", "kidney",
        "bladder", "pancreas", "bone_marrow", "urinary_bladder",
        "vein", "ureter", "uterus", "endometrium", 
        "ovary", "testis", "breast", "epigastrium",
        "stomach", "esophagus", # "omentum", # "bowel", "digestive_tract", "colon"
        "small_intestine", "large_intestine","rectum", "cecum", "heart", "prostate_gland", "abdominal_wall",
        "peritoneum", "skin", "pleura", "muscle_tissue",
        "adipose_tissue", "thyroid", "eyes", "bone_element", "cartilage_element", "nose",
        "inguen", "thigh", "axila", "mouth", "trophectoderm",
        "adrenal_gland", "tonsil", "ascitic_fluid", "feces", "islet",
        "parotid_gland", "diaphragm"
)

organ_names <- read_delim("~/my_organs.txt", delim = "/n", col_names = F)$X1
organ_names <- c(
        "blood", "brain" ,"spinal_cord" ,"placenta","lymph_node","pituitary_gland","spleen","thymus",            
        "liver","kidney","urinary_bladder","pancreas", "bone_marrow","vein","ureter","cervix"  ,          
        "fallopian_tube","uterus","endometrium","ovary","testis","lung" , "breast","colon" ,            
        "rectum","epigastrium", "stomach", "esophagus", "omentum","duodenum","large_intestine",   
        "small_intestine","colon","vermiform_appendix","intestine","rectum","digestive_tract"   ,
        "heart", "prostate_gland","sperm","abdominal_wall", "peritoneum","skin","cartilage_element" ,
        "mesentery","pleura","muscle_tissue", "adipose_tissue", "thyroid_gland","eye",               
        "pelvic_cavity","bone_element" ,"nose","inguen","thigh","axila","mouth","trophectoderm",   
        "embryo", "adrenal_gland","tonsil","ascitic_fluid","feces","islet","parotid_gland","diaphragm",
        "colorectum"
)
to_fix <- lapply(organ_names, function(o) {
        unique(unmatched[which(unmatched %in% eval(as.name(o)))])  
})

names(to_fix) <-organ_names



###################################################################################################
# Mapping values with Ontology IDs or onrology URLs to mapped column

obo_values <- unique(all_curated[which(str_detect(string=all_curated,pattern = "obolibrary"))]) 
obo_pattern <- c("http://purl.obolibrary.org/obo/")

with_obo_url <- all_curated[which(str_detect(string=all_curated, pattern=obo_pattern))] # 174
nourl_anatomic <- anatomic_site_values[!(embellecer(anatomic_site_values)
                                         %in% with_obo_url)] # 444

length(unique(filter(ph_tags, embellecer(value) %in% with_obo_url)$EGAN))  # 3631


# Map obo url values to actual ontology value by extracting final part and uppercase it
val <- toupper(sapply(obo_values, function(v) {
        gsub(x = gsub(x = str_split(v, pattern = obo_pattern)[[1]][2], pattern = "_", replacement = ":"),
             pattern = ";", replacement = "")
}))
length(val) # 174
sum(val %in% df$id) # 172
noval <- val[!(val%in%df$id)]
noval[which(str_detect(noval, df$id))]
m <- df$id[sapply(df$id, function(id){
        str_detect(string=noval[1], pattern = id)
})]


# with url for ontologies
url_pattern <- c("http")

with_url <- unmathched[which(str_detect(string=unmathched, pattern=url_pattern))] # 

ont_pattern <- "ontobee"
with_ontology <- unmathched[which(str_detect(string=unmathched, pattern=ont_pattern))] # 

nourl_anatomic_site_unmathched <- unmathched[!(unmathched
                                               %in% with_url | unmathched
                                               %in% with_ontology)] # 328


###############################################################################################


# KIDNEY, SPLEEN, THYMUS, PITUITARY GLAND
unique(unmatched[which(unmatched %in% kidney)])
unique(unmatched[which(unmatched %in% spleen)])
unique(unmatched[which(unmatched %in% thymus)])
unique(unmatched[which(unmatched %in% pituitary_gland)])
unique(unmatched[which(unmatched %in% bladder)]) 
unique(unmatched[which(unmatched %in% pancreas)]) 
unique(unmatched[which(unmatched %in% bone_marrow)])  
unique(unmatched[which(unmatched %in% ureter)])  



# grales
lapply(organ_names, function(o){
        map_all$curated[which(map_all$curated 
                              %in% still_unmatched & (map_all$original 
                                                      %in% o | map_all$curated %in% o))] <<- gsub(x=as.character(o),pattern = "_", " ")
})

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% uterus | map_all$curated %in% uterus))] <- "uterus"

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% ureter | map_all$curated %in% ureter))] <- "ureter"

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% pancreas | map_all$curated %in% pancreas))] <- "pancreas"

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% bone_marrow | map_all$curated %in% bone_marrow))] <- "bone marrow"
map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% ovary | map_all$curated %in% ovary))] <- "ovary"

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% prostate | map_all$curated %in% prostate))] <-  "prostate gland" 

map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                                              %in% skin | map_all$curated %in% skin))] <- "zone of skin"


                                              

lapply(organ_names, function(o){
        map_all$curated[which(map_all$curated 
                              %in% still_unmatched & (map_all$original 
                                                      %in% o | map_all$curated %in% o))] <<- gsub(x=as.character(o),pattern = "_", " ")
})









"douglas" >"Douglas' pouch"
"parotis"  > # "parotid gland" or "parotid vein" #idk
filter(ph_tags, value==" parotis")
filter(ph_tags, value==" ventricle" )
"ventricle" > # "cardiac ventricle" or "ventricle of nervous system"
"adrenal"  > #  "adrenal gland" or  "adrenal tissue" 
"ascitis"> "ascitic fluid"
"stool" > "feces" 
"islet" >"islet of Langerhans"
 
"somatomotor cortex"  > "somatomotor neuron" 
"supraclavicular node" > "supraclavicular lymph node"

"periventriclular" > "periventricular zone of hypothalamus"
"ventricle" >  "ventricle of nervous system"

filter(ph_tags, value==" foramen monro")$EGAD
unique(filter(ph_tags, EGAD=="EGAD00001005932 ", tag== " organism_part ")$value)
"diaphragm right side" > "diaphragm" 
filter(ph_tags, value==" diaphragm right side")

# "bladder peritoneum"
"pouch of douglas"  >  "Douglas' pouch"
#"paracolic right" > "paracolic gutter"
"right parotid gland"  > "parotid gland" 

"promontory"  > "promontory of tympanic cavity"

#"obturatorius loge right"   >  "obturator muscle"




# HEART
# map_all$curated[which(map_all$curated=="right ventricular")] <- # "right ventricular compact myocardium"   OR "right ventricular trabecular myocardium" 












# ////////////////////////////////////////////////////////////////////////////////////////////
# Map all anatomic_site_tags_values

values <- unique(anatomic_site_tags_values)
# values <- unique(filter(ph_tags, tag == " brain_part ")$value)
#values <- unique(embellecer(ln))
length(values) # 1192

# Curate 
belli_values <- embellecer(values)
all_curated  <- tolower(gsub(pattern = "^ ", replacement ="", x = belli_values))

# Maping dataframe
map_all <-  data.frame(original=values, curated=all_curated, mapped=rep(x = "NULL", length(all_curated)), stringsAsFactors = F)

# # Unmatched
length(map_all$curated)
matched <- map_all$curated[map_all$curated %in% df$label ]
length(matched) # 187
unmatched <- map_all$curated[!(map_all$curated %in% df$label )] # | map_all$curated %in% df$synonym
length(unmatched) # 1005
length(unique(unmatched)) #  957

# Harmonize/curate unmatched values before mapping
#harmonize()
# ----------------------------------------------------
#source(local = TRUE, "curating_values.R")


# ----------------------------------------------------
# Map curated values to ontology ids
map_all$mapped <- sapply(1:length(map_all$curated), function(c) {
        ifelse(map_all$curated[c] %in% df$label,
               df[which(df$label==map_all$curated[c]),]$id, 
               map_all$mapped[c]
               #df[which(df$synonym==c),]$id
        )
})


# Still unmatched curated values
mapped <- map_all$curated[which(map_all$mapped!="NULL")]
length(mapped) # 470
still_unmatched <- map_all$curated[which(map_all$mapped=="NULL")] # 280
length(still_unmatched) # 722


# Map still unmatched to broad category of organs in sample_type.R
map_all$curated[which(map_all$curated 
                      %in% still_unmatched & (map_all$original 
                  
                                         %in% skin | map_all$curated %in% skin))] <- "zone of skin"
for (o in organ_names) {
        
    map_all$curated[which(map_all$curated 
                              %in% still_unmatched & map_all$curated %in% eval(as.name(o)))] <- gsub(x=as.character(o),pattern = "_", " ")
}
# 
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% brain | map_all$curated %in% brain))] <-  "brain"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% blood | map_all$curated %in% blood))]  <- "blood"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% spinal_cord | map_all$curated %in% spinal_cord))]  <- "spinal cord"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% ln | map_all$curated %in% ln))]  <- "lymph node"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% pituitary_gland | map_all$curated %in% pituitary_gland))]  <- "pituitary gland"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% spleen | map_all$curated %in% spleen))]  <- "spleen"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% thymus | map_all$curated %in% thymus))]  <- "thymus"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% liver | map_all$curated %in% liver))]  <- "liver"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% bladder | map_all$curated %in% bladder))]  <- "bladder organ"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% pancreas | map_all$curated %in% pancreas))]  <- "pancreas"
# map_all$curated[which(map_all$curated %in% still_unmatched & (map_all$original %in% skin | map_all$curated %in% skin))]  <- "zone of skin"


#map_all$curated[which(map_all$curated== "non cortical plate" )] <-  "brain: other" 
# map_all$curated[which(map_all$curated=="whole head" )] <-  "brain:other"
# map_all$curated[which(map_all$curated %in% c("human brain",   "homo sapien brain"))] <-  "brain:other"

# Remap newly curated values to ontology ids
map_all$mapped <- sapply(1:length(map_all$curated), function(c) {
        ifelse(map_all$curated[c] %in% df$label,
               df[which(df$label==map_all$curated[c]),]$id, 
               map_all$mapped[c]
               #df[which(df$synonym==c),]$id
        )
})

# Still unmatched curated values
still_unmatched <- map_all$curated[which(map_all$mapped=="NULL")] # 280
length(still_unmatched) # 565


# # Label remaining unmapped as other anatomic site
map_all$mapped[which(map_all$mapped=="NULL" & map_all$original%in%values)] <-  "other"


# Unmatched
unmatched <- map_all$curated[map_all$mapped == "other" ] # 
length(unmatched) # 565
length(unique(unmatched)) # 541







# How many samples curated so far
matched <- map_all$original[map_all$mapped != "other"]
length(unique(matched)) # 627
originals <- map_all$original[which(map_all$mapped!="other")]
originals_other <- map_all$original[which(map_all$mapped=="other")]
length(unique(filter(ph_tags,  tag %in% anatomic_site_tags, value %in% originals )$EGAN)) # 95888

length(unique(filter(ph_tags,  tag %in% anatomic_site_tags, value %in% map_all$original)$EGAN)) #  190775 


length(unique(map_all$curated)) # 786
length(unique(map_all$mapped)) # 246



map_all$category <- rep("NULL", nrow(map_all))
for (o in main_organs) {
        map_all$category <- ifelse( embellecer(map_all$original) %in% embellecer(eval(as.name(o))), gsub(x=as.character(o),pattern = "_", " "), map_all$category)
}
unique(map_all$category) # 43

map_all$category <- ifelse(map_all$curated %in% zone_of_skin, "zone_of_skin" , map_all$category)

# map_all$category <- ifelse(map_all$curated %in% lung, "lung" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% urethra, "urethra", map_all$category)
# map_all$category <- ifelse(map_all$curated %in% mouth, "mouth" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% eye, "eye" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% stomach, "stomach" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% esophagus, "esophagus" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% brain, "brain", map_all$category)
# map_all$category <- ifelse(map_all$curated %in% heart, "heart", map_all$category)
# map_all$category <- ifelse(map_all$curated %in% ureter, "ureter", map_all$category)
# map_all$category <- ifelse(map_all$curated %in% digestive_tract, "digestive tract" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% prostate_gland, "prostate gland" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% bladder_organ, "urinary bladder" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% trachea, "trachea" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% gonad, "gonad" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% small_intestine, "small intestine" , map_all$category)
# map_all$category <- ifelse(map_all$curated %in% spinal_cord, "spinal cord" , map_all$category)




sort(table(map_all$category), descending=TRUE )

# How many of each category (main organ)
count <- sapply(main_organs, function(o){
        length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                                     map_all$original[which(map_all$category==gsub(x=as.character(o),pattern = "_", " "))])$EGAN)) # 2895
        
})


names(count) <- sapply(main_organs, function(o){
        gsub(x=as.character(o),pattern = "_", " ")})
sort(count)




# Pie chart

dtf <- data.frame(
        group=names(count),
        value=count
)
head(dtf)

library(ggplot2)
#Barplot
bp <- ggplot(dtf, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat="identity")
#bp

#Pie
pie <- bp + coord_polar("y", start = 0)
pie






length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                      map_all$original[which(map_all$category=="brain")])$EGAN)) # 2895

length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="spinal cord")])$EGAN)) #  40
length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="heart")])$EGAN)) # 182
length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="lung")])$EGAN)) # 1305
length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="kidney")])$EGAN)) # 307
length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="liver")])$EGAN)) # 1244

length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="small intestine")])$EGAN)) # 174
length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="large intestine")])$EGAN)) # 174

length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="skin")])$EGAN)) # 648

length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="blood")])$EGAN)) # 648

length(unique(filter(ph_tags, tag %in% anatomic_site_tags, value %in% 
                             map_all$original[which(map_all$category=="saliva")])$EGAN)) # 648

