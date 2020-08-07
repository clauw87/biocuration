# ACE2 metadata
library(dplyr)
library(readr)


source("~/repolab/work/biocuration/hpo_ontology.R")

setwd( "~/repolab/work/ace2")

xml_folder <- "~/repolab/work/ace2/ori_metadata"
all_files <- paste(xml_folder, list.files(xml_folder), sep = "/")
all_files
length(all_files) # 116750
length(unique(all_files)) # 116750

xml_list <-  lapply(all_files, function(f){
        xml2::as_list(xml2::read_xml(f,as_html=TRUE))
})

# test
xml <- xml_list[[1]] 



###### SAMPLE ID ####
sample_id <- lapply(xml_list, function(xml){
                xml$html$body$sample_set$sample$identifiers$primary_id[[1]]
        })

length(sample_id) # 116750 
length(unique(sample_id)) # 41465 .. so others are 
head(unique(sample_id)) # "ERS028050"
sum(is.na(unique(sample_id))) # 0

###### PHENOTYPE TAG ######
# xml_attr <-  lapply(all_files, function(f){
#         xml2::xml_attr(xml2::read_xml(f), "phenotype")
# })
# 
# length(unique(xml_attr))
# 
# head(unique(xml_attr))

######## SAMPLE ATRIBUTES ######

# xml_attr <-  lapply(all_files[4000:9000], function(f){
#         xml2::xml_attrs(xml2::read_xml(f))
# })
# 
# length(unique(xml_attr))



sample_attributes_tags <-lapply(xml_list, function(xml){
                sapply(1:length(xml$html$body$sample_set$sample$sample_attributes), function(a){
                        xml$html$body$sample_set$sample$sample_attributes[[a]]$tag
                        
                })    
                
                
        })
length(sample_attributes_tags)  # 116750
length(unique(unlist(sample_attributes_tags))) # 45

length(unlist(sample_attributes_tags, recursive = F)) #  387659

sample_attributes_values <- lapply(xml_list, function(xml){
                sapply(1:length(xml$html$body$sample_set$sample$sample_attributes), function(a){
                        xml$html$body$sample_set$sample$sample_attributes[[a]]$value
                        
                })    
                
                
        })



length(unlist(sample_attributes_values, recursive = F)) # 387659

length(sample_attributes_values)  #   116750
length(unique(unlist(sample_attributes_values))) #  37716

length(unlist(sample_attributes_values, recursive = F)) #  387659


sample <- lapply(xml_list, function(xml){
                sapply(1:length(xml$html$body$sample_set$sample$sample_attributes), function(a){
                        xml$html$body$sample_set$sample$identifiers$primary_id[[1]]
                        
                })    
                
                
        })

length(unlist(sample, recursive = F)) #   387659

length(sample)  #   116750
length(unique(unlist(sample))) #     41464

sample <-  unlist(sample, recursive = F)

tag  <- unlist(sample_attributes_tags, recursive = F)

value <- unlist(sample_attributes_values, recursive = F)

pair_table <- tibble(sample, tag, value)
dim(pair_table)     #  387659      3
colnames(pair_table) <- c("sample","tag", "value")


pair_table$value <- lapply(pair_table$value, function(v) {
        unlist(v, recursive = T)
})

unique(unlist(pair_table$tag)) # 45


################################################################################
################################################################################
####  CHECK VALUES  ############################################################

## Phenotype tags
length(unique(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$value)))

unique(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$value))[which(!(unique(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$value)) %in% c("N/A",  "Not applicable",   "not applicable", "Not provided", "None","unknown",  "Proband"     ,    "Parent" ) ))]

length(unique(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$sample))) # 34549

length(unique(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"), value%in% c("N/A",  "Not applicable",   "not applicable", "Not provided", "None","unknown",  "Proband"     ,    "Parent" ))$sample))) # 32821



length(unique(unlist(filter(pair_table, tag %in% c("Phenotype"))$sample))) # 2157
length(unique(unlist(filter(pair_table, tag %in% c("phenotype"))$sample))) # 32392


unique(unlist(filter(pair_table, tag %in% c("Sample Description" , "sample Description" ))$value))
#  "Schizophrenic"

# Cancer - related ph vals 

count$Var1[str_detect(string = count$Var1, pattern = "ancer")]
# [1] "cancer"          "Cancer breast"   "Cancer lung"     "Cancer prostate"
# [5] "lung_cancer"     "Non cancer" 

cancer_values <- c(count$Var1[str_detect(string = count$Var1, pattern = "ancer")],
                   "Lymphoma" )


filter(count, Var1=="Non cancer") # 94 samples
sum(filter(count, Var1 %in% cancer_values)$Freq) # 437 samples

sum(filter(count, Var1 =="Asthma" )$Freq) # 2 samples




#ph_values <- unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$value)

ph_values <- unlist(filter(pair_table, 
                           tag %in% c("Phenotype", "phenotype"))$value)[which(!(unlist(filter(pair_table, tag %in% c("Phenotype", "phenotype"))$value) %in% c("N/A",  "Not applicable",   "not applicable", "Not provided", "None","unknown",  "Proband"     ,    "Parent", cancer_values) ))]

length(unique(filter(pair_table, tag %in% c("Phenotype", "phenotype"), value %in% ph_values)$sample))
length(unique(ph_values)) # 1464 ..1457 .. 1451
sum(is.na(ph_values)) # 0
sum(is.na(ph_values)) # 0
# Translate from HP ontology
ph_labels <- sapply(ph_values, function(p) { 
        ifelse(p %in% tobo$id, tobo$label[which(tobo$id == p)], p)}
)

# "Functional respiratory abnormality" 
# "Asthma"    
# "Arrhythmia" 
# "Arterial tortuosity" 
#  "Abnormality of the upper respiratory tract"  
# "Fragile nails"
# "Full cheeks"
# "Gait disturbance"  
# "Inappropriate behavior" 
# "Impaired ability to form peer relationships"

count <- as.data.frame(table(unlist(ph_labels)), stringsAsFactors = F)
head_count <- head(arrange(count, desc(Freq)), 10)



#------------------------------------------------------------------------------#
# Case control tags 

cs_values <- unlist(filter(pair_table, tag %in% c("case_or_control", "case-control" ))$value)
unique(cs_values) #  "case" "both" "NA"

length(unique(filter(pair_table, tag %in% c("case_or_control", "case-control" ), value %in% cs_values)$sample))


count <- as.data.frame(table(unlist(cs_values)), stringsAsFactors = F)
head_count <- head(arrange(count, desc(Freq)), 10)

#------------------------------------------------------------------------------#
# Health and disease

dis_values <- unlist(filter(pair_table, 
                            tag %in% c("disease_site" ,"DISEASE", "DISEASE_ONTOLOGY_URI"))$value)
unique(dis_values) #  "lung" "None" "NA" 


#------------------------------------------------------------------------------#
# Geo and enthicnity
unique(unlist(filter(pair_table, tag =="country")$value))

length(unique(unique(unlist(filter(pair_table, tag =="country")$sample)))) # 348

unique(unlist(filter(pair_table, tag =="ethnolinguistic group")$value))
unique(unlist(filter(pair_table, tag =="DONOR_ETHNICITY" )$value)) # "NA"  "Northern European"
length(unique(filter(pair_table, tag =="DONOR_ETHNICITY", value =="Northern European" )$sample)) # 37
length(unique(filter(pair_table, tag =="DONOR_ETHNICITY", value =="NA" )$sample)) # 36 ..buuff

et_values <- unlist(filter(pair_table, 
                           tag %in% c("ethnolinguistic group" ,"DONOR_ETHNICITY") ,value!="NA" )$value)

length(unique(filter(pair_table, tag %in% c("ethnolinguistic group" ,"DONOR_ETHNICITY"), value %in% et_values)$sample))


#------------------------------------------------------------------------------#

# Organism part tags
"region"
"organism_part" 
"SAMPLE_ONTOLOGY_URI"
"TISSUE_TYPE"
"BIOMATERIAL_TYPE" 


unique(unlist(filter(pair_table, tag =="organism_part"  )$value)) # blood

unique(unlist(filter(pair_table, tag =="SAMPLE_ONTOLOGY_URI" )$value)) #"http://purl.obolibrary.org/obo/UBERON_0013756"

unique(unlist(filter(pair_table, tag =="TISSUE_TYPE" )$value)) # "venous blood"

unique(unlist(filter(pair_table, tag =="BIOMATERIAL_TYPE"  )$value)) # "Primary Tissue"

sa_values <- unlist(filter(pair_table, 
                           tag %in% c("organism_part", "SAMPLE_ONTOLOGY_URI", "TISSUE_TYPE" ) ,value!="NA" )$value)

length(unique(filter(pair_table, tag %in% c("organism_part", "SAMPLE_ONTOLOGY_URI", "TISSUE_TYPE" ) , value %in% sa_values)$sample))


count <- as.data.frame(table(unlist(sa_values)), stringsAsFactors = F)
head_count <- head(arrange(count, desc(Freq)), 10)
