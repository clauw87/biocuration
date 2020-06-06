uberon_terms <- read_delim("~/uberon_terms.txt", delim = "|",  col_names = F)$X1 # , xquote=" \"",escape_backslash = T, escape_double = T 
uberon_terms2 <- read_delim("~/uberon_terms2.txt", delim = "|",  col_names = F)$X1 # , xquote=" \"",escape_backslash = T, escape_double = T 


library(ontologyIndex)
uberon <- get_ontology("~/human-view.obo", propagate_relationships = "is_a", extract_tags = "minimal")
fma <- get_ontology("~/fma2.obo", propagate_relationships = "is_a", extract_tags = "minimal")

# How many terms
sum(str_detect(uberon_terms, "--"  )) # 16920
sum(str_detect(uberon_terms, "\\[Term\\]")) #  17584


uberon_terms_collapsed <- paste0(uberon_terms, collapse = "; ")
        

uberon_terms_split <-  strsplit(uberon_terms_collapsed, split = "\\[Term\\]")

str_detect( uberon_terms_split, "")
length(uberon_terms_split)  
length(uberon_terms_split[[1]])# 17585

# sum(sapply(uberon_terms_split[[1]], function(e){
#         e==""
# })) # only the first instance of [Term] yielded "" OK


#sapply(uberon_terms_split[[1]], function(e){
        
#}) #

term_bulk <- uberon_terms_split[[1]]
term_bulk_split <- str_split(term_bulk, "; ")[-1] # 17584
#each_term <- term_bulk_split[[6]][-c(1, length(term_bulk_split[[6]]))]

##
#each_term_row #id name synonym is_a_id is_a_name

# id
id <- each_term[str_detect(each_term, "id:")]
str_split(id, "id:")[[1]][2]

#  name
name <- each_term[str_detect(each_term, "name:")]
str_split(name, "name:")[[1]][2]

# # part_of
part_of <- each_term[str_detect(each_term, "relationship: part_of ")]
# 

# synonym
synonym <- each_term[str_detect(each_term, "synonym:")]
ifelse(length(synonym)>0, str_split(synonym, "synonym:")[[1]][2], NA)

# is a
is_a_term <- gsub(x = each_term[str_detect(each_term, "is_a:")],
                  pattern = "is_a: ", replacement = "")

is_a_id <- str_split(is_a_term, " ! ")[[1]][1]
is_a_name <- str_split(is_a_term, " ! ")[[1]][2]

# each_term <- c(""        ,                                               
#                "id: UBERON:6000132"     ,                                
#                "name: mesodermal crest of segment T3"   ,                
#               "xref: FBbt:00000132"         ,                           
#                "is_a: UBERON:6026002 ! visceral mesoderm derivative"   , 
#                "relationship: part_of UBERON:6000131 ! mesodermal crest",
#                "--"     ,                                                
#                ""  )

obo_tabla <- c()
for (i in 1:length(term_bulk_split)) {
   each_term <- term_bulk_split[[i]][-c(1, length(term_bulk_split[[i]]))]
   # id
   id <- each_term[str_detect(each_term, "id: ")]
   id <- ifelse(length( id)>0,str_split(id, "id: ")[[1]][2], NA)
   #  name
   name <- each_term[str_detect(each_term, "name: ")]
   name <- ifelse(length(name)>0, str_split(name, "name: ")[[1]][2], NA)
   # synonym
   synonym <- each_term[str_detect(each_term, "synonym: ")]
   synonym <- ifelse(length(synonym)>0, str_split(synonym, "synonym: ")[[1]][2], NA)
   # is a
   is_a_term <- gsub(x = each_term[str_detect(each_term, "is_a: ")],
                     pattern = "is_a: ", replacement = "")
   is_a_id <- ifelse(length(is_a_term)>0, str_split(is_a_term, " ! ")[[1]][1], NA)
   is_a_name <- ifelse(length(is_a_term)>0,str_split(is_a_term, " ! ")[[1]][2], NA)
   
   # part of

   part_of <- gsub(x = each_term[str_detect(each_term, "relationship: part_of ")],
                     pattern = "relationship: part_of ", replacement = "")
   part_of_id <- ifelse(length(part_of)>0, str_split(part_of, " ! ")[[1]][1], NA)
   part_of_name <- ifelse(length(part_of)>0,str_split(part_of, " ! ")[[1]][2], NA)
   
  
   # feed table
    addto <-c (id=id, label=name, synonym=synonym, is_a_id=is_a_id, is_a_label=is_a_name,
               part_of_id=part_of_id, part_of_name=part_of_name)
    obo_tabla <- rbind(obo_tabla, addto)

}
tobo <- as.data.frame(obo_tabla, stringsAsFactors = F)


# major organ clasification

# 24 major organs
#liver, heart, skeletal, kidney, bladder, brain, skin, mouth, esophagus, stomach, 
#small intestine, large intestines, trachea nose, lungs, brain, spinal cord,  
#peripheral nerves, kidneys, ureters, bladder, urethra, gonads # thymus
major_organs <- read_delim("~/major_organs.txt", delim = "|",  col_names = F)$X1
major_organs_id <-  gsub(x= major_organs[str_detect(string = major_organs,pattern = "id: ")], 
                         pattern = "id: ", replacement = "")[-16]
major_organs_label <- gsub(x= major_organs[str_detect(string = major_organs,pattern = "name:")], 
                           pattern = "name: ", replacement = "")

length(major_organs_id)==length(major_organs_label)

# 204 organs
organ_slim <- read_delim("~/organ_slim.txt", delim = "|",  col_names = F)$X1
organ_slim_id <-  gsub(x= organ_slim[str_detect(string = organ_slim,pattern = "id: ")], 
                         pattern = "id: ", replacement = "")[-c(22, 68, 88, 169, 175)]
organ_slim_label <-  gsub(x= organ_slim[str_detect(string = organ_slim,pattern = "name: ")], 
                         pattern = "name: ", replacement = "")


length(organ_slim_id )==length(organ_slim_label)
