# Sample type, Anatomical site/organism part/tissue/cell type or cell line and Molecular type
# 1. sample type or specimen type > classifies samples in purified cell type (Kupfer cells), 
# mixed cells from tissue, (PMBCs) mixed cells from organ (liver), xenograft, primary cell line 
#from organ, established cell line (HeLa)
# 2. organism part will specify the origin, the info in parenthesis in 1. 
#Level is according to sample type, if purified cell type is specified, 
#cell type should be the level, if mixed cells or primary cell line from organ, then organ name. 
# • Note the if mix of cell types, a named mixed such as PMBCs or 
#the list of cell types can be specified otherwise
# 3. Molecular type> Total RNA, Total DNA, mitochondrial DNA, 

source("from_raw_tags.R")

# SAMPLE TYPE ###################################################################################### 
# sample type is expected to describe the origin of the sample such as " Patient Derived Xenograft", " diseased tissue", "tumour", " primary cell", "cell line", " PBMC" *(no, maybe this is anatomic site instead)

# Sample type sounding custom tags>these categories contain other stuff such as descriptions " Primary tumour - solid tissue" (histological_features), "diseased tissue" (control?case)
#---------------------------------------
sample_type <- c(" Xenograft ", " Tissue_type " , " Is_tumor " , " Tissue Type ", " ppcg_sample_type ", 
                 " tissue type ", " diseased tissue type ", " Material ",  " material " , " ppcg sample type ", 
                 " sample_type ", " TISSUE_TYPE ", " tissue_type ", " BIOMATERIAL_TYPE "," specimen_type " , 
                 " biomaterial_type ",  " SampleOrigin ", " sample type ", " sample_description ", 
                 " Sample Description ",  " xenograft ", " disease_site ", " sample_type_detail ", " Lund " , 
                 " Lund2 ", " SAMPLE_TYPE " , " Tumour " ," sample.type ", "Biopsy site\" | \"Prostate gland" , 
                 " Tissue Source ", " samples_group " ," Cell_line ", " Primary_Tissue " , " Cell_line ", 
                 " tumor location " , " sample source "," sample_type2 ",  " source ", " LINE ", " tissue ", 
                 " Isolation source ", " organism_part ", " sample type ", " biomaterial_type ",  
                 " Sample type ", " specimen_type " )
length(unique(filter(ph_tags, tag%in% sample_type )$EGAN)) #  279156

unique(filter(ph_tags, tag == " Xenograft ")$value) # no
unique(filter(ph_tags, tag == " sample_type2 " )$value) # " clonal culture" 
unique(filter(ph_tags, tag == " sample type ")$value) # " liver"   " primary"
unique(filter(ph_tags, tag == " Sample type " )$value) # " RNA-seq"   " Exome-seq" " DNA-seq"  
unique(filter(ph_tags, tag == " Cell_line " )$value)  #CELL LINES
unique(filter(ph_tags, tag == " biomaterial_type ")$value) # .. ,  primary cell"
unique(filter(ph_tags, tag == " Material " )$value) # " cell line"



# True sample type values in sample type sounding tags / should be stuff like tissue, cell line, xenograft
sample_type_values <- c(unique(filter(ph_tags, tag == " biomaterial_type " )$value),
                        " Primary Cell", " primary cell", " primary", " Primary Tissue", " Primary",
                        " cell line", " Patient Derived Xenograft", " Xenograft", " xenograft ", " Xenograft of Colorectal Tumor",
                        " Primary tumour - solid tissue", #" bladder tumor", " Renal",
                        " immortalised cell line", " Cell Line", " cell line", " Cell line" ,
                        " primary sample", " Fecal microbiome", " primary sample",
                        " Tumour", " tumour", " tumor", " Tumor", " tumour sample",
                        " diseased tissue",  " normal tissue" ,  " normal from other tissue", " tissue", " disease_site " # these are primary tissue
)

length(sample_type_values)
sum(sample_type_values %in% unique(filter(ph_tags, tag %in% phenotype  )$value))

sample_type_values[sample_type_values %in% unique(filter(ph_tags, tag %in% phenotype  )$value)] #  " primary"       " cell line"     " normal tissue"

# Check 
# Number of samples with this info/ Of course, this info can be derived from organism part, it's a classification
length(unique(filter(ph_tags, tag %in% sample_type | tag %in% phenotype, value %in% sample_type_values )$EGAN)) # 828


# Mapping sample type values -------------------------------------------------------------------------------
primary_cell <- c( " Primary Cell", " primary cell") 
primary_tissue <- c(" Primary Tissue", " diseased tissue", " normal tissue" ,  " normal from other tissue"  ) # here> " primary", " primary sample"  refer to cell, tissue or organ > see paper
primary_tumour <- c( " Tumour", " Primary tumour - solid tissue", " bladder tumor", " tumour sample", " primary tumor")
metastatic_tumour <- c( " metastatic", " lymph node metastasis", " lymph node metastasis")
# are these primary stuff in vitro or ex vivo?
filter(ph_tags, value==" Primary Cell" )$EGAN
filter(ph_tags, EGAN == "EGAN00001165355 " )
xenograft <- c(" Patient Derived Xenograft", " xenograft",  " Biopsies from human grafted duodenum" ) 
# tumour_xenograft <- c(" Patient Derived Xenograft")
cell_line <- c(" cell line", " immortalised cell line", " Cell Line" ) 
invitro_derived_cell <- c(" RNA from iPSC derived macrophages", " macrophages derived from iPS",  " Total RNA from human iPSC-derived sensory neurons") # what is iPSC? (induced pluripotent stem cells)
microbiome <- c(" Fecal microbiome")
autopsy <- c(" Autopsy")

# See in phenotype tags

" Cell line" %in% unique(filter(ph_tags, tag %in% phenotype  )$value)



# ORGANISM PART ###################################################################################### 
# ANATOMICAL SITE 

#TAGS  47
anatomy_tags <- unique(c( " Sample origin " , " Tissue Type ", " lymph_node " , " tissue ", " sample_type2 ", 
              " brain_part " , " origin ", " biopsy_site ", " organ ", " sample site "  , " Part " , 
              " ArrayExpress-OrganismPart " , " dna_source " , " Primary_Tissue " , " Tissue_supergroup ", 
              " Tissue type " , " sample_origin ", " sample_component ",  " sample_site ", " disease_site ", 
              " organismPart ",  " sampling organ ", " sampling site ",  
              " tissue_origin ", " TISSUE_TYPE ", " TISSUE_DEPOT " , " brain_region ",
              " Tissue ", " Sample source ", # here " Sample spatially-mapped " 
              " disease_site_ontology ", # here " SampleOrigin " 
              " Anatomic site " , " Anatomic Site " , " SAMPLE_ONTOLOGY_URI ",  " region ", " organism_part ",
              " ORIGIN_SAMPLE ", " ORIGIN_SAMPLE_ONTOLOGY_URI ", " sample type ", 
              " location of tumour ", "Biopsy site\" | \"Prostate gland" , " esophagus_location ", 
              " age_at_blood_collection ", " blood_collection ", " tumor location ",
              " TISSUE_DEPOT_ONTOLOGY_URI "," CELL_TYPE_ONTOLOGY_URI ", " TISSUE_TYPE_ONTOLOGY_URI "))
#

## Bona fide anatomic site tags
  
unique(filter(ph_tags, tag == " Anatomic Site " )$value)
unique(filter(ph_tags, tag == " organism_part " )$value)
unique(filter(ph_tags, tag == " Primary_Tissue ")$value)
unique(filter(ph_tags, tag == " Tissue_supergroup ")$value)
unique(filter(ph_tags, tag == " Tissue type ")$value)
unique(filter(ph_tags, tag == " sample_origin ")$value)
unique(filter(ph_tags, tag == " sampling site ")$value)
unique(filter(ph_tags, tag == " sample_site ")$value)
unique(filter(ph_tags, tag == " sample_origin ")$value)
unique(filter(ph_tags, tag == " sampling organ ")$value)
unique(filter(ph_tags, tag == " organismPart ")$value)
unique(filter(ph_tags, tag == " ORIGIN_SAMPLE ")$value)
unique(filter(ph_tags, tag == " sample type ")$value)
unique(filter(ph_tags, tag ==  " tissue_origin ")$value)
unique(filter(ph_tags, tag == " TISSUE_TYPE ")$value)
unique(filter(ph_tags, tag == " TISSUE_DEPOT ")$value)
unique(filter(ph_tags, tag == " Tissue ")$value)
unique(filter(ph_tags, tag == " Sample source ")$value)
unique(filter(ph_tags, tag == " Anatomic site ")$value)
unique(filter(ph_tags, tag == " region ")$value)
unique(filter(ph_tags, tag ==  " tumor location ")$value)
 
anatomic_site_tags <- c(" Anatomic Site ", " organism_part ", " Primary_Tissue ", " Tissue_supergroup ",
                        " Tissue type ", " sample_origin ", " sampling site ", " sample_site ", " sample_origin ",
                        " sampling organ ", " organismPart ", " ORIGIN_SAMPLE ", " sample type ",  " tissue_origin ",
                        " TISSUE_TYPE ", " TISSUE_DEPOT ", " Tissue ", " Sample source ", " Anatomic site ",
                        " brain_region ", " brain_part " , " region ", " tumor location ", " specimen_type "
        )

anatomic_site_tags_values <- unique(filter(ph_tags, tag %in% anatomic_site_tags)$value) # 704
length(anatomic_site_tags_values) # 1192
length(intersect(anatomic_site_tags_values, anatomic_site_values)) # 
length(setdiff(anatomic_site_tags_values, anatomic_site_values)) # 

# tags of specific organ
" brain_region "
" brain_part "

" lymph_node "
" specimen_type "

"Biopsy site\" | \"Prostate gland"
" esophagus_location "


# anatomic tags as ontology
unique(filter(ph_tags, tag==  " ORIGIN_SAMPLE_ONTOLOGY_URI ")$value) # ontology https
unique(filter(ph_tags, tag==  " SAMPLE_ONTOLOGY_URI ")$value) # ontology https

# SAMPLES
# Samples with anatomy tags 
length(unique(filter(ph_tags, tag%in% anatomy_tags )$EGAN)) # 203834
# Samples with anatomy tag "organism part" specifically
length(unique(filter(ph_tags, tag == " organism_part ")$EGAN)) # 92845



# VALUES
# null values
na_values <- c(" NA", " unknown", " N/A")
# All anatomy tags values 
length(unique(filter(ph_tags, tag %in% anatomy_tags)$value)) # 2289 unique values from anatomy tags
anatomy_tags_values <- unique(filter(ph_tags, tag %in% anatomy_tags)$value) # 2289
anatomy_tags_values <-anatomy_tags_values[!(anatomy_tags_values %in% na_values)] # 2286
length(anatomy_tags_values)   #  2286 unique not null values in anatomy tags



# Anatomic values per bona fide anatomy tag

# ANATOMICAL SITE Values
length(unique(filter(ph_tags, tag == " Anatomic Site " )$value)) #  204
length(unique(filter(ph_tags, tag == " organism_part " )$value)) #  407
length(unique(filter(ph_tags, tag == " Sample origin " )$value)) # 1 " saliva"
length(unique(filter(ph_tags, tag == " Tissue Type " )$value)) #  5
length(unique(filter(ph_tags, tag==  " ORIGIN_SAMPLE_ONTOLOGY_URI ")$value)) # 26
length(unique(filter(ph_tags, tag==  " SAMPLE_ONTOLOGY_URI ")$value)) # 171


unique(filter(ph_tags, tag==  " specimen_type ")$value)

# Fish for more tags
# tags with anatomical site values 
unique(filter(ph_tags, value!=" NA", value!= " unknown", value!=" Other", value!=" N/A", is.na(value)==FALSE, value %in%
                      unique(filter(ph_tags, tag == " organism_part " )$value))$tag) # 97 tags
possible_anatomy_tags <- unique(filter(ph_tags, 
                                       value!=" NA", value!= " unknown", value!=" Other", value!=" N/A", is.na(value)==FALSE, 
                                       value %in%
                                        unique(filter(ph_tags, tag == " organism_part " )$value))$tag) # 97 tags


additional_anatomy_tags <- possible_anatomy_tags[!(possible_anatomy_tags%in%anatomy)] #65
# values
length(unique(filter(ph_tags, tag %in% additional_anatomy_tags,
                     value!=" NA", value!= " unknown", value!=" Other", value!=" N/A", is.na(value)==FALSE)$value)) #  558836





# search for values in each tag
unique(unique(filter(ph_tags, value!=" NA", value!= " unknown", value!=" Other", value!=" N/A", is.na(value)==FALSE, 
                     value %in%
                      unique(filter(ph_tags, tag ==  " organism_part ")$value), tag==  " Phenotype "  ))$value)

# loose mathching
agrep(pattern = "tumour", x = "tumor" )

# let's start with these 618
# anatomic site values in sample_type tags> tags " ppcg sample type ", " tissue_type " 
anatomic_site_values <- unique(c(" PBMC", " blood", " Whole blood" , " blood (whole)", " blood", " Blood",   
                          " Normal - blood derived",  " DNA from whole blood", " Peripheral Blood" , 
                          " Whole blood lymphocyte DNA", " Blood",  " venous blood", " Whole blood" ,
                          " liver",
                          " saliva",
                          " bladder", " bladder tumor",
                          " RNA from iPSC derived macrophages", " macrophages derived from iPS", 
                          " DNA from normosmic", # here
                          " Total RNA from human iPSC-derived sensory neurons", 
                          " Normal - lymph node"," lymph node", " Lymph Node",
                          " bone marrow" ," Bone Marrow",
                          " Brain", " Temporal Cortex" , " Cerebellum" , " Prefrontal Cortex" , " anterior cingulate cortex", " VERMIS",
                          " ureter"    , 
                          " bladder"  , 
                          " other"   ,  
                          " lymph node metastasis", 
                          " kidney"   ,  " Glomerular capsule" ,
                          " ovary" , 
                          " Lung", " Left Lung" , " Right lung",
                          " BC",  
                          " colon"," Metastatic colorectal cancer", " Colon",
                          " Prostate", " prostate",
                          " Breast", " Breast epithelium" ,
                          unique(filter(ph_tags, tag== " organism_part ")$value), # old sataset [-c(1, 66, 75, 76,81,92)],
                          unique(filter(ph_tags, tag==  " ORIGIN_SAMPLE_ONTOLOGY_URI ")$value),
                          unique(filter(ph_tags, tag==  " SAMPLE_ONTOLOGY_URI ")$value) # here [-c(2,9,10,11,13,14,15)]
                          ))


length(unique(anatomic_site_values)) #  618
length(anatomic_site_values %in%unique(filter(ph_tags, tag== " organism_part ")$value)) # 616
unique(filter(ph_tags, tag== " organism_part ")$value)[-c(1, 66, 75, 76,81,92)] # 401


# Check 
# Number of samples with this info/ Of course, this info can be derived from organism part, it's a classification
length(unique(filter(ph_tags, tag %in% anatomy_tags | tag %in% phenotype, value %in% anatomic_site_values )$EGAN)) # 14092


# with url for ontologies
url_pattern <- c("http")

with_url <- anatomic_site_values[which(str_detect(string=anatomic_site_values, pattern=url_pattern))] # 174

nourl_anatomic_site_values <- anatomic_site_values[!(anatomic_site_values
                                                     %in% with_url)] # 438



# NOTE All samples with true anatomy values (or ontologies) in custom tags or phenotype tags could theoretically be categorized as primary tissue sample type






# Grouping antomic site values from anatomic site and organism part tags-------------------------------------------------------------------------------

# blood
pbmc <- embellecer(c("blood-peripheral-leukocytes-monocytes", "peripheral blood mononuclear cells", "modc from pbmc" , " PBMC", " PBMCs", " blood peripheral mononuclear cells", 
          "pbmc (b cells)", "pbmc (t cells)", "blood leucocyte dna", "blood cd3+", " Blood-Peripheral-Leukocytes-Lymphocytes",
          " Blood-Peripheral-Leukocytes", "blood cd3+", "blood leucocyte dna" ))
capillary_blood <- c("capillary blood")
plasma <- embellecer(c("platelet-free plasma", "plasma"))
serum <- embellecer(c("serum"))
cord_blood <- embellecer(c(" Cord tissue/blood", " Cord Blood" , "cord blood", " Cord Blood"))
venous_blood <- embellecer(c("venous blood"))
buffy_coat <- embellecer(c(" Blood-Peripheral-Buffy Coat"," Buffy coat DNA"))
peripheral_blood <- embellecer(c(buffy_coat,"primary tumour - blood derived (peripheral blood)", "peripheral blood/left axilla", " blood-peripheral/", " blood-peripheral" , " Blood-Peripheral", "peripheral blood" , 
                     " Blood-Peripheral", " Peripheral Blood", "peripheral blood"))
whole_blood <- embellecer(c("wholeblood-dna", "whole blood", " Whole blood lymphocyte DNA", " Whole blood" , 
                 " blood (whole)"," Whole-blood"," DNA from whole blood"))
blood_other <- embellecer(c("blood cd19+", " DNA from blood"," blood granulocytes"))
                  
just_blood <- embellecer(c("human blood" , " Blood", " blood", " Normal - blood derived"," blood DNA", "nf1-blood"))

blood <- embellecer(c("blood" , "uberon:0000178", pbmc, capillary_blood,plasma,serum, cord_blood, venous_blood, buffy_coat, peripheral_blood, 
           whole_blood, blood_other, just_blood, "human blood")) # blood is after all usually PBMCs but if info is not available, the more broad term for tissue, blood, is used

# brain
brain <- embellecer(c("uberon:0000955",  "Brain", "human brain" , "cerebellar", # "left caudate"
           unique(filter(ph_tags, tag== " brain_region ")$value), 
           unique(filter(ph_tags, tag== " brain_part ")$value)   , 
           " Temporal Cortex" , " Cerebellum" , " Prefrontal Cortex", 
           " anterior cingulate cortex", " Brain-Temporal lobe-Left", 
           " VERMIS", " brain glioblastoma", " Brainstem", " Right frontal lobe",
           " Amygdala", " occipital cortex", "posterior cranial fossa", "hippocampus",
          "thalamus" , "bithalamic - right thalamus", #  "disseminated from spinal cord"
           "right cortex" ,  "bulbo protuberancial", "temporo-parietal lobe" , "posterior fossa",
           "left fronto-parietal lobe", "hemispheric", "pineal" , "suprasellar",  "midline" , "ventricle",
           "fronto-temporal lobe" ,  "left tempo-parietal lobe"  ,"left parietal" ,  "right temporal" , 
           "left cortex" , "disseminated from spinal cord", "external capsule", " nf1-glioma", "periventriclular",
           "basal nuclei",  "homo sapien brain", "brainstem (mesencephalon)" , "right temporal", "cerebrum",
           "left temporo-parietal lobe" , "frontal right cortex", "thalamus/fornix",  "left temporo-parietal lobe", "right parietal",  "hypothalamic chiasm", "right thalamus", "frontal right cortex", 
        "left thalamus" , "right occipital lobe", " Brain-Temporal lobe-Left", " Brain-Frontal Lobe-Left" ,
        " Brain-Frontal Lobe-Right", " Brain-Cerebellum", " brain-forebrain", " Brain-Temporal lobe", 
        " temporal lobe" , " amygdala", " Brain-right temporal", "right occipetal lobe",
        " Meningeal cluster", "interior pons"," Median nerve", " GBM", " Brain-Frontal Lobe-Left", 
        " foramen monro","left parasagittal lobe" ,"4th ventricle", "cortex middle", "cortical hem", 
        "mesencephalon", "medulla","pons/cereb", "subcortex",
        "frontotemporal cortex","meninges", "mge (striatum)",
        "spinal cord cervical", "dorsal midbrain","lower cortex","spinal cord thoracic","upper cortex",
        "tel/diencephalon","ventral midbrain","lge (striatum)", "cortex anterior","forebrain cortex",
        "white matter brain tissue", "subcortical forebrain",
        unique(filter(ph_tags, tag == " brain_region ")$value),
        unique(filter(ph_tags, tag == " brain_part ")$value)
        ))

spinal_cord <- embellecer(c("spinal cord", "spinal cord lumbar", "spinal cord-thoracic" , 
                            " Spinal Cord-Sacral", " Spinal cord", " Spinal cord-thoracic", 
                 " Sacral spinal cord", "disseminated from spinal cord", "spinal cord cervical", 
                 "thoracic spinal chord","cervical spinal chord", "thoracic spinal chord", 
                 "spinal cord thoracic", "spinal cord-thoracic", "spinal cord cervical", 
                 "spinal cord lumbar", "spinal cord-sacral", " Disseminated from Spinal Cord"))

placenta <- embellecer(c(" placenta")) 

# lymph node
lymph_node <- embellecer(c("lymph node of neck", " Normal - lymph node", " lymph node", " Lymph Node",
         " lymph node metastasis", " Mediastinal lymph node", #" Lymphoid"
         " Lymph nodes", " Lymph node",  "supraclavicular node", "paratracheal lymph node",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Left Supraclavicular",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node", 
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-left axilla",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-right axilla",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Peripheral-Submandibular",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Retroperitoneal",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Right Supraclavicular",
        " lymph node metastases", " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Inguinal",
        " Lymph Node-Right Supraclavicular", " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Axillary",
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Axillary" ,
        " External iliac lymph node", " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Neck-Left" ,
        " Hematopoietic Or Lymphoreticular Systems-Lymph Node-right inguinal",
        " Left supraclavicular lymph node" , " Hematopoietic Or Lymphoreticular Systems-Lymph Node-Celiac",
        " Right supraclavicular lymph node", "hematopoietic or lymphoreticular systems-lymph node-hilar",
        "right axillary lymph node", "hematopoietic or lymphoreticular systems-lymph node-neck-left",
        "hematopoietic or lymphoreticular systems-lymph node-right parasternal", "left inguinal lymph node",
        "right inguinal lymph node",  "hematopoietic or lymphoreticular systems-lymph node-mediastinal-paratracheal",
        "hematopoietic or lymphoreticular systems-lymph node-left inguinal",
        "hematopoietic or lymphoreticular systems-lymph node-left paratracheal",
        "metastatic tumour - lymph node", "metastatic tumour - metastasis local to lymph node",
        "mesosigma lymphnode",
        #dubious see "EGAD00001004109"
        "axillary", "laterocervical", "inguinal", "iliac", 
        "non specified/maxillar", "supraclavicular",
        "laterocervical/supraclavicular/inguinal", "left axillary lymph node", 
        "lymph node of left neck"
        ))

pituitary_gland <- embellecer(c("pituitary gland", "pituitary adenoma"))

spleen <- embellecer(c("spleen" , "hematopoietic or lymphoreticular systems-spleen"))

thymus <- embellecer(c("thymus", " thymus lymphoid tissue"))

# liver
liver <- embellecer(c("liver", "left liver 2"  , "right liver 2" , "left liver 1"  , "left liver 5" , "liver metastasis" , "hepatocytes", " liver", " Liver metastasis",  " Liver",
           "left liver 3", "right liver 1" , "right liver 4",  "right liver 3" , "left liver 4" ))

kidney <- embellecer(c("renal",  "kidney", " Kidney", " Glomerular capsule" )) 

urinary_bladder <- embellecer(c("urinary bladder", " Urinary tract-bladder" , "bladder peritoneum", "bladder", 
                        " bladder", " bladder tumor"
                        ))
pancreas <- embellecer(c("distal pancreas" , " Pancreas", "foetal pancreas"))
bone_marrow <- embellecer(c("bone marrow " , "normal - bone marrow", "primary tumour - blood derived (bone marrow)", "bone marrow",  " Marrow", " Bone marrow", " Bone Marrow", " Bone marrow - mononuclear cells", 
                 "bone marrow dna",  "peripheral blood/bone marrow"))
vein <- embellecer(c("vein"," median cubital vein"))



ureter <- embellecer(c(" ureter", "vesico uterine pouch", "ureter onfice" ))
cervix <- embellecer(c("cervix", " Cervix"))
fallopian_tube <- embellecer(c("fallopian tube", "uterus-fallopian tube"))
uterus <- embellecer(c(cervix, fallopian_tube, " Uterus"  ))
endometrium <- embellecer(c(" ENDOMETRIUM", " Endometrium"))
ovary <- embellecer(c("ovary", " Ovary-Left", " Ovary-Right", " OVARY" , "right ovary exterior vegetation" , "right ovary exterior nodule" , "left ovary deep sampling", "right ovary deep sampling", "right ovary macronodule" , "epithelial ovary / primary peritoneal" , "ovary (left side within tumor)" , "ovary (right side within tumor)", " ovary", " epithelial ovary", " epithelial ovary / fallopian tube" , "right ovary capsule",
           "solid component right ovary", "ovary left"  ))
testis <- embellecer(c(" Testis"))
lung <- embellecer(c("lung", "metastatic lesion in lung", "normal lung tissue", "lung-right middle lobe", "Lower left lobe", " Lower lobe of left lung", " Lung-Trachea", " Lung-Pleural Effusion", " Lung-Right Upper Lobe" , " Lung (lobe)" ,
          " Lung-Left Lobe", " Lung-Right Lower Lobe", " Lung-Left Lower Lobe", " Lung-Left Upper Lobe", 
          " Lung-Right Middle Lobe", "left lower lobe lung", "normal solid tissue, 
          adjacent to lung adenocarcinoma" , "lung adenocarcinoma"  , "metastatic lesion in lung", 
          " lung", " Lung", " Left Lung" , " Right lung", " Left lung", " Lung-Bronchus",
          " bronchial biopsy", " normal lung tissue",  "metastatic lesion in lung",
          "lung-right middle lobe","lung-left lower lobe","lung-left upper lobe",
          "lung-right lower lobe", "lung-left lower lobe", "lung-left upper lobe",
          "lung-right lower lobe","lung-left lobe", "bronchial biopsy",
          "lung (lobe)", "lung-right lobe", "left lower lobe lung",
          "lung adenocarcinoma",
          "normal solid tissue, adjacent to lung adenocarcinoma",
          "lung-right upper lobe", "lung-pleural effusion"
          ))
breast <- embellecer(c( "breast cancer", " breast", " Breast", " Breast epithelium", " BREAST" , 
             " Mammary gland-Breast", "skin-breast"))
epigastrium <- embellecer(c("epigastrium",  " Abdomen-Epigastrium"))
stomach <- embellecer(c( "greater curvature stomach", " Stomach" , "greater curvature of stomach"))
esophagus <- embellecer(c(" Esophagus"))
omentum <- embellecer(c(" Abdomen-Abdominal cavity-Omentum", " Abdominal cavity (omentum)" , 
                        "metastatic lesion in omentum", " Omentum", " Omemtum"))
caecum <- embellecer(c("caecum", " Cecum", " Large Intestine-Cecum"))
large_intestine <- embellecer(c("large intestine", cecum, " mucosa of large intestine", " Large Intestine-Colon-Rectosigmoid [normal]",
                     " Large Intestine-Colon [normal from clear margin of CEMT0064]",                
                     " Large Intestine-Colon [normal from clear margin of CEMT0067]" ,               
                     " Large Intestine-Colon-Ascending (Right) [adjacent normal to CEMT0062]",
                     " Large Intestine-Colon-Rectosigmoid [normal from clear margin of CEMT0066]"
                      ))
duodenum <- c(" Duodenum", " Human duodenum" ," Biopsies from human (native)  duodenum","biopsies from human grafted duodenum"
)
small_intestine <- c(duodenum, " Small Intestine", "small bowel meso",  " Small Intestine", " Small bowel",  "meso small intestine")
intestine <- embellecer(c("intestine", small_intestine, large_intestine, " Gut", "gut", "intestinal", " Bowel - colon, caecum",
                         "right paracolic area"
                          ))

colon <- embellecer(c("msi" , "mss", " Colon Sigmoid", " colon" , " Colon", 
                      " Large Intestine-Colon-Rectosigmoid [adjacent normal to CEMT0066]",  
                      " Large Intestine-Colon-Rectosigmoid",
                      "sigmoid", "rectosigmoïd"," Large Intestine-Colon-Rectosigmoid [adjacent normal to CEMT0066]",
                      "colon transversum"," Bowel - colon, ascending" ,
                      "colon ascendens", " Bowel - colon, descending",
                      "colon descendens"," Bowel - colon, sigmoid" ,  " Bowel - colon, transverse", "transverse" ,
                      " Large Intestine-Colon-Rectosigmoid [adjacent normal to CEMT0066]" , 
                      " Large Intestine-Colon-Rectosigmoid"," Left colon" ,
                      " Large Intestine-Colon [adjacent normal to CEMT0063]" , "mesentery rectosigmoid",
                      " Large Intestine-Colon-Sigmoid", "bowel - colon", " Right colon" ," Rectosigmoid junction",
                      " Large Intestine-Colon-Rectosigmoid" ,
                      " Large Intestine-Colon-Ascending (Right)"," Large Intestine-Colon [normal from clear margin of CEMT0065]"  ,
                      " Large Intestine-Colon", " Large Intestine-Colon [adjacent normal to CEMT0065]"    , 
                      " Large Intestine-Colon [adjacent normal to CEMT0064]"," Large Intestine-Colon [adjacent normal to CEMT0067]",
                      " Large Intestine-Colon-Descending (Left)"
))

colorectum <- c("xenograft of colorectal tumor",  " Colon and rectum", " Colorectum", "intestine (colorectal cancer)", " Colorectal tumor",
                " Metastatic colorectal cancer")
vermiform_appendix <- embellecer(c("vermiform appendix", " Appendix",  " Appendix" )) 
rectum <- embellecer(c(" Abdomen-Abdominal wall-Rectus", " Large Intestine-Rectum" , 
                       " rectum", " Bowel - rectum", "large intestine-anus"))
digestive_tract <- c("digestive tract", esophagus, epigastrium, stomach, omentum, bowel, colon, colorectum, rectum, gut, cecum, large_intestine, " GI Tract", "bowel", " Alimentary part of gastrointestinal system")
heart <- embellecer(c("whole-heart", "heart", " left ventricle",  "Heart", "right ventricular"))
prostate_gland <- embellecer(c("prostate gland", "human prostate tissue" , "human prostate_rna" , " Prostate", " prostate", 
               "prostate cancer metastasis", "human prostate_rna",  " Prostate Tumour",
                " PROSTATE" ))
sperm <- embellecer(c( " Sperm" ))
abdominal_wall <- embellecer(c("abdominal wall"," Abdomen-Abdominal wall" ))
peritoneum <- embellecer(c("peritoneum", " Abdomen-Abdominal cavity-Peritoneal cavity"," Abdomen-Abdominal cavity-Peritoneum", 
                           " Abdomen-Abdominal cavity-Retroperitoneum" , "epithelial ovary / primary peritoneal" , 
                           "metastatic peritoneal lesion", " peritoneum", " Retroperitoneum", " Peritoneum", 
                " primary peritoneal", "bladder peritoneum",
                " epithelial ovary / fallopian tube / primary peritoneal", " Abdominal cavity (retroperitoneum)" )) # > peritoneum

zone_of_skin <- embellecer(c("zone of skin", "head & neck", "skin-breast", "skin-leg", "skin (arm)"  , "skin (head and neck)" , 
                      "primary human epidermal melanocytes isolated from adult skin", 
                      "skin-chest", " SKIN", " Skin-Back", " Skin-Abdominal", " Skin-Right Flank", " Skin-eyelid", 
                      " Skin-Chest Left" ," Skin-Head and Neck", " Penis foreskin" , " Skin-Arm", "skin non-photoexposed", 
                      "skin chronically-photoexposed", "skin intermittently-photoexposed" , " Skin", " skin" , 
                      " Skin Fibroblast", "intraepithelial lymphocytes", "human skin", "head-neck", "head and neck" ))
cartilage_element <- embellecer(c(" Cartilage" ))
mesentery <- embellecer(c(" mesentery", " Mesentery" ))
pleura <- embellecer(c("right pleura", " Soft Tissue-Pleura",  " Pleura", " Mesothelium-Pleural" )) # >"pleura"
muscle_tissue <- embellecer(c("muscle tissue", "lateral of psoas muscle", "soft tissue-muscle", "muscle", "Soft Tissue-Muscle-Smooth" , " Skeletal Muscle", " Skeletal muscle", 
                       " Smooth muscle", " skeletal muscle tissue", " Vastus Lateralis Muscle"))

adipose_tissue <- embellecer(c("adipose tissue", "omental adipose tissue", " Adipose" , " Fat", " subcutaneous adipose tissue", 
                               " Periprostatic fat", " white adipose tissue"  ))
thyroid_gland <- embellecer(c(" Thyroid", " thyroid tissue", " thyroid tumor", " Thyroid"))
eye <-  embellecer(c("thyroid gland", " eyes", "primary eye tumor", " Eye"))

pelvic_cavity <- embellecer(c("pelvic cavity", " Pelvis-Pelvic cavity", "pelvic wall", "pelvic wall, left"))

#bone
bone_element <- embellecer(c("bone element", "bone-vertebra", "sinonasal-ethmoid", "bone-rib", "pelvis-left","bone-pelvis-left-iliac crest",  "bone", "bone-skull-clivus", "bone (rib)" , "bone (sternum)", "bone-skull", "bone-femur" , 
                  "bone-sternum", " Pelvis-Right", " Femur", " Ilium", " Bone-Vertebra", 
                  " Upper jaw region", " Sinonasal-Ethmoid", " Hip", " Pelvis-Left", 
                  " Bone-Pelvis-Left-iliac crest" , " Bone-Rib", " iliac (hip) bone", 
                  "ileocoecaalovergang", "pelvis right side" , "iliac bone", 
                  "t8 vertebrae" , "bone" , "pelvic bone" , " mandibula" , " Bone", " iliac (hip) bone", 
                  " Pelvis", " Presacral", " Spine", " Thorax", " pelvis right side"))

#artery <- c("iliac left", "iliac right")

nose <- embellecer(c(" Nasopharynx-Nasal", " nose", " Nasal cartilage", " Nasal Pharynx"))
inguen <- embellecer(c(" Left inguinal", " Groin")) # inguinal part of abdomen
thigh <- embellecer(c("thigh"," Left thigh"))
axila <- embellecer(c("axila", " Right axilla"))
mouth <-  embellecer(c("mouth", parotid, " Mouth floor" , " Oral Cavity", " Tongue", " buccal cell", " Buccal cells", " Oropharynx-Tongue", "normal - buccal cell",
                       "mandibula", 
                       "palate"
                     ))
bc <- c(" BC" ) # here esto que es, buccal cells as well?
trophectoderm <- embellecer(c(" trophectodermbiopsy"))
embryo <- embellecer(c(" wholeembryo"))
adrenal_gland <- embellecer(c(" Adrenal gland" , " Adrenal", " adrenal", "right adrenal",  "left adrenal", " Adrenal Gland" ))
tonsil <- embellecer(c(" Tonsil", " tonsil" ))
ascitic_fluid <- embellecer(c("ascites cell", " Soft Tissue-Peritoneal-Ascites"))
feces <- embellecer(c( "stool", "faeces", "human faecal_sample" ))
islet <- embellecer(c("islet")) # "islet of Langerhans"
parotid_gland <- embellecer(c("parotid gland", "parotid", " Salivary Gland-Parotid", "right parotid gland"))
diaphragm <- embellecer(c("diaphragm", "right diaphragm", "Diaphragm", "diaphragm right side", "diaphragm left side"))
#vagina <- c(" Vagina" )
#" Head and neck"
#" Thorax"
#" Presacral"
saliva <- c("saliva")


# everything such as left, right, lower, distal, anterior
anatomic_detail <- embellecer(c(" Left upper quadrant"))


# CELL TYPE OR CELL LINE
cell_type <- c( pbmc, " cell type ", " target cell type ", " cell category ", " cell_type ", 
               " PASSAGE_IF_EXPANDED ", #here
               " CELL_TYPE " ," cellLine "," cell_line ",  
               " CELL_TYPE_ONTOLOGY_URI ",  " T cells", " B cells", " Bone marrow - mononuclear cells", 
               " CD34+ cells"
)

# special cell types
neuron <-  c(" Total RNA from human iPSC-derived sensory neurons")
macrophage <- c(" RNA from iPSC derived macrophages", " macrophages derived from iPS") # what is iPSC? (induced pluripotent stem cells)
lymphocyte <- c( "whole blood lymphocyte dna")
fibroblast <- c(" Skin Fibroblast")
epithelial <- c(" epithelial ovary",  "squamous epithelial cell"  )
# search values from ontology dictionaries with all cases combinations converted 



# search values containing dictionary terms
string <- unique(unique(filter(ph_tags, value!=" NA", value!= " unknown", value!=" Other", value!=" N/A", is.na(value)==FALSE, value %in%
                                       unique(filter(ph_tags, tag ==  " organism_part ")$value), tag==  " Phenotype "  ))$value)
string[str_detect(string, "colon")]


cell_type_values <- c(" CD14-positive, CD16-negative classical monocyte", " Whole blood lymphocyte DNA")

# Mapping cell type values ---------------------------------------------------------------------------
monocyte <- c(" CD14-positive, CD16-negative classical monocyte", " monocytes")
lymphocyte <- c("cd19neg t-cell" , "b-cell" , "tumor-infiltrating lymphocyte" , " Whole blood lymphocyte DNA", " Human lymphocytes" )
granulocyte <- c(" blood granulocytes", " granulocytes" )
lymphoblast <- c(" Transformed lymphoblast line")
dc <- c("modc from pbmc" )
hepatocyte <- c("hepatocytes")                                          
macrophage <- "ifng-primed macrophages" 
plasma_cell <- c("aberrant plasma cells" )
# Cell lines
# NOTE All samples with true cell line values (or ontologies) in custom tags or phenotype tags could theoretically be categorized as cell line sample type


# MOLECULAR MARKERS
molecular_markers <- c(" CD31_IHC ", " CD8A_IHC ", " tumor cd8 count ", 
                       " MARKERS ")


# HISTOLOGICAL MARKERS


# MARKERS ###################################################################################### 
cell_type_markers <- c(" CD14-positive, CD16-negative classical monocyte")
# samples with this info
length(unique(filter(ph_tags,tag ==" MARKERS ")$EGAN)) # 16
length(unique(filter(ph_tags,tag ==" MOLECULE ")$EGAN)) # 7

# Mapping cell type markers values ---------------------------------------------------------------------------
cd4_postitive  <- c(" CD14-positive, CD16-negative classical monocyte", " CD4+" )
cd16_negative <- c(" CD14-positive, CD16-negative classical monocyte",  " CD14+ CD16-", " CD11b+/CD16-")
cd11b_postitive  <- c(" CD11b+/CD16-", " CD11b+/CD16++")
cd16_hi <- c(" CD11b+/CD16++")
cd45_negative <- c(" CD45-/CD31-/CD49f+/EpCAM-(low)",  " CD45-/CD31-/CD49f-/EpCAM+" , 
                   " CD45-/CD31-/CD49f+/EpCAMhigh", " CD45-/CD31-/CD49f-/EpCAMhigh", 
                   " CD45-/CD31-/CD49f-/EpCAM-")
cd31_negative <- c(" CD45-/CD31-/CD49f+/EpCAM-(low)",  " CD45-/CD31-/CD49f-/EpCAM+" , 
                   " CD45-/CD31-/CD49f+/EpCAMhigh", " CD45-/CD31-/CD49f-/EpCAMhigh", 
                   " CD45-/CD31-/CD49f-/EpCAM-")
cd90_negative <- c(" CD90(-) Podocalyxin(+)" )
cd49f_negative <- c(" CD45-/CD31-/CD49f+/EpCAM-(low)", " CD45-/CD31-/CD49f-/EpCAM+", " CD45-/CD31-/CD49f-/EpCAMhigh" )
cd49f_positive <- c(" CD45-/CD31-/CD49f+/EpCAM-(low)", " CD45-/CD31-/CD49f+/EpCAMhigh"  )
epcam_lo <- c(" CD45-/CD31-/CD49f+/EpCAM-(low)")
epcam_positive <- c(" CD45-/CD31-/CD49f-/EpCAM+")
epcam_hi <- c(" CD45-/CD31-/CD49f+/EpCAMhigh", " CD45-/CD31-/CD49f-/EpCAMhigh")

# MOLECULAR TYPE ###################################################################################### 
# molecular sample values in sample_type> info is to be extracted as Total RNA, Total DNA, Mitochondrial DNA, Amplicons
# (starting material used for library prep, i.e the result of some purification etc but not enrichment for target sequencing, which would)
# tag " MOLECULE " #  " genomic DNA" " total RNA" 
molecular_type_tags <- c(" organism_part ") 

molecular_type_values <- c(" nuclear RNA", 
                           " DNA" , 
                           " mtDNA" ,
                           " Total RNA", 
                           " total RNA",
                           " ChIP" , 
                           " DNA from whole blood", " Whole blood lymphocyte DNA",
                           " genomic DNA" , 
                           " RNAi induced genomic T brucei DNA samples grown in each drug for 7-10 days",
                           " RNA from iPSC derived macrophages",
                           " DNA from normosmic", 
                           " Total RNA from human iPSC-derived sensory neurons",
                           " Amplicons", 
                           " DNA stock" , 
                           #" Pooled Nextera libraries generated from intestinal organoid single cells",
                           " RNA from patient with primordial dwarfism", 
                           " RNA-seq",   " Exome-seq", " DNA-seq" )

# Mapping molecular type values -------------------------------------------------------------------------------
genomic_dna <- c(" ChIP" , " DNA from whole blood", " genomic DNA", " DNA from normosmic", " DNA stock", " Exome-seq", " DNA-seq") #" RNAi induced genomic T brucei DNA samples grown in each drug for 7-10 days" # here This doesn't sound human
total_rna <- c(" total RNA", " Total RNA",  " RNA from iPSC derived macrophages",  " Total RNA from human iPSC-derived sensory neurons", " RNA from patient with primordial dwarfism",
               " RNA-seq")

