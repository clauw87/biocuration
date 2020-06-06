
## BLOOD AND DERIVATES
unique(unmatched[which(unmatched %in% blood)])

map_all$curated[which(map_all$curated %in% pbmc)] <-  "peripheral blood mononuclear cell"
#map_all$curated[which(map_all$curated %in% peripheral_blood)] <-  "peripheral blood" # here
#map_all$curated[which(map_all$curated %in% whole_blood)] <-  "whole blood"
map_all$curated[which(map_all$curated %in% plasma)] <-  "blood plasma"
map_all$curated[which(map_all$curated %in% cord_blood)] <-  "umbilical cord blood"
map_all$curated[which(map_all$curated %in% serum)] <- "blood serum"

# # unmmapped        
# "peripheral blood"   
#  "dna from blood" 
# "whole blood" 
# "blood granulocytes"
# "human blood"                       
# "nf1-blood"                         
# "blood cd19+"                       

## BRAIN 
unique(unmatched[which(unmatched %in% brain)]) # 93




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
map_all$curated[which(map_all$curated=="hemispheric")] <-   "lobe of cerebral hemisphere" #"obsolete hemispheric parts of the cerebellar cortex" # ("EGAD00001003762 " "EGAD00001004036 " "EGAD00001004070 " "EGAD00001004116 " "EGAD00001005131 ")
map_all$curated[which(map_all$curated=="pineal")] <-   "obsolete pineal complex"
map_all$curated[which(map_all$curated=="fronto-temporal lobe")] <-  c("frontal lobe", "temporal lobe") 
map_all$curated[which(map_all$curated=="right parietal")] <-  "right parietal lobe"
map_all$curated[which(map_all$curated=="right temporal")] <- "right temporal lobe"
map_all$curated[which(map_all$curated=="brain-temporal lobe" )] <- "temporal lobe" 
map_all$curated[which(map_all$curated=="brain-right temporal")] <- "right temporal lobe"
# map_all$curated[which(map_all$curated=="left cortex")] <-  #  here see paper "left auditory cortex" or "temporal cortex" (syn "cortex of temporal lobe" )
#map_all$curated[which(map_all$original %in% spinal_cord | map_all$curated %in% spinal_cord)] <-  "spinal cord"
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
map_all$curated[which(map_all$curated %in% c("lower cortex", "whole cortex", "upper cortex", "cortex anterior", "forebrain cortex",
                                             "cortex middle", "cortical hem","right cortex", "left cortex", "hemispheric") )] <-  "cortex of cerebral lobe"
map_all$curated[which(map_all$curated=="whole cortex parietal")] <-  "parietal cortex"
map_all$curated[which(map_all$curated=="whole cortex temporal"  )] <-  "temporal cortex"
map_all$curated[which(map_all$curated=="brain-temporal lobe-left")] <-  "left temporal lobe"
map_all$curated[which(map_all$curated=="brain-occipetal lobe-right")] <-  "right occipital lobe" 
map_all$curated[which(map_all$curated=="external capsule")] <-   "external capsule of telencephalon"
map_all$curated[which(map_all$curated %in% c("interior pons", "pons/cereb"))] <-   "pons" 
map_all$curated[which(map_all$curated=="foramen monro")] <-   "interventricular foramen of CNS"
#map_all$curated[which(map_all$curated== "non cortical plate" )] <-  "brain: other" 
#map_all$curated[which(map_all$curated=="whole head" )] <-  "brain:other"
#map_all$curated[which(map_all$curated %in% c("human brain",   "homo sapien brain"))] <-  "brain:other"
map_all$curated[which(map_all$curated=="white matter brain tissue")] <-   "brain white matter"
 
map_all$curated[which(map_all$curated=="subcortical forebrain")] <-  "forebrain" 

map_all$curated[which(map_all$curated %in% c("dorsal midbrain", "mesencephalon"))] <-  "midbrain" 

map_all$curated[which(map_all$curated =="tel/diencephalon")] <-  c("telencephalon", "diencephalon" )


map_all$curated[which(map_all$curated %in% c("mge (striatum)"), "lge (striatum)")] <-  "striatum" 

map_all$curated[which(map_all$curated=="meninges")] <-  "forebrain meninges" 

map_all$curated[which(map_all$curated=="frontotemporal cortex")] <-  c("temporal cortex", "frontal cortex" )

map_all$curated[which(map_all$curated=="bone-vertebra")] <-  "vertebra"

map_all$curated[which(map_all$curated=="4th ventricle")] <-  "fourth ventricle"

map_all$curated[which(map_all$curated=="medulla")] <- "medulla oblongata"
 
# unmmapped
# "brain glioblastoma"
# "thalamus"  
# "right cortex"
# "bulbo protuberancial" 
# "suprasellar"
#  "midline"
# "ventricle" 
# "left cortex"
# "nf1-glioma" 
#  "periventriclular" 
# "basal nuclei" 
# "hypothalamic chiasm"  > optic chiasm ?


#map_all$curated[which(map_all$curated=="basal nuclei")] <-  "collection of basal ganglia" #basal amygdaloid nucleus" # "basal nuclear complex" 
#unique(filter(ph_tags, EGAD=="EGAD00001005131 ", tag ==" organism_part ")$value)


# SPINAL CORD
unique(unmatched[which(unmatched %in% spinal_cord)])

map_all$curated[which(map_all$curated== "spinal cord-sacral" )] <-  "sacral spinal cord" 
map_all$curated[which(map_all$curated %in% c("thoracic spinal chord", "spinal cord thoracic", "spinal cord-thoracic"))] <-  "thoracic spinal cord" 
map_all$curated[which(map_all$curated== "spinal cord lumbar")] <-  "lumbar spinal cord" 
map_all$curated[which(map_all$curated== "spinal cord cervical")] <-  "cervical spinal cord" 




## LYMPH NODE
# rm these values from ln
#"lymphoid"

unique(unmatched[which(unmatched %in% ln)])

# lnc <- gsub(pattern = "hematopoietic or lymphoreticular systems", "", x = ln)
# lnc <- gsub(pattern = "lymph node-", "", x = ln)
# lnc <- gsub(pattern = "-", " ", x = ln)
# lnc

map_all$curated[which(map_all$curated %in% c("hematopoietic or lymphoreticular systems-lymph node" ,
                                             "normal - lymph node", "lymph nodes", "lymph node metastases", 
                                             "lymph node metastasis"))] <- "lymph node"

map_all$curated[which(map_all$curated %in% 
                              c("hematopoietic or lymphoreticular systems-lymph node-left axilla",
                                "hematopoietic or lymphoreticular systems-lymph node-left axilla", 
                                "hematopoietic or lymphoreticular systems-lymph node-right axilla",
                                "hematopoietic or lymphoreticular systems-lymph node-axillary",
                                "right axillary lymph node",
                                "left axillary lymph node"
                              ))] <- "axillary lymph node"



map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-left paratracheal"
)] <- "paratracheal lymph node"


map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-peripheral-submandibular"
)] <- "submandibular lymph node"

map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-retroperitoneal" 
)] <- "retroperitoneal lymph node"

map_all$curated[which(map_all$curated %in% 
                              c("hematopoietic or lymphoreticular systems-lymph node-right supraclavicular", 
                                "supraclavicular node",
                                "lymph node-right supraclavicular",
                                "left supraclavicular lymph node",
                                "right supraclavicular lymph node",
                                "hematopoietic or lymphoreticular systems-lymph node-left supraclavicular")
)] <- "supraclavicular lymph node"

map_all$curated[which(map_all$curated %in% 
                              c("hematopoietic or lymphoreticular systems-lymph node-right inguinal" ,
                                "hematopoietic or lymphoreticular systems-lymph node-left inguinal",
                                "hematopoietic or lymphoreticular systems-lymph node-inguinal",
                                "left inguinal lymph node",
                                "right inguinal lymph node"
                              ))] <-"inguinal lymph node"  

map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-celiac" 
)] <- "celiac lymph node"  

map_all$curated[which(map_all$curated %in%
                              c("lymph node of neck",
                                "lymph node of left neck",
                                "hematopoietic or lymphoreticular systems-lymph node-neck-left")
)] <- "submental lymph node"  # here, not sure

map_all$curated[which(map_all$curated %in% c(
                              "hematopoietic or lymphoreticular systems-lymph node-hilar",
                              "left hilar node")
)] <- "hilum of lymph node" # here not sure, closest match
# "\"hilum nodi lymphoidei\" EXACT LATIN [FMA:62842, FMA:TA]" 
map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-hilar"
)] <- "hilum of lymph node" # here not sure, closest match

map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-right parasternal"
)] <- "parasternal lymph node"

map_all$curated[which(map_all$curated== 
                              "hematopoietic or lymphoreticular systems-lymph node-mediastinal-paratracheal"
)] <- "mediastinal lymph node"
#"pulmonary lymph node"
# "\"intrapulmonary lymph node\" EXACT [FMA:5968]"  

map_all$curated[which(map_all$curated== 
                              "metastatic tumour - lymph node"
)] <- "lymph node"


map_all$curated[which(map_all$curated== 
                              "median cubital vein")] <- "median basilic vein"
map_all$curated[which(map_all$curated== 
                              "vesico uterine pouch")] <- "uterovesical pouch"
map_all$curated[which(map_all$curated== 
                              "cervix" )] <- "uterine cervix" 

map_all$curated[which(map_all$curated== 
                              "uterus-fallopian tube" )] <- "fallopian tube"

map_all$curated[which(map_all$curated== 
                              "epithelial ovary")] <- "ovarian surface epithelial cell"
map_all$curated[which(map_all$curated== 
                              "epithelial ovary / fallopian tube")] <- c("ovarian surface epithelial cell", "fallopian tube")

map_all$curated[which(map_all$curated== 
                              "right ovary capsule")] <-  "capsule of ovary" 

map_all$curated[which(map_all$curated== 
                              "epithelial ovary / primary peritoneal")] <-  c("ovarian surface epithelial cell", "peritoneal sac")
map_all$curated[which(map_all$curated== 
                              "mammary gland-breast")] <-  "mammary gland"

map_all$curated[which(map_all$curated %in%
                              c("breast cancer",  "breast epithelium", "skin-breast"))] <-  "breast epithelium"




# DIGESTIVE TRACT
map_all$curated[which(map_all$curated==
                              "abdomen-epigastrium")] <-  "epigastrium"


map_all$curated[which(map_all$curated== "esophageal squamous cell")]  <- "epithelium of esophagus" # here "esophagus" 

map_all$curated[which(map_all$curated %in%
                              c("large intestine-colon [adjacent normal to cemt0063]",
                                "large intestine-colon [normal from clear margin of cemt0064]" ,            
                                "large intestine-colon [normal from clear margin of cemt0067]",
                                "large intestine-colon [normal from clear margin of cemt0065]" ,            
                                "large intestine-colon"                        ,                            
                                "large intestine-colon [adjacent normal to cemt0065]" ,                     
                                "large intestine-colon [adjacent normal to cemt0064]" ,                     
                                "large intestine-colon [adjacent normal to cemt0067]"))]  <- "colon"

map_all$curated[which(map_all$curated %in%
                              c("large intestine-colon-rectosigmoid" , 
                                "large intestine-colon-rectosigmoid [adjacent normal to cemt0066]",
                                "rectosigmoid", "large intestine-colon-rectosigmoid [normal]"  , 
                                "large intestine-colon-rectosigmoid [normal from clear margin of cemt0066]",
                                "rectosigmoïd"))] <- "rectosigmoid junction"


map_all$curated[which(map_all$curated %in%
                              c("sigmoid", "large intestine-colon-sigmoid", "bowel - colon, sigmoid")
)] <- "sigmoid colon"

map_all$curated[which(map_all$curated =="bowel - colon, splenic flexure"
)] <- "splenic flexure of colon" 

map_all$curated[which(map_all$curated %in% c("bowel - colon, transverse", "colon transversum")
)] <- "transverse colon" 

map_all$curated[which(map_all$curated == "large intestine-anus")] < "anus"

map_all$curated[which(map_all$curated %in%
                              c("large intestine-cecum", "cecum" , "bowel - colon, caecum")
)] <- "caecum"

map_all$curated[which(map_all$curated %in%
                              c("colorectal tumor", "colo-rectal", "colon and rectum", "intestine (colorectal cancer)", "msi", "mss", "xenograft of colorectal tumor" ) 
)] <-c("colon", "rectum")

map_all$curated[which(map_all$curated %in%
                              c("abdomen-abdominal wall-rectus", "large intestine-rectum" ,"bowel - rectum")
)] <- "rectum"

map_all$curated[which(map_all$curated %in%
                              c("colon ascendens", "large intestine-colon-ascending (right)",  "bowel - colon, ascending",
                                "large intestine-colon-ascending (right) [adjacent normal to cemt0062]")
)] <- "ascending colon"

map_all$curated[which(map_all$curated %in%
                              c("human duodenum", "biopsies from human (native)  duodenum", "biopsies from human grafted duodenum" )
)] <-  "duodenum"


map_all$curated[which(map_all$curated %in%
                              c("bowel - colon, descending","large intestine-colon-descending (left)", "colon descendens")
)] <-  "descending colon"

map_all$curated[which(map_all$curated %in% c("right paracolic area", "paracolic right")
)] <-  "paracolic gutter"



map_all$curated[which(map_all$curated %in% omentum
)] <-  "omentum"

map_all$curated[which(map_all$curated %in% c("meso small intestine", "small bowel meso") 
)] <-  "mesentery of small intestine" # c("small intestine", "meso-epithelium")

map_all$curated[which(map_all$curated =="gi tract"
)] <- "digestive tract"

map_all$curated[which(map_all$curated =="mesentery rectosigmoid"
)] <- "gastrointestinal system mesentery" 

map_all$curated[which(map_all$curated %in% c("greater curvature stomach", "greater curvature of stomach")
)] <-"greater curvature of stomach"

map_all$curated[which(map_all$curated == "bowel - colon, caecum")] <- c("colon", "caecum")



# HEART
map_all$curated[which(map_all$curated =="left ventricle"
)] <-   "heart left ventricle" 
map_all$curated[which(map_all$curated =="right ventricular"
)] <-   "heart right ventricle"


# PROSTATE
map_all$curated[which(map_all$curated %in% prostate_gland
)] <-  "prostate gland" 



# AB WALL
map_all$curated[which(map_all$curated == "abdomen-abdominal wall"
)] <-   "abdominal wall" 

map_all$curated[which(map_all$curated == "abdomen-abdominal cavity"
)] <-   "abdominal cavity" 


# PERITONEUM
map_all$curated[which(map_all$curated == "abdomen-abdominal cavity-retroperitoneum" 
)] <-  "retroperitoneal space" 
map_all$curated[which(map_all$curated == "primary peritoneal"
)] <-  "peritoneum"


# SKIN
map_all$curated[which(map_all$curated %in% c("skin-arm",  "skin (arm)"  ) 
)] <-  "arm skin" 
map_all$curated[which(map_all$curated == "penis foreskin"
)] <-  "skin of prepuce of penis"
map_all$curated[which(map_all$curated %in% c("head & neck", "head-neck" ,"head and neck", "skin-head and neck", "skin (head and neck)" )
)] <-  "head or neck skin"
map_all$curated[which(map_all$curated %in% c("skin-chest", "skin-chest left") 
)] <-  "skin of chest"
map_all$curated[which(map_all$curated ==  "intraepithelial lymphocytes"   
)] <-  "skin epithelium" 
map_all$curated[which(map_all$curated ==  "skin-eyelid"   
)] <-   "skin of eyelid" 
map_all$curated[which(map_all$curated %in% c("skin-right flank" , "left flank")
)] <-   "skin of body" 
map_all$curated[which(map_all$curated ==  "skin-abdominal" 
)] <-   "skin of abdomen"  
map_all$curated[which(map_all$curated ==  "skin-back"
)] <-    "skin of back" 
map_all$curated[which(map_all$curated ==  "skin-leg"
)] <-    "skin of leg"




#CARTILAGE ELEMENT 
map_all$curated[which(map_all$curated %in% cartilage_element
)] <-    "cartilage element"


#MESENTERY 
map_all$curated[which(map_all$curated %in% mesentery
)] <-    "mesentery"


#PLEURA
"pleura"  
map_all$curated[which(map_all$curated ==  "mesothelium-pleural"
)] <-   "mesothelium of pleural cavity"      


# MUSCLE
"muscle tissue" 
map_all$curated[which(map_all$curated ==  "skeletal muscle" 
)] <-   "skeletal muscle tissue"  # "skeletal muscle organ"     
map_all$curated[which(map_all$curated == "soft tissue-muscle-smooth"
)] <-  "smooth muscle tissue"    

# ADIPOSE TSSUE
"adipose tissue"
map_all$curated[which(map_all$curated ==  "skeletal muscle" 
)] <-   "skeletal muscle tissue"  # "skeletal muscle organ"     
map_all$curated[which(map_all$curated == "soft tissue-muscle-smooth"
)] <-  "smooth muscle tissue"    
map_all$curated[which(map_all$curated == "lateral of psoas muscle"
)] <-  "psoas muscle"


# THYROID
map_all$curated[which(map_all$curated %in% thyroid
)] <- "thyroid" 



# EYE

map_all$curated[which(map_all$curated %in% eye
)] <-"eye"


# PELVIC CAVITY
map_all$curated[which(map_all$curated %in% pelvic_cavity
)] <-  "pelvic cavity"  

# BONE ELEMENT
"bone element" 
map_all$curated[which(map_all$curated == "t8 vertebrae"
)] <-  "thoracic vertebra 8"  
map_all$curated[which(map_all$curated %in%  c("pelvis-right", "pelvis right side" , "pelvic bone" , "pelvis", "small pelvis", "pelvis-left")
)] <-  "bone of pelvis"  
map_all$curated[which(map_all$curated %in% c("bone (sternum)", "bone-sternum")  
)] <-   "sternum"  
map_all$curated[which(map_all$curated %in% c("bone-skull-clivus", "bone-skull")
)] <-   "skull"  
map_all$curated[which(map_all$curated ==   "bone-femur"
)] <-     "femur" 
map_all$curated[which(map_all$curated ==   "bone-femur"
)] <-     "femur" 
map_all$curated[which(map_all$curated %in% c("iliac (hip) bone", "iliac bone") 
)] <-    "ilium"  
map_all$curated[which(map_all$curated  %in% c("bone-rib", "bone (rib)")
)] <-    "rib"  
map_all$curated[which(map_all$curated ==   "ileocoecaalovergang"  
)] <-      "ileocecal junction"  

map_all$curated[which(map_all$curated ==  "bone-pelvis-left-iliac crest"  
)] <-      "iliac crest"

map_all$curated[which(map_all$curated ==  "sinonasal-ethmoid"  
)] <-     "ethmoid bone" 




# ADRENAL GLAND
map_all$curated[which(map_all$curated ==   "right adrenal"
)] <-     "right adrenal gland" 

map_all$curated[which(map_all$curated ==   "left adrenal" 
)] <-    "left adrenal gland" 


# ASCITIC FLUID
map_all$curated[which(map_all$curated ==   "ascites cell"
)] <-    "ascitic fluid"


# FECES

map_all$curated[which(map_all$curated %in% feces
)] <-    "feces" 

# ISLET
map_all$curated[which(map_all$curated =="islet"
)] <-    "islet of Langerhans"


# LUNG
# unique(unmatched[which(unmatched %in% lung)])

map_all$curated[which(map_all$curated== "lung-right middle lobe" )] <-  "middle lobe of right lung" 
 
map_all$curated[which(map_all$curated %in% c("left lower lobe lung" ,"lung-left lower lobe")  )] <-  "lower lobe of left lung"


map_all$curated[which(map_all$curated== "lung-left upper lobe" )] <- "upper lobe of left lung"
        map_all$curated[which(map_all$curated==  "lung-right upper lobe" )] <- "upper lobe of right lung"
       

map_all$curated[which(map_all$curated== "lung-right lower lobe"  )] <-  "lower lobe of right lung"

 map_all$curated[which(map_all$curated==  "lung-left lobe" )] <-  "left lung lobe"
 
 map_all$curated[which(map_all$curated==  "lung-right lobe" )] <-  "right lung lobe"

        
map_all$curated[which(map_all$curated==   "lung (lobe)" )] <-  "lobe of lung"
map_all$curated[which(map_all$curated==   "lung-pleural effusion"  )] <- "pleural effusion"  
        
map_all$curated[which(map_all$curated==  "lung-trachea"    )] <- "trachea"   

map_all$curated[which(map_all$curated %in% c("bronchial biopsy",  "lung-bronchus")  )] <- "bronchus"   

map_all$curated[which(map_all$curated == "thorax-thoracic wall" )] <- "thoracic wall"   


# INGUEN
map_all$curated[which(map_all$curated %in% inguen  )] <- "inguinal part of abdomen"  


# NOT SURE 
# map_all$curated[which(map_all$curated=="iliac left"
# )] <-  "left common iliac artery" 
# 
# map_all$curated[which(map_all$curated=="iliac right"
# )] <-  "right common iliac artery" 

map_all$curated[which(map_all$curated =="inguen"
)] <-    "inguinal part of abdomen"

map_all$curated[which(map_all$curated =="lymphoid"
)] <-    "lymphoid tissue"


map_all$curated[which(map_all$curated %in% c("pouch of douglas", "douglas")
)] <-    "Douglas' pouch" 
 

map_all$curated[which(map_all$curated %in% parotid_gland
)] <-    "parotid gland"

map_all$curated[which(map_all$curated %in% diaphragm
)] <-    "diaphragm"


map_all$curated[which(map_all$curated %in% mouth
)] <-    "mouth"

map_all$curated[which(map_all$curated == "promontory"
)] <-    "promontory of tympanic cavity"

