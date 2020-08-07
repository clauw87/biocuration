# ACE2 metadata
library(dplyr)
library(readr)


source("~/repolab/work/biocuration/hpo_ontology.R")

# EGAFs

ace2_empty <- read_csv("~/repolab/work/biocuration/ace2_metadata_empty.csv", col_names = F)$X2
ace2_empty <- ace2_empty[which(!(is.na(ace2_empty)))]
length(ace2_empty) # 699 
length(unique(ace2_empty)) # 65

ace2_egaf <- c(ace2$EGAF, ace2_empty)
length(ace2_egaf) # 8234
length(unique(ace2_egaf)) # 7597
length(unique(ace2_egaf)) # 7597 .. so, these were included



# With metadata 
ace2 <- read_csv("~/repolab/work/biocuration/ace2_metadata.csv")
colnames(ace2)[17] <- "EGAN"
colnames(ace2)[20] <- "EGAF"

ace2$EGAN[which(ace2$EGAN %in% ph_tags$EGAN & !(is.na(ace2$EGAN)))] # 0
length(ace2$EGAN) # 7535
length(unique(ace2$EGAN)) # 7524


# Metadata

# Sample detail
unique(ace2$sample_detail) # NA

#Case control
unique(ace2$case_control) # NA

# Gender
unique(ace2$gender) 
# [1] "male"    "female"  "unknown" "Female"  "Male"    NA       

# Organism part
unique(ace2$organism_part)  # NA

# Cell line
unique(ace2$cell_line)  # NA

# Phenotype tag
unique(ace2$phenotype) # Parent .. NA ..so, nothing

# Aren't there custom tags?


# Data

# ega
ega <- read_csv("~/repolab/work/biocuration/ace2_data.csv")
ega$variant <- paste0(ega$POS, ":", ega$REF, ">", ega$ALT) # 282
length(unique(ega$variant)) # 282

# gnomad
gnomad <- read_csv("~/repolab/work/biocuration/ace2_gnomad.csv")
gnomad$variant <- paste0(gnomad$Position, ":", gnomad$Reference, ">", gnomad$Alternate) # 282
length(unique(gnomad$variant)) # 2363

length(intersect(ega$variant, gnomad$variant)) # 109

annot <- read_delim("~/repolab/work/biocuration/ACE2_example_annotation_genomes_filtered.annovar.hg19_multianno.txt",
                    delim="\t")

unique(annot$cosmicv90)

unique(annot$GTEx_V6_tissue)

# Pathogenicity score / we could see distribution in population for missense in general
# and these ones as possible comorbility risk 
unique(annot$Polyphen2_HVAR_pred)
unique(annot$SIFT_pred)

# Population frequency by population
unique(annot$`1000g2015aug_all`)





###### second release 84 K
ace2 <- read_csv("~/repolab/work/ace2/84K_metadata_results_parsed.csv")

length(unique(ace2$ega_stable_id))# 75286 EGAF
length(unique(ace2$ega_stable_id_1)) # 41464 EGAN


# Genome> all NA

# title <- 3466
# description <- 3388
# analysis type <- 1 "SEQUENCE_VARIATION"
# sample_detail < -1 NA
# case control NA
# phenotype <- 425 unique
# samples with phenotype tag ?

# d <- distinct(ace2, EGAN, .keep_all = TRUE)
# dim(ace2)
# dim(d) #  41464    18

d <- ace2

length(unique(d$EGAN)) # 41464
length(d$phenotype[which(!(is.na(d$phenotype) | d$phenotype %in% c("N/A",  "Not applicable",   "not applicable", "Not provided", "None","unknown",  "Proband"     ,    "Parent" ) ))]) # 41464
# 1677 samples with useful phenotype tag
unique(d$phenotype[which(!(is.na(d$phenotype) | d$phenotype %in% c("N/A",  "Not applicable",   "not applicable", "Not provided", "None","unknown",  "Proband"    ,     "Parent" ) ))]) # 41464
# 416 unique values


# summarize unique non NA values per colmums
n <- colnames(ace2)
n[6] <-"EGAZ"
n[7] <- "title"
n[8] <- "description" 

d<- ace2
v <- mclapply(1:ncol(d), function(c) {
      unique(d[which(!(is.na(d[c]))),c])
}, mc.cores = 4)
l <- unlist(mclapply(1:ncol(d), function(c) {
       length(unlist(unique(d[which(!(is.na(d[c]))),c])))
}, mc.cores = 4))

nl <- unlist(mclapply(1:ncol(d), function(c) {
        length(unlist(d[which(!(is.na(d[c]))),c]))
}, mc.cores = 4))

names(l) <- n

datr <- data.frame(tag=n, val= l, stringsAsFactors = F)
p <- ggplot(data=datr, 
         mapping= aes(x=tag, y=val)) +
        geom_col() +
        theme(title = element_text(size=3, vjust = 0.5)) +
        labs(x="") +
        labs(y="Unique not NULL values") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        theme(legend.position = "top") +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1)) +
        geom_text(label=datr$val, vjust=-0.1, size=2.5) 
p
        

# Samples with value

# phenotype
length(unique(filter(d, !(is.na(phenotype)) & phenotype != "N/A" & phenotype != "Not applicable" &  phenotype != "not applicable" & phenotype != "Not provided" & phenotype !=  "None" & phenotype != "unknown" & phenotype !=  "Proband"   & phenotype !=  "Parent" )$EGAN))
# 1676

# EGAZ
length(unique(filter(d, !(is.na(ega_stable_id)))$EGAN)) # 41464

# subject
length(unique(filter(d, !(is.na(subject_id)))$EGAN)) #  36723

# gender
length(unique(filter(d, !(is.na(gender)))$EGAN)) #  37529


j <- unlist(mclapply(1:ncol(ace2), function(c) {
        #unique(ace2[which(!(is.na(ace2[c]))),c])
        length(unique(filter(ace2, !(is.na(ace2[c])) & 
                                    ace2[c] != "N/A" & ace2[c] != "Not applicable" &  ace2[c] != "not applicable" & ace2[c] != "Not provided" & ace2[c] !=  "None" & ace2[c] != "unknown" & ace2[c] !=  "Proband"   & ace2[c] !=  "Parent" 
                             & ace2[c] !=  "Unknown" )$EGAN)) # 41464
        
}, mc.cores = 4))
names(j) <- n


datr <- data.frame(tag=n, val= j, stringsAsFactors = F)
p <- ggplot(data=datr, 
            mapping= aes(x=tag, y=val)) +
        geom_col(fill="black") +
        theme(title = element_text(size=3, vjust = 0.5)) +
        labs(x="") +
        labs(y="Samples (EGAN) with useful values") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        geom_text(label=datr$val, vjust=-0.1, size=2.5) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 1)) 
         
p
pdf("/Users/claudiavasallovega/repolab/work/ace2/metadata.pdf",  height =5.83,  width =8.27)
p
dev.off()

# EGANs with phenotype


fil <- filter(ace2, EGAN %in% unique(filter(d, !(is.na(phenotype)) & phenotype != "N/A" & phenotype != "Not applicable" &  phenotype != "not applicable" & phenotype != "Not provided" & phenotype !=  "None" & phenotype != "unknown" & phenotype !=  "Proband"   & phenotype !=  "Parent" )$EGAN))
# Translate from HP ontology
fil_labels <- sapply(fil$phenotype, function(p) { 
        ifelse(p %in% tobo$id, tobo$label[which(tobo$id == p)], p)}
        )

count <- as.data.frame(table(unlist(fil_labels)), stringsAsFactors = F)
head_count <- head(arrange(count, desc(Freq)), 10)
