library(readr)
library(stringr)
library(dplyr)


#ph_tags1 <- read_delim("~/Downloads/raw_data_sample_tag.txt", delim = "|", col_names = F )
# colnames(ph_tags1) <- c("EGAN", "tag", "value")
# head(ph_tags1)

#ph_tags2 <- read_delim("~/Downloads/raw_data_sample_tag2.txt", delim = "|", col_names = F )
# colnames(ph_tags2) <- c("EGAD", "EGAN", "tag", "value")
# head(ph_tags2)
# nrow(ph_tags2) # 5971257

ph_tags <- read_delim("~/Downloads/raw_data_sample_tag3.txt", delim = "|", quote=" \"", col_names = F, 
                      escape_backslash = T, escape_double = T )
# Warning: 4208 parsing failures.
# row col           expected    actual                                   file
# 108454  -- 4 columns          5 columns '~/Downloads/raw_data_sample_tag3.txt'
# 108463  -- 4 columns          5 columns '~/Downloads/raw_data_sample_tag3.txt'
# 109176  -- 4 columns          5 columns '~/Downloads/raw_data_sample_tag3.txt'
# 183345  X3 delimiter or quote           '~/Downloads/raw_data_sample_tag3.txt'
# 183345  X3 delimiter or quote P         '~/Downloads/raw_data_sample_tag3.txt'

colnames(ph_tags) <- c("EGAD", "EGAN", "tag", "value")
head(ph_tags) # 8435312 
dim(ph_tags)

# Number of Studies
u_studies <- unique(ph_tags$EGAD)
length(u_studies) # 4914... 4931 ...6137

# Number of Samples
u_samples <- unique(ph_tags$EGAN)
length(u_samples) # 700791 ... 710681 ... 858618 


# TAGS
u_tags <- unique(ph_tags$tag)
length(u_tags) # 1612 ..1608 .. 1027


# VALUES
u_values <- unique(ph_tags$value)
length(u_values) # 727115
" BLAF" %in% u_values
" blaf" %in% u_values
" Bronchoalveolar lavage fluid " %in% u_values 
sum(str_detect(string= u_values , pattern = "alveolar"), na.rm = T)
u_values[str_detect(string= u_values , pattern = "alveolar")]
u_values[str_detect(string= u_values , pattern = "alveolar")]


unique(filter(ph_tags, value %in% u_values[str_detect(string= u_values , pattern = "alveolar")])$tag)
##---------------------------------------##---------------------------------------##---------------------------------------
# Group/Homogenize tags by meaning 
age <- c(" Age_at_diagnosis ", " Age at diagnostic " , " Age at diagnosis ", " age_at_diagnosis " , " age at diagnosis ", " Age_at_sampling ",  " Age ", " Race " , " Sample age " , 
         " Age at sample collection " , " sample_age " , " donor_age_unit ", " donor_age " ,
         " DONOR_AGE_UNIT " , " DONOR_AGE ", " age ",
         " sampling date ", # here
         " age (years) ", " Age at Treatment ", " age_at_blood_collection ", " age_at_diagnosis_years " ,
         " AGE ", " Age_Unit ", " age_first_cancer " , " date_of_sample_collection ", " date_of_sample_extraction ",
         " Years_diabetes ", " birthdate ", " sampling_date ", " gestional_age "  )
length(unique(filter(ph_tags, tag%in% age )$EGAN)) # 27505


gender <- c(" DONOR_SEX ", " sex ", " Gender ", " gender ",  " Sex ", " ArrayExpress-Sex "  )
length(unique(filter(ph_tags, tag%in% gender )$EGAN)) # 662830


case_control <- c( " affectedStatus ", " caseOrcontrol " , " case_control_pair_id " , " case-control ", " ArrayExpress-DiseaseState " , " Normal"  , " case_or_control ", " caseOrControl ", " case or control ", " case_control ",
                  " DONOR_HEALTH_STATUS_ONTOLOGY_URI " , " DONOR_HEALTH_STATUS " ,
                  " relation ", " affected_unaffected ", " sample_attributes ", " ADDITIONAL_NOTE "  )
length(unique(filter(ph_tags, tag%in% case_control )$EGAN)) # 662830

ethnicity <- c( " ethnolinguistic group "  , " Caucasian ", " origin " , " population ", " region ", " ethnicity ", " race ", " Ethnicity " , " Race " , " DONOR_ETHNICITY "  )
#length(unique(filter(ph_tags, tag%in% ethnicity )$EGAN)) # 137962

blood_type <- c(" blood_type " )

country <- c(" district ", " geographic location "   , " city " , " country_of_origin " , " geographical_region ", " country_of_origin " , " country ", " Country of origin ", " characteristics region ", " Tissue Source " , " location ")
#length(unique(filter(ph_tags, tag%in% country )$EGAN)) # 35781

relatedness <- c( " aunt_uncle_of_patient ", " BIOLOGICAL_REPLICATES " , " child_of_patient "  , " this patient is the niece/nephew of patient with id ", " this_patient_is_parent_of_patient_with_id " , " niece_nephew_of_patient ", " aunt_uncle_of_patient_with_id "  , " this_patient_is_sibling_of_patient_with_id ", " this patient is the aunt / uncle of patient with id ", " this patient is the grandparent of patient with id " , " parent_of_patient_with_id ", " this patient is the parent of patient with id ", " twin_no ", " this_patient_is_twin_of_patient_with_id ", " pedigree_description " , " proband ", " father " ,  " mother " , " pedigree_type ", " this patient is the grandchild of patient with id ", " this_patient_is_child_of_patient_with_id ", " this patient is the child of patient with id " , " this patient is the sibling of patient with id " , " this patient is the aunt/uncle of patient with id " , " relationship " , " pedigree_type " , " FAMILY ID "  , " FAMILY ROLE " ," child_of_patient_with_id ", " child_of_patient_with_id ", " sibling_of_patient_with_id " , " parent_of_patient ", " sibling_of_patient ", " family ", " familiarity ", " heredity " , " TWIN_PAIR_ID " , " Pedigree " , " is_trio ", " consanguinity ", " family_id ", " father_id ", " mother_id "  )
#length(unique(filter(ph_tags, tag%in%relatedness)$EGAN)) # 2423

unique(filter(ph_tags, tag==  " Sample origin " )$value)
#---------------------------------------
sample_type <- c(" Xenograft ", " Tissue_type " , " Is_tumor " , " Tissue Type ", " ppcg_sample_type ", " tissue type ", " diseased tissue type ", " Material ",  " material " , " ppcg sample type ", " sample_type ", " TISSUE_TYPE ", " tissue_type ", " BIOMATERIAL_TYPE ",
                  " specimen_type " , " biomaterial_type ",  " SampleOrigin ", " sample type ", " sample_description ", " Sample Description ", 
                 " xenograft ", " disease_site ", " sample_type_detail ", " Lund " , " Lund2 ", " SAMPLE_TYPE " , " Tumour " ,
                 " sample.type ", "Biopsy site\" | \"Prostate gland" , " Tissue Source ", " samples_group " ,
                 " Cell_line ", " Primary_Tissue " , " Cell_line ", " tumor location " , " sample source ",
                 " sample_type2 ",  " source ", " LINE ", " tissue ", " Isolation source " )
length(unique(filter(ph_tags, tag%in% sample_type )$EGAN)) # 192801

anatomy <- c( " Sample origin " , " Tissue Type ", " lymph_node " , " tissue ", " sample_type2 ", 
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
             " TISSUE_DEPOT_ONTOLOGY_URI ", " CELL_TYPE_ONTOLOGY_URI ", " TISSUE_TYPE_ONTOLOGY_URI ")
length(unique(filter(ph_tags, tag%in% anatomy )$EGAN)) # 187949
length(unique(filter(ph_tags, tag == " organism_part ")$EGAN)) # 92845

procedure <- c(procedure, " tissue_processing_method ")

cell_type <- c(" Cell type ", " cell type ", " target cell type ", " cell category ", " cell_type ", 
               " PASSAGE_IF_EXPANDED ", #here
               " CELL_TYPE " ," cellLine "," cell_line ", " Cell_line ", " cell line ID ", " LINE ",
               " ArrayExpress-CellType " 
                )
length(unique(filter(ph_tags, tag %in% cell_type)$EGAN)) # 83535

molecular_type <- c(" Sample type ",  " dna_source ", " data type "   )


unique(filter(ph_tags, tag== " ppcg_sample_type ")$value)


#---------------------------------------
molecular_markers <- c(" CD31_IHC ", " CD8A_IHC ", " tumor cd8 count ",  " cell_population " ,
                       " MOLECULE ", #here
                       " MARKERS ",  " attribute_27_ubiquitin_positive ",  " diagnostic_psa ", " Sorted cell phenotype " )

histology_feature <- c(" SarcomatoidFeatures ", " ICSCORE " , " Neoantigen burden per MB ", " Baseline ECOG Score ",
                       " Enrollment IC ", " IC Level ", " TC Level ", #here 
                       " Differentiation ",  " Grade " , " Histology ", " Lesion ",
                       " sample_description ", " Sample Description ", " histology ",  
                       " TISSUE_DIAGNOSIS ", " histological stage ",   " gleason_score ", 
                       " gleason_total ", " histology_first_cancer " , " histological_stage ",
                       " pleomorphism ", " cytology "
                       )

disease <- c(" attribute_25_nft_dementia " , " depression " , " attribute_6_fh_disease ", " attribute_4_clindx ", " diagnosis " , 
             " Met Disease Status " , " disease_ontology ", " disease ", 
             " Pathology ",  " Pathology Occurrence ", " DISEASE ", 
             " Primary Sclerosing colangitis (PSC) ", #here
             " TISSUE_DIAGNOSIS ", " Tissue.Diagnosis ",  " Tissue_Diagnosis " ,
             " phenotype_name ", " prior_phenotype_name ", " disease_type ",
             " EGA-Disease ",  " schizophrenia " , " Parkinson's ", " attribute_9_cjd_copathology ", 
             " primarymelanomatype " , " sample source " , " attribute_22_picks_disease " , " donor_health_status " 
             )
#length(unique(filter(ph_tags, tag%in% disease )$EGAN)) # 6886

disease_attributes <- c(" attribute_3_diseaseduration ", " attribute_1_ageonset ", 
                        " progression free survival ", " progression to T2+ ", 
                        " disease state ", " disease_state " , " laterality ", 
                        " mode_of_inheritance ", " Mode of Inheritance ", " dysplasia ", 
                         " Years_diabetes ", " HemoPositive ", " TYPE " 
                        )

analysis_score <- c(" EORTC risk score NMIBC ", " ICSCORE ", " FUHRMAN ")

cancer_related <- c( " Tumor size at endoscopy (mm) " , " Disease condition " , " tumor volume ", " glioma type " , " differentiation grade ", 
                    " abc classification ", " Stage ",  " MSKCC ", " tumor size " ,
                    " tumor grading " ,  " tumor stage ", " growth pattern ", " subtype ", 
                    " primary type ",  " stage ", " tumor cd8 count ",
                    " progression status ",  #here
                    " WHO Grade ",  #here
                    " TCGA Subtype ", " FUHRMAN ", " EORTC risk score NMIBC ", " Stage " ,
                    " Metastasis", " LIVERMET ", " Kidney ",
                    " Lund " , " Lund2 ", " Kataegis " ,  " who_tnm_stage ",  " size of tumour ",
                    " pleural invasion " , " vascular invasion ", " tnm_stage ", " Tumour Cellularity ",
                    " tumor_type ", " tumor size (mm) ", " dysplasia ", " metastatic_site ",
                    " Metastatic_Tissue " , " Pathology T " ,   " gleason_score ", " gleason_total ",
                    " ClinStageT " , " PathStageT ",  " TNM.code " , " Tumor.differentiation ",
                    " Tumor type ", " Tumour_subtype ", " Molecular Subtype ", " primarymelanomatype ",
                    " tumor_stage7thedition " , " HRD ", " Gleason Score ", " gleason_score ", 
                    " histology_first_cancer " , " histological_stage " ,  " Molecular.subtype " , 
                    " pleomorphism "
)

unique(filter(ph_tags, tag==   " histone_mutation " )$value)
#---------------------------------------
genes_markers <- c(" TP53 ", " BAP1 ", " MTOR ", " SETD2 ",  " VHL ", " PBRM1 ", " TNB ", " TMB ", 
                  " attribute_29_apoe_genotype ", " CNA chr7 gain/chr10 loss ", " CNA CNA1p19q ", 
                  " chr.15_summary " , " chr.9_summary ",  " chr.5_summary ", " TP53_summary ", 
                  " WWOX_summary ", " CYLD_summary ", " TRAF3_summary ", " chr.13_summary ",  
                  " DIS3_summary ",  " RB1_summary " , " chr.12p_summary " , " chr.1q_summary ",
                  " CKS1B_summary " , " FAM46C_summary ", " FAF1_CDKN2C_summary ", " NMFCluster ", 
                  " MYCtranslocation ", " Translocation_consensus ", " MSI status ", " ER Status ",
                  " attribute_7_c9orf72_status ", " attribute_17_alpha_synuclein_braak ", " attribute_5_path_dx " ,
                  " antibody ", " IDH mutation ", " FMOne mutation burden per MB ", 
                  " ER.status " , " HER2.status ", " MLH1 copy number status ", " ighv_mutation_status ", " ighv_homology ",
                  " MSI.Status ", " Mutation_2 ", " RB1_Status ", " Mutation_1 ", " genotype ", " BRAF_mutation ",
                  " microsatellite status ", " factor_histone_mark ", " Germline BRCA Status ", " attribute_10_cjd_prpgenotype ",
                  " attribute_11_cjd_prpresisoform ",  " histone_mutation ")

history_intervention <- c( " PriorNephrectomy ", " prior targeted therapy ",  " prior immunotherapy " )

treatment <- c(" TREATMENT " ," BCG treatment ",  " treatment subtype " , " Intravesical BCG administered ", 
               " Treatment " , " ARM ", " treatment ", " treatment ", 
               " Received platinum ",  # here > is this a treatment
               " treatment protocol "  , " therapy ", " EGA-Treatment ", " receivedbevacizumab ",
               " bevacizumab " ,  " carboplatin "  , " cetuximab "   ,  " erlotinib ", " etoposid " ,
               " gemcitabin ", " paclitaxel ", " antidepressants " , " treatmentgroup ", " inulin ", " Dose ",
               " Prior_therapy ", " IFN-treated ", " Recent-GC-treated ", " Patient Treated with TMZ " ,
               " Sample Treated with TMZ ",  " treatment_arm ", " presurgery drug exposure ", " study_arm_1startswprobiotic_2startswplacebo " 
               )


intervention <- c(" cystectomy " ,  " Surgery: extent of resection " )

response <- c(" BestResponse ", " Best Confirmed Overall Response ", " binaryResponse " )


clinical_findings <- c(" rx status ", " elevated ldh ",  " Immune phenotype ", " status ", 
                       " ClinicalChorioamnionitis " , " Csection ", " LDH_high " , " HRD ", 
                       " Sepsis ", " disease_status ", " donor_health_status "  )

measures <- c( " lymph ",  " lymph_perc ", " neut ", " neut_perc ",
               " mono " , " mono_perc "," luc " , " luc_perc ",
               " eos ", " eos_perc ",
               " Baso_Perc ", " Eos_Perc ", " Neut_Perc ", " LUC_Perc ", 
               " Lymph_Perc ", " Mono_Perc ", " Baso " , " Eos " , " Neut ",
               " Lymph ", " Mono ",  " LUC ",
               " baso ", " baso_perc "," eos ", " eos_perc "," luc "," luc_perc ",                                                         
               " lymph ", " lymph_perc ", " mono ", " mono_perc ", " neut ", " neut_perc ",
               " human_dna_perc ", " os " , " tumor size (mm) ", " Total_cholesterol ",
               " Cortisol " , " Insulin " ,  " glycemia " , " BMI ", " Chronotype ", 
               " serum alkaline phosphatase (u/l) ", " average faecal cell count (cells/g) ",
               " serum crp (mg/l) ", " moisture content (%) ",  " faecal calprotectin (�g/g) ",
               " HOMA_IR " , " HbA1c ", " WBCMax ", " GlucoseAF ", " CutoffIL6 ", " IL6_LA ", " GramAF ",
               " LymPerMax ", " LymCountMax ", " LymPerBirth ", " LymCountBirth ", " ANCMax " ,  " ANCadm ",
               " ANCadmPc ",  " WBCMax ", " WBCBirth " , " WBCadm " , " CRPadm ", 
               " Total_cholesterol ",  " LDL " ,  " HDL ",  " Triglycerides ",
               " EFO_0000246 ", " EFO_0004340 ", " PD.L1_H_score "  )


timepoints <- c(" time point in days ", " time point ", " time to progression (days) " ,  
                " POST_MORTEM_INTERVAL ", " differentiation start date ", " blood_collection " , 
                " timepoint " , " follow_up_length_in_days " , " follow_up_length_in_years "  
)

death_status <- c(" age at death " , " attribute_2_agedeath ", " DONOR_LIFE_STAGE ", " cause_of_death ")


ontologies <- c(" ontology1 ", " ontology2 ", " ontology3 ", " ontology4 ", " ontology5 ",
                " ontology6 ", " ontology7 "," ontology8 "," ontology9 ", " ontology10 ",
                " ontology11 ", " ontology12 ",  " phenotype_ontology ", 
                " SAMPLE_ONTOLOGY_URI ", " DISEASE_ONTOLOGY_URI ",
                " DONOR_HEALTH_STATUS_ONTOLOGY_URI "," ORIGIN_SAMPLE_ONTOLOGY_URI ",
                " TISSUE_DEPOT_ONTOLOGY_URI ", " CELL_TYPE_ONTOLOGY_URI ", " TISSUE_TYPE_ONTOLOGY_URI "
                )

phenotype <- c( " Phenotype ", " phenotype ", " phenotype2 ",  " phenotype3 ", 
                " phenotype_2 " , " Phenotype2 ", " prior_phenotype " ,  " phenotype_url "  )
#length(unique(filter(ph_tags, tag%in% phenotype )$EGAN)) # 644819

unique(filter(ph_tags, tag==   " phenotype_url "    )$value)
#---------------------------------------
submission <- c(" ENA-SUBMISSION-TOOL ")

sequencing_depth <- c(" sequencing.depth ")

experiment <- c( " histone_mutation ", " library_type "  , " target_gene ", " methylation_subclass " , "Sequencing Type\" | \"whole genome" , " data type "  , " truseq adapter " , " read_length ", " sequencing_type ", " paired_sample " , " assay ", " Sequencing method ",  " CARS " , " MACHINE ", " LIBRARY " , " LANE " , 
                " FLOWCELL ", " INSTRUMENT_MODEL ", " PLATFORM " , " Sample type ",  " read length " , 
                " sequencing_type ", " genome_coverage ", " library ",  " ASSEMBLY_ID ",  " sequencing instrument "  )


procedure <- c(" preservation ", " pipeline sample uuid ", " COLLECTION_METHOD ", " Sample collected pre-platinum ",
               " extract protocol ",  " CULTURE_CONDITIONS ", "Enrichment Type\" | \"Hybridization in Solution",
               "Size of enriched Region\" | \"3.9Mb", "Tissue Preparation Type\" | \"Snap frozen" )

molecular_type <- c(" Sample type ",  " histone_mutation ")


sample_id <- c(" bio_sample_id ", " bioSampleId ", " sample id ", " AnonPatID " , " AnnoRNASampleID " , " AnnoTumorWESID " , " AnnoNormalWESID ",
               " barcode1 ", " barcode2 " , " SUBMISSION_ALIAS ", " icgc_specimen_id " , " icgc_sample_id ",
               " aliquot_id/sample_uuid ", " ppcg sample id "  ," submitter_specimen_id ",
               " Specimen ID " , " SampleID ", " submitter_sample_id ", " sample_id ", " Sample ID "
)

subject_id <- c(" DONOR ", " Donor ID ", " subject_id " , " Donor Id "  ,  " donor_id " , " patient_id ", " ppcg id ", " submitter_donor_id " , " icgc_donor_id ", " anonymized_name "  )

dataset_cohort <- c(" experiment_dataset_id ", " cohort ", " cohort_id " )

id <- c(" externalID ", " other-identifiers "  )

biosample_provenance <- c(" BIOMATERIAL_PROVIDER ", " brain_bank ")

centre_project <- c(" clinical center ", " center ", " centre id ", " centerName ", 
                    " icgc_project_code ", " Project ", " project ",
                    " brain_bank ")


unique(filter(ph_tags, tag==  " attribute_19_tdp43_pathology "  )$value)
##---------------------------------------##---------------------------------------##---------------------------------------
groups <- c(age, gender, case_control, ethnicity, country, relatedness, treatment, cancer_related, 
            sample_type, anatomy, histology_feature, molecular_markers, sample_id, subject_id, 
            dataset_cohort, genes_markers, disease, history_intervention, measures, intervention, 
            response, disease_attributes, clinical_findings, timepoints, death_status, cell_type, 
            procedure, ontologies, centre_project, submission, sequencing_depth, id, experiment, 
            phenotype, biosample_provenance, centre_project )
length(unique(groups)) #  582



# Other tags unclassified..fish here
others <- u_tags[!(u_tags %in% groups)]
length(others) # 1035


# -----------------------------------------------------------------------------------------------
# Tags containing tabs |
vtab_tags <- others[sapply( others, function(l) {
        str_detect(string = l , pattern =  "\\v")})
        ]
length(vtab_tags) #  593



# Tags containing line skip \n
ntab_tags <- others[sapply( others, function(l) {
        str_detect(string = l , pattern =  "\n")})
        ]
length(ntab_tags) #  593
















# VALUES
u_values <- unique(raw_data_sample_tag$value)
length(u_values) # 94529

# -----------------------------------------------------------------------------------------------

# case_control values in sample_type
case_control_values <- c(" normal tissue", " diseased tissue", " Normal - blood derived", " Normal - lymph node",  " normal from other tissue"   )

# anatomic site values in sample_type tags> tags " ppcg sample type ", " tissue_type " 
anatomic_site_values <- c(" PBMC", " blood", " liver", " Whole blood" , " RNA from iPSC derived macrophages",  
                          " DNA from normosmic", # here
                          " DNA from whole blood",
                          " Total RNA from human iPSC-derived sensory neurons", 
                          " macrophages derived from iPS",
                          " Normal - lymph node",
                          " Normal - blood derived",
                          " blood (whole)")

 # experiment values in sample_type
experiment_values <- c(unique(filter(ph_tags, tag == " Sample type " )$value), 
                       " RNA-seq", " ChIP", " Exome-seq", " ChIP DNA", " DNA-seq", " ChIP STARR input",
                       " Pooled Nextera libraries generated from intestinal organoid single cells",
                       " 10x GEX for pooled 4 lanes PE 26/8/91",
                       " 10x TCR for pooled single lane PE 150/8/150",
                       " 10x Cell surface protein library for pooled 4 lanes PE 26/8/91 (1/10 concentration)")
 
# molecular sample values in sample_type
molecular_type_values <- c(" Total RNA", " DNA from whole blood",
                       " genomic DNA" , 
                       " RNAi induced genomic T brucei DNA samples grown in each drug for 7-10 days",
                       " RNA from iPSC derived macrophages",
                       " DNA from normosmic", 
                       " Total RNA from human iPSC-derived sensory neurons",
                       " Amplicons", 
                       " DNA stock" , 
                       " Pooled Nextera libraries generated from intestinal organoid single cells",
                       " RNA from patient with primordial dwarfism")

#true sample type in sample type / should be stuff like tissue, cell line, xenograft
sample_type_values <- c(unique(filter(ph_tags, tag == " biomaterial_type " )$value),
                        " Primary Cell", " primary cell", " primary", " Primary Tissue", 
                        " cell line", " Patient Derived Xenograft",
                        " Primary tumour - solid tissue",
                        " immortalised cell line", 
                        " primary sample",  " primary",
                        " diseased tissue",  " normal tissue" ,  " normal from other tissue" # these are primary tissue
                        )
# add 
filter(ph_tags, tag == " BIOMATERIAL_TYPE " )$value




#" primary"

biosample_provenance_values <- c(filter(ph_tags, tag %in% biosample_provenance_tags)$value)

# disease or condition 
disease_values <- c(" RNA from patient with primordial dwarfism")
