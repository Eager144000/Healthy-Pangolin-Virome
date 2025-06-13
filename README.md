# Healthy Pangolin Virome

## Project Structure

```
Healthy Pangolin Virome/
├── README.md
├── .gitignore
├── .venv/
├── data/
│   └── pangolin_metadata.xlsx
└── analysis/
    ├── 1_Sample characterization and phylogenetic relationships/
    │   ├── a. Pangolin sample column/
    │   ├── b. cytb phylogenetic tree/
    │   ├── Host identification.xlsx
    │   └── align_phylo_cytb.fasta
    ├── 2_Characterization of the pangolin viromes/
    │   ├── R/
    │   ├── ViralScan pipeline.png
    │   ├── Pangolin.coverage
    │   ├── Pangolin.rpms
    │   ├── Pangolin_Virus_results.csv.gz
    │   ├── Pangolin_virus_rpms.csv.gz
    │   ├── Virus_prevalance.csv.gz
    │   └── 2_RPM_pre.fasta
    ├── 3_Evolutionary relationships of mammalian-associated viruses in pangolins/
    │   ├── Genome completeness/
    │   ├── Pidentity/
    │   ├── RdRp/
    │   └── Hallmark identity of Mammal-Associated Viruses.xlsx
    ├── 4_Recombination analysis of novel pangolin viruses/
    │   ├── 1_vOTU/
    │   └── 2_recombination/
    ├── 5_coinfection/
    │   ├── correlation-plot-coverage.R
    │   ├── correlation-pearson-RPM.pdf
    │   ├── pearson_P.csv
    │   ├── cor_pearson.csv
    │   └── healthy_pangolin_virus_rpm.csv
    └── 6_Identifying viruses with zoonotic potential/
        ├── README.md
        ├── zoonotic-rank/
        ├── 2_result.predictions.csv.gz
        ├── result.all_cluster_members.csv.gz
        ├── result.explanations.csv.gz
        ├── result.feature_dendrogram.pdf
        ├── result.genome_features.csv.gz
        ├── result.predictions.csv.gz
        ├── result.similar_explanations.csv.gz
        ├── result.similar_explanations.pdf
        ├── vOTU_annotation.csv.gz
        └── vOTU_rep_seq.fasta
```

## Overview

Our study employs metatranscriptomic analysis to characterize the virome of 83 healthy pangolins, a critically understudied wildlife reservoir, and compares it to viromes from 52 diseased individuals. By integrating high-throughput sequencing, phylogenetics, recombination analysis, and machine learning-driven risk assessment, we:

1. **Expand ecological understanding** of viral communities in a keystone species, identifying 51 viral operational taxonomic units (vOTUs) across six mammalian-associated viral families.

2. **Elucidate host-microbe interactions** by revealing baseline virome composition in healthy pangolins, including immune-relevant findings (e.g., cross-species Morbillivirus recombination implicating viral adaptation).

3. **Address zoonotic risk** through predictive modeling, identifying 16 vOTUs with high human infection potential, directly linking wildlife viromes to public health preparedness.

These elements align with Microbiome's focus on microbial ecology, host-microbe dynamics, and translational implications for health and disease.

## Analysis Workflow

The analysis is organized into six main components:

### 1. Sample Characterization and Phylogenetic Relationships
- Host species identification and phylogenetic analysis
- Cytochrome b gene sequencing and phylogenetic tree construction

### 2. Characterization of Pangolin Viromes
- Viral detection pipeline using ViralScan
- Coverage and RPM (Reads Per Million) analysis
- Viral prevalence assessment

### 3. Evolutionary Relationships of Mammalian-Associated Viruses
- Genome completeness evaluation
- Sequence identity analysis
- RNA-dependent RNA polymerase (RdRp) phylogenetics

### 4. Recombination Analysis of Novel Pangolin Viruses
- Viral operational taxonomic unit (vOTU) classification
- Recombination event detection and analysis

### 5. Coinfection Analysis
- Correlation analysis between viral species
- Statistical assessment of coinfection patterns

### 6. Identifying Viruses with Zoonotic Potential
- Machine learning-based risk assessment
- Zoonotic potential ranking and prediction
- Feature analysis and explanations