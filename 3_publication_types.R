# 0_publication_types.R
# publication types available on pubmed
# from https://pubmed.ncbi.nlm.nih.gov/help/#publication-types
# experienced showed that the type "clinical trial" was often not randomised so there were no groups to compare
# May 2021

pub_types = read.table(header=TRUE, sep='!', stringsAsFactors = FALSE, text='
type!include
Adaptive Clinical Trial!1
Address!0
Autobiography!0
Bibliography!0
Biography!0
Case Reports!1
Classical Article!1
Clinical Conference!0
Clinical Study!1
Clinical Trial!1
Clinical Trial, Phase I!1
Clinical Trial, Phase II!1
Clinical Trial, Phase III!1
Clinical Trial, Phase IV!1
Clinical Trial Protocol!1
Clinical Trial, Veterinary!1
Collected Works!0
Comparative Study!1
Congress!0
Consensus Development Conference!0
Consensus Development Conference, NIH!0
Controlled Clinical Trial!1
Dataset!0
Dictionary!0
Directory!0
Duplicate Publication!0
Editorial!0
Electronic Supplementary Materials!0
English Abstract!0
Equivalence Trial!1
Evaluation Study!1
Expression of Concern!0
Festschrift!0
Government Publication!0
Guideline!0
Historical Article!0
Interactive Tutorial!0
Interview!0
Introductory Journal Article!0
Journal Article!1
Lecture!0
Legal Case!0
Legislation!0
Letter!0
Meta-Analysis!1
Multicenter Study!1
News!0
Newspaper Article!0
Observational Study!1
Observational Study, Veterinary!1
Overall!0
Patient Education Handout!0
Periodical Index!0
Personal Narrative!0
Portrait!0
Practice Guideline!0
Preprint!0
Pragmatic Clinical Trial!1
Publication Components!0
Publication Formats!0
Published Erratum!0
Randomized Controlled Trial!1
Research Support, American Recovery and Reinvestment Act!0
Research Support, N.I.H., Extramural!0
Research Support, N.I.H., Intramural!0
Research Support, Non-U.S. Govt Research Support, U.S. Govt, Non-P.H.S.!0
Research Support, U.S. Govt, P.H.S.!0
Retracted Publication!0
Retraction of Publication!0
Review!1
Scientific Integrity Review!0
Study Characteristics!1
Support of Research!0
Systematic Review!1
Technical Report!0
Twin Study!1
Validation Study!1
Video-Audio Media!0
Webcast!0')

## Make rmarkdown for appendix
rmarkdown::render(input = "98_publication_types.Rmd",
                  output_format = "pdf_document",
                  output_file = 'results/appendix_included.pdf') # output to specific file
#
rmarkdown::render(input = "98_publication_types.Rmd",
                  output_format = "word_document",
                  output_file = 'results/appendix_included.docx') # output to specific file
