### Method : Discriminant Correspondence Analysis
```
Discriminant correspondence analysis (DCA) is an extension of discriminant analysis (DA) and correspondence analysis (CA). Like discriminant analysis, the goal of DCA is to categorize observations in pre-defined groups, and like correspondence analysis, it is used with nominal variables. The main idea behind DCA is to represent each group by the sum of its observations and to perform a simple CA on the groups by variables matrix. The original observations are then projected as supplementary elements and each observation is assigned to the closest group. The comparison between the a priori and the a posteriori classifications can be used to assess the quality of the discrimination. A similar procedure can be used to assign new observations to categories. The stability of the analysis can be evaluated
using cross-validation techniques such as jackknifing or bootstrapping.
```

### Dataset : oasis_cross-sectional
```
The Open Access Series of Imaging Studies (OASIS) is a project aimed at making MRI data sets of the brain freely available to the scientific community.
It is a cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults.
This set consists of a cross-sectional collection of 216 subjects (rows) aged 33 to 96. The subjects include both men and women. 
It measures 216 subjects (rows) on 4 quanitiative variables (columns). The 5 qualitative variables have been used to interpret the data. 

Measures included in the study:

Age: Age (years)

Gender: Male or Female

Education: Years of Education - Recoded into categories from 1 (highest) to 5 (lowest)

SES: Socioeconomic status classified into categories from 1 (highest) to 5 (lowest)

MMSE: Mini-Mental State Examination score – range from 0 (worst) to 30 (best)

CDR: Clinical Dementia Rating - (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate AD)

ASF : Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix)

eTIV: Estimated total intracranial volume (cm3)

nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process 
```
