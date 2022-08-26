# DataTK
Set of data manipulation tools.
  
### Author  
- Nicolas Chanard \<nicolas.chanard@univ-tlse3.fr>  
  
### Version  
- 0.1.0  
  
### Depends  
- R (>= 4.1)

### Imports  
- DevTK
- dplyr
- magrittr
- Matrix
- methods
- S4Vectors
- tibble

## 1. Instalation  
```
devtools::install_github("Nchanard/DataTK")
```
  
## 2. Documentation  
https://nchanard.github.io/DataTK/
  
## 3. Standard  
### A. Variables  
**Names**  
Variable names must be written according to the camelCase convention. ***i.e.*** entirely in lower case except for the first letters of the second and subsequent words.
After the name of a variable try to put an extension separated by a dot. This extension informs about the object class of the variable. The extension must be written entirely in lower case and must be an abbreviation. For variables that have several possible object classes, you could separate the extensions by an underscore.  
  
**Extensions**  
- .chr \<character> ...  
- .pth \<character> File path  
- .dir \<character> Directory path  
- .num \<numeric> ...  
- .int \<numeric> Integer  
- .ndx \<numeric> Position Number  
- .tim \<numeric> Time  
- .fac \<factor> Factor  
- .idn \<factor *or* character *or* numeric> Identifier (unique)  
- .lgk \<logical *or* function> Logical expression  
- .bln \<logical> Boolean  
- .fun \<function> Function  
- .any \<any> Any  
- .lst \<list> ...  
- .nst \<list> Nested List  
- .{*}_vec \<*> Vector of an other specific class  
- .{*}_lst \<list> list of an other specific class  
- .dtf \<data.frame> ...   
- .tbl \<tibble> ...  
- .mtx \<matrix> ...  
- .spm \<dgCMatrix *or* dgRMatrix *or other Matrix format*> Sparse matrix  
- .gnr \<GRanges> ...  
- .gnp \<Pairs> GeNomic ranges Pairs  
- .gni \<GInteractions> GeNomic Interactions  
- .cmx \<ContactMatrix> Contact Matrix  
- .ggp \<gg *or* ggplot> ggplot object  
- .col \<character *or* numeric *or* list> a color in any format  
  
### B. Functions  
Functions names must be written according to the camelCase convention. ***i.e.*** entirely in lower case except for the first letters of all words.
The name of a function should be an action, a verb or a question.  
