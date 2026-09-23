## Code and auxiliary files for "Distinct Age Subpopulations in Opioid-Detected Overdose Deaths: A Multi-State Analysis of Age Distribution Patterns, 2009–2023"

If you have any questions please reach out to a.ram@yale.edu. Please note that I cannot share any data as it requires prior approval to use.  

Files are as follows: 

* age\_overdose.Rmd -- My main analysis on age in CT
* age\_overdose\_mass.Rmd -- My main analysis on age in MA [not as detailed as CT to facilitate comparison with data from other non-CT states]
* mass\_overdose\_eras.Rmd -- My MA analysis grouped by overdose crisis era, instead of done year-by-year
* age\_overdose\_vt.Rmd -- My main analysis on age in VT [level of detail is the same as Mass]
* age\_overdose\_nc.Rmd -- My main analysis on age in NC [same level of detail as MA and VT]
* age\_overdose\_nd.Rmd -- my main analysis on age in NH
* ocme\_old\_mass and ocme\_old\_testing.Rmd have analyses for CT/MA from 2009-2017 if that's what you need, but you are probably better served at looking at ct\_ma\_analysis.Rmd
* analysis\_explanation.Rmd gives a plain-language explanation of the main analysis, so in case the comments in the code aren't legible enough to do your own analysis this provides a more through description of what I'm doing.
* ct\_ma\_analysis.Rmd has additional analysis for CT/MA
* data\_preprocessing/ -- A self-contained directory to preprocess all data used
  * ocme\_processing.R processes OCME data
  * mass\_processing.R processes Mass death data
  * vt\_processing.R processes VT death data
  * nc\_processing.R processed NC death data
  * nh\_processing.R processed NH death data
  * ny\_processing.R processed NY death data
  * sword\_processing.R processes SWORD data
  * sword\_geocoding.Rmd reverse geocodes SWORD data so we can get the town for each incident
  * city\_county\_processing.R processes publicly available city/county population data
  * processed\_data/ -- a directory with preprocessed data
* pop\_files/ -- A directory with files with publicly available population information. Note that the 2022/2023 county data is manually tabulated from city population estimates using [this](https://www1.ctdol.state.ct.us/lmi/misc/counties.asp). 
* images/ -- All the plots I have generated. Non-CT images are in folders separated by state. 
