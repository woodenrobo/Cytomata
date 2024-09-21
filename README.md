Cytomata is an open-source script collection that integrates tools and methods used for mass cytometry (CyTOF) analysis into an automated pipeline.
Author: Lev Petrov

## OMNICYTO PEOPLE GUIDE
- install R and Rstudio on your machine https://posit.co/download/rstudio-desktop/
- clone the repo, set the path to the repo folder in master.R (line 15)
``r
path_to_cytomata <- "~/DOCTORATE/Cytomata/"
``
- go here: [data](https://charitede-my.sharepoint.com/:f:/g/personal/lev_petrov_charite_de/EsfhZi47RwNNoJUOX0mpD-0BcDWFalPU2dBoNUyM4SKbTg?e=pTXe8E)
- download the /fcs/1_raw folder
- download the /meta folder
- *preserve the folder structure!* It should look like this
  /your/path/to/Cytomata_data_folder/
    |-fcs
      |-1_raw
    |-meta
- put downloaded data into the folder and set the path to the folder using the excel file in Cytomata (repo) folder. It's the first setting.
- don't change anything else, just select the part of the script in master.R up to and including this point
``r
runApp()
``
- this will launch the shiny application for you to play around
- the scripts in question here are 

/your/path/to/Cytomata/gating_application/app.R

/your/path/to/Cytomata/gating_application/www/d3_graphics.js

and

/your/path/to/Cytomata/gating_application/www/styles.css





