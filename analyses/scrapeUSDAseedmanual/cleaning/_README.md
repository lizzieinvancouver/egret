Updated by Dan on September 9 2024.

There are 3 important files for cleaning the seed germination data.

cleanAllUsda.R is the main cleaning file that sources the other two from the source folder.

In the source folder:
cleanAllUsda_JNVER.R does the bulk of the cleaning and was made by Justin.
cleaningUSDA_ForDan.R takes columns were there are a range of values for chilling temps, or germination repsponse (eg. 5-10 degrees) and seperates the out into seperate columns for a min and max. This was made by Freddi.

The other files are for cleaning phenology, which Dan doesn't know about and someone else should update.

