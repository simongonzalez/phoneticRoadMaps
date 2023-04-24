#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

writeInfoLine: "Start"

  # Get original transcription in TextGrid format............................................
  	#Specify the directory
	directory_raw$ = "01_rawInput_TGs"
  Create Strings as file list... list_raw 'directory_raw$'/*.TextGrid

	#Get files from the aligned folder from MFA outputs...................
	directory_mfa$ = "02_aligned"
  
	Create Strings as file list... list_mfa 'directory_mfa$'/*.TextGrid

	#Directory for placed the new merged files
	directory_output$ = "03_prepForAcoustics"

  numberOfFiles = Get number of strings

appendInfoLine: "Merging files (0%) |                    |"
  for ifile to numberOfFiles

		#Import Raw files................................

    select Strings list_raw
    fileName$ = Get string... ifile
     #Reads file from directory location
    Read from file... 'directory_raw$'/'fileName$'

		fullName_raw$ = selected$ ()
		type_raw$ = extractWord$ (fullName_raw$, "")
		name_original$ = extractLine$ (fullName_raw$, " ")
		name_raw$ = extractLine$ (fullName_raw$, " ")

		Rename... 'name_raw$'_raw

		fullName_raw$ = selected$ ()
		type_raw$ = extractWord$ (fullName_raw$, "")
		name_raw$ = extractLine$ (fullName_raw$, " ")

		#Import MFA outputs................................

    select Strings list_mfa
    fileName$ = Get string... ifile
    Read from file... 'directory_mfa$'/'fileName$'

		fullName_mfa$ = selected$ ()
		type_mfa$ = extractWord$ (fullName_mfa$, "")
		name_mfa$ = extractLine$ (fullName_mfa$, " ")

		Rename... 'name_mfa$'_mfa

		fullName_mfa$ = selected$ ()
		type_mfa$ = extractWord$ (fullName_mfa$, "")
		name_mfa$ = extractLine$ (fullName_mfa$, " ")

		#Select both files and save.........................
		selectObject: "TextGrid " + name_raw$
		plusObject: "TextGrid " + name_mfa$

		Merge

		Save as text file... 'directory_output$'/'name_original$'.TextGrid

		selectObject: "TextGrid " + name_raw$
		plusObject: "TextGrid " + name_mfa$
		plusObject: "TextGrid merged"

		Remove

  endfor

#Save All different formant files
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Directory for placed the new merged files
	directory_output$ = "03_prepForAcoustics"

	directory$ = "FA_nanasu_in"

appendInfoLine: "Create Intensity files (20%) |----                |"
  # Create Intensity files............................................
  Create Strings as file list... list 'directory$'/*.mp3

  numberOfFiles = Get number of strings

  for ifile to numberOfFiles
    select Strings list
    fileName$ = Get string... ifile
    Read from file... 'directory$'/'fileName$'

	fullName$ = selected$ ()

	type$ = extractWord$ (fullName$, "")
	name$ = extractLine$ (fullName$, " ")

	#Save as WAV file... 'directory$'/'name$'.wav

	To Intensity... 100 0.0 Yes
    Save as binary file... 'directory_output$'/'name$'.Intensity
	Remove
  endfor

appendInfoLine: "Create Pitch files (40%) |--------            |"
  # Create Pitch files............................................
  Create Strings as file list... list 'directory$'/*.mp3

  numberOfFiles = Get number of strings

  for ifile to numberOfFiles
    select Strings list
    fileName$ = Get string... ifile
    Read from file... 'directory$'/'fileName$'

	fullName$ = selected$ ()

	type$ = extractWord$ (fullName$, "")
	name$ = extractLine$ (fullName$, " ")

	To Pitch... 0.0 75 600
   	Save as binary file... 'directory_output$'/'name$'.Pitch
	Remove
  endfor

appendInfoLine: "Create Formant files (60%) |------------        |"
  # Create Formant files............................................
  Create Strings as file list... list 'directory$'/*.mp3

  numberOfFiles = Get number of strings

  for ifile to numberOfFiles
    select Strings list
    fileName$ = Get string... ifile
    Read from file... 'directory$'/'fileName$'

	fullName$ = selected$ ()

	type$ = extractWord$ (fullName$, "")
	name$ = extractLine$ (fullName$, " ")

	To Formant (burg)... 0.0 5.0 5500 0.025 50.0
    Save as binary file... 'directory_output$'/'name$'.Formant
    Remove
  endfor

appendInfoLine: "Create MFCC files (80%) |----------------    |"
  # Create MFCC files............................................
  Create Strings as file list... list 'directory$'/*.mp3

  numberOfFiles = Get number of strings

  for ifile to numberOfFiles
    select Strings list
    fileName$ = Get string... ifile
    Read from file... 'directory$'/'fileName$'

	fullName$ = selected$ ()

	type$ = extractWord$ (fullName$, "")
	name$ = extractLine$ (fullName$, " ")

	To MFCC... 12 0.015 0.005 100.0 100.0 0.0
    Save as binary file... 'directory_output$'/'name$'.MFCC
    Remove
  endfor

appendInfoLine: "End (100%) |--------------------|"
