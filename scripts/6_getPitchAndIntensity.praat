#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Populates the CSV file from R and extracts features in this script

filename$ = "nanasu_append"

Read Table from comma-separated file... 'filename$'.csv

selectObject: "Table 'filename$'"
tableCols = Get number of rows

writeInfoLine: "Start"
for i from 1 to tableCols
	#appendInfoLine: i

#Get time information...........................................
selectObject: "Table 'filename$'"

	tmpFile$ = Get value... i speaker
	tmpTime = Get value... i time

#Get pitch......................................................
selectObject: "Pitch " + tmpFile$
	
	tmpPitchValue = Get value at time... tmpTime Hertz Linear

#Get intensity..................................................
selectObject: "Intensity " + tmpFile$
	
	tmpIntensityValue = Get value at time... tmpTime Cubic

#Get formants...................................................
selectObject: "Formant " + tmpFile$
	
	f1Value = Get value at time... 1 tmpTime hertz Linear
	f2Value = Get value at time... 2 tmpTime hertz Linear
	f3Value = Get value at time... 3 tmpTime hertz Linear

#Get mfccs...................................................
selectObject: "MFCC " + tmpFile$

	frame_number = Get frame number from time... tmpTime
	mfcc_value_1 = Get value in frame... frame_number 1
	mfcc_value_2 = Get value in frame... frame_number 2
	mfcc_value_3 = Get value in frame... frame_number 3
	mfcc_value_4 = Get value in frame... frame_number 4
	mfcc_value_5 = Get value in frame... frame_number 5
	mfcc_value_6 = Get value in frame... frame_number 6
	mfcc_value_7 = Get value in frame... frame_number 7
	mfcc_value_8 = Get value in frame... frame_number 8
	mfcc_value_9 = Get value in frame... frame_number 9
	mfcc_value_10 = Get value in frame... frame_number 10
	mfcc_value_11 = Get value in frame... frame_number 11
	mfcc_value_12 = Get value in frame... frame_number 12

#Insert values in the table.....................................
selectObject: "Table 'filename$'"

	Set numeric value... i pitchValue tmpPitchValue

	Set numeric value... i intensityValue tmpIntensityValue

	Set numeric value... i formant_1 f1Value
	Set numeric value... i formant_2 f2Value
	Set numeric value... i formant_3 f3Value

	Set numeric value... i mfcc_1 mfcc_value_1
	Set numeric value... i mfcc_2 mfcc_value_2
	Set numeric value... i mfcc_3 mfcc_value_3
	Set numeric value... i mfcc_4 mfcc_value_4
	Set numeric value... i mfcc_5 mfcc_value_5
	Set numeric value... i mfcc_6 mfcc_value_6
	Set numeric value... i mfcc_7 mfcc_value_7
	Set numeric value... i mfcc_8 mfcc_value_8
	Set numeric value... i mfcc_9 mfcc_value_9
	Set numeric value... i mfcc_10 mfcc_value_10
	Set numeric value... i mfcc_11 mfcc_value_11
	Set numeric value... i mfcc_12 mfcc_value_12

endfor

selectObject: "Table 'filename$'"

Save as comma-separated file... 'filename$'.csv






