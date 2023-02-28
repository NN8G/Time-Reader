/'*******************************************************************
TimeReader 042020
	-Moved preprocessing of input file to separate subroutine. And 
		instead of fixing bad characters in each line as read, make
		a temp file and fix all problems before starting print out of 
		input text file.
*******************************************************************'/
#include "string.bi"
#include "file.bi"
#Include "file fix.bas"

Dim buffer As String
Dim f As Integer
Dim g As Integer
Dim InText As String
Dim WordDelay As Double
Dim NextWord As String
Dim NextSpace As Integer
Dim LineOne As String = ""
Dim LineTwo As String = ""
Dim LineThree As String = ""
Dim LineFour As String = ""
Dim LineFive As String = ""
Dim FileName As String
Dim WPMRate As Double
Dim CountDown As Integer
Dim Entry As String
Dim c As Integer
Dim Result As Long
Dim PrintPage As String = "True"
Dim PrintDiagInfo As String = "No"
Dim Dump As String ' a place to put unwanted keystrokes

/'******************New part added for time correction *****************************'/
Dim StartTime As Double
Dim EndTime As Double
Dim ElapsedSeconds As Double
Dim ElapsedMinutes As Double
Dim WordsRead As Integer = 0
Dim WPMActual As Double
Dim WPMError As Double
Dim StartPause As Double
Dim EndPause As Double
Dim DotCounter As Integer = 0
/'************************************************************************************'/



/'****************New part added for PID time correction *****************************'/
Dim PIDcurrentTime As Double = 0
Dim PIDpreviousTime As Double = 0
Dim dT As Double = 0
Dim PIDerror As Double = 0
Dim PIDintegral As Double = 0
Dim PIDderivative As Double = 0
Dim PIDoutput As Double = 0
Dim PIDpreviousError As Double = 0
Dim Kp As Double = 4
Dim Kd As Double = .125
Dim Ki As Double = 2
Dim CalibrationFlag As String = "True"
/'*************************************************************************************'/



GetFileName:
Cls
Print
Input "Name of text file"; FileName


/'**********************************************
Copy input file to "input.trt"
**********************************************'/
'First get rid of old input.trt if still around
result = FileExists( "input.trt" )
If result <> 0 Then
	Shell("del input.trt")
EndIf
'Copy the desired file to input.trt
'If result = 0 file doesn't exist
result = FileExists( FileName )
If result <> 0 Then
	FileCopy(FileName, "input.trt")
Else
	'Maybe file extension forgotten?
	FileName = FileName & ".txt"
	result = FileExists( FileName )
	If result <> 0 Then
		FileCopy(FileName, "input.trt")
	Else
		Cls
		Print
		Print "File Not Found."
		Print 
		Print "Press any key to continue, or q to quit."
		Sleep
		Entry = LCase(InKey)
		If Entry = "q" Then
			End
		EndIf
		GoTo GetFileName
	EndIf

EndIf

/'*******************************************
 Call "file fix" here to make input file nice
*******************************************'/
FileFix()

f = FreeFile

RestartHere:
Open "temp.trt" For Input As #f
If Err > 0 Then
		Cls
		Print 
		Print "File temp.trt Not Found."
		Sleep
			End
EndIf


RateAgain:
Cls
Print
Input "Desired Words per Minute"; WPMRate
If WPMRate < 5 Or WPMRate > 350 Then
	Cls
	Print
	Print "Really? " + Str(WPMRate) + " words per minutes?"
	Print
	Print "Press any key when sanity returns."
	Sleep
	GoTo RateAgain
EndIf
Cls
WordDelay = 60000 / WPMRate

If CalibrationFlag = "True" Then
	GoTo Calibration
EndIf

GoTo Countdown

CountdownDone:


Cls

/'********** New Timing Section **********'/
StartTime = Timer
/'******* End New Timing Section *********'/

/'************** PID Correction Section **********'/
PIDpreviousTime = StartTime
/'********** End PID Correction Section **********'/

Do While Not Eof(f)
	Line Input #f, InText
	Do While InText <> ""
		'Get rid of any leading space(s)
		InText = LTrim(InText)
		
		'Find where the next space in the line is
		NextSpace = InStr(InText, " ")
		
/'********************************************************************
	To deal with lines that have text but don't end 
		with a space (NextSpace = 0) do the first "then" 
		otherwise do the "else"
********************************************************************'/
		If NextSpace = 0 Then
			' Only end up here when no more words to print
			GoTo NoMoreWords
		Else
			NextWord = Left(InText, NextSpace)
			InText = Mid(InText, NextSpace) 
		EndIf
				
		LineFive = LineFive + NextWord
		
		
		'This makes it necessary to only print line five after every new word.
		'Only when all the lines are shifted up is it necessary to print all five lines.
		If PrintPage <> "True" Then
			Locate 11
			Print "     " + LineFive
			GoTo SkipPage
		EndIf
		
		Cls
		Print
		Print
		Print "     " + LineOne
		Print
		Print "     " + LineTwo
		Print
		Print "     " + LineThree
		Print
		Print "     " + LineFour
		Print
		Print "     " + LineFive
		
		PrintPage = "False"
		
		SkipPage:
		
		If Len(LineFive) > 80 Then
			LineOne = LineTwo
			LineTwo = LineThree
			LineThree = LineFour
			LineFour = LineFive
			LineFive = ""
			PrintPage = "True"
		EndIf
		
		'*********************************************************************
		If PrintDiagInfo = "No" Then
			GoTo SkipDiagInfo
		EndIf
		Print
		Print
		Print "     " + "WPMActual = " + Format(WPMActual, "###.000")
		Print "     " + "WordDelay = " + Format(WordDelay, "###.000")
		Print "     " + "    PIDoutput = " + Format(PIDoutput, "###.000")
		Print "     " + "           dT = " + Format(dT, "###.000")
		Print "     " + "     PIDerror = " + Format(PIDerror, "###.000")
		Print "     " + "  PIDintegral = " + Format(PIDintegral, "###.000")
		Print "     " + "PIDderivative = " + Format(PIDderivative, "###.000")
		SkipDiagInfo:
		'********************************************************************

		
		Sleep WordDelay
		
		/'********** Original Timing Section **********'/
		WordsRead = WordsRead + 1
		ElapsedMinutes = (Timer - StartTime) / 60
		WPMActual = WordsRead / ElapsedMinutes
		/'******* End Original Timing Section *********'/
		

		/'************* PID Correction ******************************'/
		PIDcurrentTime = Timer
		dT = PIDcurrentTime - PIDpreviousTime
		PIDpreviousTime = PIDcurrentTime
		
		PIDerror = (WPMRate - WPMActual)
		PIDintegral = PIDintegral + ( PIDerror * dT )
		PIDderivative = (PIDerror - PIDpreviousError) / dT
		PIDoutput = (Kp * PIDerror) + (Ki * PIDintegral) + (Kd * PIDderivative)

		PIDpreviousError = PIDerror
		WordDelay = 60000 / (WPMRate + PIDoutput)
		/'***********************************************************'/
	
		If InKey <> "" Then
			' To compensate for program not running during pause
			StartPause = Timer
			
			Locate 23
			Print "Press q to quit, r to restart, or any other key to resume."
			Sleep
	
			Entry = LCase(InKey)
			Select Case Entry
				Case "q"
					Close #f					
					result = FileExists( "input.trt" )
					If result <> 0 Then
						Shell("del input.trt")
					EndIf
					
					result = FileExists( "temp.trt" )
					If result <> 0 Then
						Shell("del temp.trt")
					EndIf

					End
				Case "r"
					CalibrationFlag = "True"
					GoTo RestartInit
				Case "i"
					If PrintDiagInfo = "No" Then
						PrintDiagInfo = "Yes" 
					Else 
						PrintDiagInfo = "No"
					EndIf
			End Select

			' To clear message "Press q to quit..." from above
			Locate 23
			Print String(75, " ")
			
			' To compensate for program not running during pause
			EndPause = Timer
			StartTime = StartTime + EndPause - StartPause
			
		EndIf
		
	NoMoreWords:
	Loop
	
Loop


Print
Print
Print "Words Read: " + Str(WordsRead) 
Print "Elapsed Time: " + Format(ElapsedMinutes, "###.000") + " minutes"
Print "WPM = " + Format(WordsRead / ElapsedMinutes, "###.000")
Print
Print "Press r to restart, or any other key to close."
Close #f
Sleep
Entry = LCase(InKey)
If Entry <> "r" Then
	Close #f
	result = FileExists( "input.trt" )
	If result <> 0 Then
		Shell("del input.trt")
	EndIf
	result = FileExists( "temp.trt" )
	If result <> 0 Then
		Shell("del temp.trt")
	EndIf

	End
EndIf



RestartInit:
LineOne = ""
LineTwo = ""
LineThree = ""
LineFour = ""
LineFive = ""
WordsRead = 0

'*********** For PID Reset **********************
PIDcurrentTime = 0
PIDpreviousTime = 0
dT = 0
PIDerror = 0
PIDintegral = 0
PIDderivative = 0
PIDoutput = 0
PIDpreviousError = 0
'********* End for PID Reset *********************

Close #f
f = FreeFile
GoTo RestartHere


CountDown:
' Cool ASCII letters from https://onlineasciitools.com/convert-text-to-ascii-art
Beep()
' Five:

Cls
Print
Print
Print "      " + " ,---.,-..-.   .-.,---."
Print "      " + " | .-'|(| \ \ / / | .-'"
Print "      " + " | `-.(_)  \ V /  | `-."  
Print "      " + " | .-'| |   ) /   | .-'"  
Print "      " + " | |  | |  (_)    |  `--."
Print "      " + " )\|  `-'         /( __.'"
Print "      " + "(__)             (__)     "

Sleep 1000

' Four:

Cls
Print
Print
Print "      " + " ,---. .---.  .-. .-.,---. "
Print "      " + " | .-'/ .-. ) | | | || .-.\ "  
Print "      " + " | `-.| | |(_)| | | || `-'/  " 
Print "      " + " | .-'| | | | | | | ||   (    "
Print "      " + " | |  \ `-' / | `-')|| |\ \   "
Print "      " + " )\|   )---'  `---(_)|_| \)\  "
Print "      " + "(__)  (_)                (__) "

Sleep 1000

' Three:

Cls
Print
Print
Print "      " + " _______ .-. .-.,---.    ,---.  ,---.   "
Print "      " + "|__   __|| | | || .-.\   | .-'  | .-'   "
Print "      " + "  )| |   | `-' || `-'/   | `-.  | `-.   "
Print "      " + " (_) |   | .-. ||   (    | .-'  | .-'   "
Print "      " + "   | |   | | |)|| |\ \   |  `--.|  `--. "
Print "      " + "   `-'   /(  (_)|_| \)\  /( __.'/( __.' "
Print "      " + "        (__)        (__)(__)   (__)     "

Sleep 1000
    
' Two:

Cls
Print
Print
Print "      " + " _______ .-.  .-. .---.   "
Print "      " + "|__   __|| |/\| |/ .-. )  "
Print "      " + "  )| |   | /  \ || | |(_) "
Print "      " + " (_) |   |  /\  || | | |  "
Print "      " + "   | |   |(/  \ |\ `-' /  "
Print "      " + "   `-'   (_)   \| )---'   "
Print "      " + "                 (_)      "

Sleep 1000

    
' One:
Cls
Print
Print
Print "      " + " .---.  .-. .-.,---.   "
Print "      " + "/ .-. ) |  \| || .-'   "
Print "      " + "| | |(_)|   | || `-.   "
Print "      " + "| | | | | |\  || .-'   "
Print "      " + "\ `-' / | | |)||  `--. "
Print "      " + " )---'  /(  (_)/( __.' "
Print "      " + "(_)    (__)   (__)     "

Sleep 1000

GoTo CountDownDone

'**************************************************
'               Calibration
'**************************************************
Calibration:
Cls

/'********** New Timing Section **********'/
StartTime = Timer
/'******* End New Timing Section *********'/

/'************** PID Correction Section **********'/
PIDpreviousTime = StartTime
/'********** End PID Correction Section **********'/

g = FreeFile

Open "calibration.txt" For Input As #g
If Err > 0 Then
		Cls
		Print 
		Print "File calibration.txt not found."
		Print
		Print "Press any key to continue."
		Sleep
		Dump = InKey()
EndIf

Do While Not Eof(g)
	Line Input #g, InText
	Do While InText <> ""
		'Get rid of any leading space(s)
		InText = LTrim(InText)
		
		'Find where the next space in the line is
		NextSpace = InStr(InText, " ")
		
/'********************************************************************
	To deal with lines that have text but don't end 
		with a space (NextSpace = 0) do the first "then" 
		otherwise do the "else"
********************************************************************'/
		If NextSpace = 0 Then
			'GoTo CalibrationDone
			NextWord = InText & " "
			InText = ""
		Else
			NextWord = Left(InText, NextSpace)
			InText = Mid(InText, NextSpace) 
		EndIf
				
		LineFive = LineFive + NextWord
		
		Cls
		Print
		Select Case DotCounter
			Case 0
				Print "     C A L I B R A T I N G "
				DotCounter = 1
			Case 1
				Print "     C A L I B R A T I N G ."
				DotCounter = 2
			Case 2
				Print "     C A L I B R A T I N G . ."
				DotCounter = 3
			Case 3
				Print "     C A L I B R A T I N G . . ."
				DotCounter = 0
		End Select
		
		Print
		Print "     " + LineOne
		Print
		Print "     " + LineTwo
		Print
		Print "     " + LineThree
		Print
		Print "     " + LineFour
		Print
		Print "     " + LineFive
		
		If Len(LineFive) > 80 Then
			LineOne = LineTwo
			LineTwo = LineThree
			LineThree = LineFour
			LineFour = LineFive
			LineFive = ""
		EndIf
			
		Sleep WordDelay
		
		/'********** New Timing Section **********'/
		WordsRead = WordsRead + 1
		ElapsedMinutes = (Timer - StartTime) / 60
		WPMActual = WordsRead / ElapsedMinutes
		/'******* End New Timing Section *********'/
		

		/'************* PID Correction ******************************'/
		PIDcurrentTime = Timer
		dT = PIDcurrentTime - PIDpreviousTime
		PIDpreviousTime = PIDcurrentTime
		
		PIDerror = (WPMRate - WPMActual)
		PIDintegral = PIDintegral + ( PIDerror * dT )
		PIDderivative = (PIDerror - PIDpreviousError) / dT
		PIDoutput = (Kp * PIDerror) + (Ki * PIDintegral) + (Kd * PIDderivative)

		PIDpreviousError = PIDerror
		WordDelay = 60000 / (WPMRate + PIDoutput)
		/'***********************************************************'/
	
	Loop
	
Loop

CalibrationDone:	

Close #g
LineOne = ""
LineTwo = ""
LineThree = ""
LineFour = ""
LineFive = ""
WordsRead = 0
CalibrationFlag = "False"
GoTo CountDown