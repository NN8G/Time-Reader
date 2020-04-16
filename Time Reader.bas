/'*******************************************************************
*******************************************************************'/


/'*******************************************************************
*******************************************************************'/
#include "string.bi"

Dim buffer As String, f As Integer
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

/'*******************************************************************
New part added for time correction ********************************'/
Dim StartTime As Double
Dim EndTime As Double
Dim ElapsedSeconds As Double
Dim ElapsedMinutes As Double
Dim WordsRead As Integer = 0
Dim WPMActual As Double
Dim WPMError As Double
Dim StartPause As Double
Dim EndPause As Double
/'*******************************************************************
*******************************************************************'/

/'**************** For fixing quotation marks *********************'/
Dim LeftQuote As String = Chr(226, 128, 156)
Dim RightQuote As String = Chr(226, 128, 157)
Dim Apostrophe As String = Chr(226, 128, 153)
Dim Hyphen As String = Chr(226, 128, 147)
Dim LongHyphen As String = Chr(226, 128, 148)
Dim LeftOfString As String
Dim RightofString As String
Dim CurrentQuote As Integer
/'*****************************************************************'/

f = FreeFile

GetFileName:
Cls
Print
Input "Name of text file"; FileName

RestartHere:
Open FileName For Input As #f
If Err>0 Then
	' Check to see of ".txt" on end of filename was forgotten
	FileName = FileName & ".txt"
	Open FileName For Input As #f
	If Err>0 Then
		Cls
		Print 
		Print "File Not Found. Press any key, or q to quit."
		Sleep
		Entry = LCase(InKey)
		If Entry = "q" Then
			End
		EndIf
		GoTo GetFileName
	EndIf
	
EndIf

RateAgain:
Cls
Print
Input "Desired Words per Minute"; WPMRate
If WPMRate < 1 Or WPMRate > 300 Then
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

GoTo Countdown

CountdownDone:

Cls

/'********** New Timing Section **********'/
StartTime = Timer
/'******* End New Timing Section *********'/

Do While Not Eof(f)
	Line Input #f, InText
	
	' To get rid of typographic quotation marks
	Do While InStr(InText, LeftQuote) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, LeftQuote)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(34) & RightOfString
	Loop
	
	Do While InStr(InText, RightQuote) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, RightQuote)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(34) & RightOfString
	Loop
	
	'To get rid of Extended ASCII apostrophe
	Do While InStr(InText, Apostrophe) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, Apostrophe)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(39) & RightOfString
	Loop
	
	'To get rid of Extended ASCII hyphen
	Do While InStr(InText, Hyphen) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, Hyphen)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(45) & RightOfString
	Loop
		
			'To get rid of Extended ASCII long hyphen
	Do While InStr(InText, LongHyphen) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, LongHyphen)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(45) & RightOfString
	Loop

	
	Do While InText <> ""
		InText = LTrim(InText)
		NextSpace = InStr(InText, " ")
		'To deal with lines that don't end with a space
		If NextSpace = 0 Then
			NextSpace = Len(InText) + 1
		EndIf
		NextWord = Left(InText, NextSpace)
		InText = Mid(InText, NextSpace) 
		
		

		
		LineFive = LineFive + NextWord
		'To deal with end of paragraph
		If InStr(InText, " ") = 0 Then
			LineFive = LineFive + " "
		EndIf
		
		Cls
		Print '"     " + "WPMActual = " + Format(WPMActual, "###.000")
		Print '"     " + "WordDelay = " + Format(WordDelay, "###.000")
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
		WPMError = WPMActual - WPMRate
		WordDelay = WordDelay + (WordDelay * (WPMError / WPMRate))
		/'******* End New Timing Section *********'/
	
		
		If InKey <> "" Then
			' To compensate for program not running during pause
			StartPause = Timer
			
			Print
			Print "Press q to quit, r to restart, or any other key to resume."
			Sleep
	
			Entry = LCase(InKey)
			Select Case Entry
				Case "q"
					End
				Case "r"
					GoTo RestartInit
			End Select
			
			' To compensate for program not running during pause
			EndPause = Timer
			StartTime = StartTime + EndPause - StartPause
		EndIf
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
	End
EndIf



RestartInit:
LineOne = ""
LineTwo = ""
LineThree = ""
LineFour = ""
LineFive = ""
WordsRead = 0

Close #f
f = FreeFile
GoTo RestartHere


CountDown:
' Cool ASCII letters from https://onlineasciitools.com/convert-text-to-ascii-art

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
