/' **********************************************************************

	Preprocessor that fixes a text file before running Time-Reader

***********************************************************************'/
Sub FileFix

Dim result As Long
Dim ff As Long
Dim f As Long

Dim InText As String
Dim LeftQuote As String = Chr(226, 128, 156)
Dim RightQuote As String = Chr(226, 128, 157)
Dim Apostrophe As String = Chr(226, 128, 153)
Dim Hyphen As String = Chr(226, 128, 147)
Dim LongHyphen As String = Chr(226, 128, 148)
Dim StandAloneHyphen As String = Chr(32, 45, 32)
Dim LeftOfString As String
Dim RightofString As String
Dim CurrentQuote As Integer


'See if old temp.trt still around
'If result = 0 file doesn't exist
result = FileExists( "temp.trt" )
If result <> 0 Then
	Shell "del temp.trt"
EndIf

ff = FreeFile


' Where to write the fixed text
Open "temp.trt" For Output As #ff

f = FreeFile

Open "input.trt" For Input As #f
If Err>0 Then
		Cls
		Print 
		Print "File input.trt Not Found. Press any key to end."
		Sleep
		Close #ff
		Close #f
		End
EndIf
	

/'*****************************************************************
Here is where the work gets done. 
*****************************************************************'/

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
	
/'****************************************************************************
	To deal with stand-alone hyphens which throw off timing
	because they're treated like an actual word. Replace " - " with "- ".
	That way dashes from trailing off should be handled properly, too.
****************************************************************************'/
	Do While InStr(InText, StandAloneHyphen) <> 0
		LeftOfString = ""
		RightOfString = ""
		CurrentQuote = InStr(Intext, StandAloneHyphen)
		LeftOfString = Left(InText, CurrentQuote - 1)
		RightOfString = Mid(InText, CurrentQuote + 3)
		InText = LeftOfString & Chr(45) & Chr(32) & RightOfString
	Loop


	'This will fix any lines that don't end with a space.
	If Right(InText, 1) <> " " Then
		InText = InText & " "
	EndIf

	' All it took was the semicolon at the end to get rid of the CRLF
	Print #ff, InText;
	
Loop

Close #f
Close #ff

'Now processing done - delete unprocessed copy of input file
'If result = 0 file doesn't exist
result = FileExists( "input.trt" )
If result <> 0 Then
	Shell "del input.trt"
EndIf

End Sub