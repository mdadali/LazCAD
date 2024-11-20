procedure RunDialogsDemo;
var
  fileName: string;
  directory: string;
  selectedFont: string;
  selectedColor: TColor;
  calendarDate: TDateTime;
  calculatorResult: Double;
begin
  // 1. Datei öffnen
  fileName := OpenFileDialog('Wähle eine Datei aus', 'C:\', 'Alle Dateien (*.*)|*.*');
  if fileName <> '' then
    Writeln(fileName)
  else
    Writeln('Keine Datei ausgewählt.');

  // 2. Verzeichnis auswählen
  directory := SelectDirectoryDialog('Verzeichnis wählen', 'C:\');
  if directory <> '' then
    Writeln(directory)
  else
    Writeln('Kein Verzeichnis ausgewählt.');

  // 3. Schriftart auswählen
  selectedFont := FontDialog;
  if selectedFont <> '' then
    Writeln(selectedFont)
  else
    Writeln('Keine Schriftart ausgewählt.');

  // 4. Farbe wählen
  selectedColor := ColorDialog;
  if selectedColor <> clNone then
    Writeln(IntToStr(selectedColor))
  else
    Writeln('Keine Farbe ausgewählt.');

  // 5. Rechner verwenden
  calculatorResult := CalculatorDialog;
  Writeln(FloatToStr(calculatorResult));

  // 6. Datum über den Kalender wählen
  calendarDate := CalendarDialog;
  if calendarDate <> 0 then
    Writeln(DateToStr(calendarDate))
  else
    Writeln('Kein Datum ausgewählt.');
end;

begin
  RunDialogsDemo;
end.

