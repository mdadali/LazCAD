Program test;
var filename: string;
begin

 77ImportESSI
 fileName := OpenFileDialog('Please choose a file', 'C:\', 'All-Files (*.*)|*.*');
  if fileName <> '' then
    CAD_ImportESSI(fileName)
  else
    Writeln('No file selected.');

 //ImportDXF
 fileName := OpenFileDialog('Please choose a file', 'C:\', 'All-Files (*.*)|*.*');
  if fileName <> '' then
    CAD_ImportDXF(fileName)
  else
    Writeln('No file selected.');

end.
