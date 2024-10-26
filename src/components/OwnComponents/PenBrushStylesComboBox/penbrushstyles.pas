{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PenBrushStyles;

{$warn 5023 off : no warning about unused units}
interface

uses
  fPenStyleComboBox, fBrushStyleComboBox, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fPenStyleComboBox', @fPenStyleComboBox.Register);
  RegisterUnit('fBrushStyleComboBox', @fBrushStyleComboBox.Register);
end;

initialization
  RegisterPackage('PenBrushStyles', @Register);
end.
