unit CADDocument;

//{$mode DELPHI}{$H+}
{$mode objfpc}{$H+}

interface


uses ComCtrls,
     CADSys4,
     CS4BaseTypes,
     CS4Shapes;

var
    CADPrg2D:  TCADPrg2D;
    CADCmp2D:  TCADCmp2D;
    CADViewport2D: TCADViewport2D;
    ProgressBar: TProgressBar;


procedure RegisterDocumet(ACADPrg2D: TCADPrg2D; ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D; AProgressBar: TProgressBar);

implementation

procedure RegisterDocumet(ACADPrg2D: TCADPrg2D; ACADCmp2D: TCADCmp2D; ACADViewport2D: TCADViewport2D; AProgressBar: TProgressBar);
begin
  CADPrg2D      := ACADPrg2D;
  CADCmp2D      := ACADCmp2D;
  CADViewport2D := ACADViewport2D;
  ProgressBar   := AProgressBar;
end;

initialization
  CADPrg2D      := nil;
  CADCmp2D      := nil;
  CADViewport2D := nil;
  ProgressBar   := nil;

finalization

end.
